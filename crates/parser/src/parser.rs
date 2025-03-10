use crate::{
    event::Event,
    event_holder::EventHolder,
    marker::{Complete, Incomplete, Marker},
};
use std::cell::RefCell;

use lexer::lexer::Lexer;
use syntax::{
    Syntax, anchor::RollingBackAnchor, bitset::SyntaxKindBitSet, context::ParserContext,
    syntax_kind::SyntaxKind,
};

use miette::Result as MietteResult;
use thin_vec::ThinVec;

#[derive(Debug)]
pub struct Parser<'input> {
    pub(super) lexer: RefCell<Lexer<'input>>,
    pub event_holder: RefCell<EventHolder>,
    pub context: RefCell<ParserContext>,
    // when looking ahead, we omit whitespaces and errors for a second, we store them here to deal with later on
    pub buffer: RefCell<Option<Syntax>>,
}

use SyntaxKind::*;

impl<'input> Parser<'input> {
    pub fn new(program: &'input str) -> Self {
        // let is by default in recovery set, i.e. it won't be bumped in a recovery case
        let context = ParserContext::new();
        context.disallow_recovery_of([KwLet].as_ref());
        context.forbid_only([RParen, RBrace, RBrack].as_ref());

        Self {
            lexer: RefCell::new(Lexer::new(program)),
            event_holder: RefCell::new(EventHolder::new()),
            context: RefCell::new(context),
            buffer: RefCell::new(None),
        }
    }

    pub fn start(&self) -> Marker {
        let checkpoint = self.event_holder.borrow_mut().push_marker_event();
        Marker::new(checkpoint)
    }

    pub fn complete_marker_with(
        &self,
        marker: Marker<Incomplete>,
        kind: SyntaxKind,
    ) -> Marker<Complete> {
        let completed = marker.complete();
        self.event_holder
            .borrow_mut()
            .update_corresponding_marker_event(completed.get_checkpoint(), kind);
        completed
    }
    pub fn precede_marker_with(&self, marker: &Marker<Complete>) -> Marker<Incomplete> {
        let new = self.start();
        let forward_parent_index = marker.forward_parent_index_from(&new);
        self.event_holder
            .borrow_mut()
            .add_forward_parent_marker_event(marker.get_checkpoint(), forward_parent_index);
        new
    }

    pub fn push_event(&self, event: Event) {
        self.event_holder.borrow_mut().push(event);
    }

    pub fn clean_buffer(&self) {
        self.buffer.take().iter().for_each(|syntax| {
            self.push_event(Event::AddSyntax {
                syntax: syntax.clone(),
            });
        });
    }

    pub fn roll_back_context_after_drop(&self) -> RollingBackAnchor {
        let ctx = self.context.as_ptr();
        let anchor = ParserContext::rolling_back_anchor(ctx);
        anchor
    }

    pub fn impose_restrictions_of(&self, syntax: Syntax) {
        let context_updates = syntax.imposed_restrictions();
        let matching_bitsets = {
            let ctx = self.context.borrow();
            let e = ctx.get_expectations();
            let rc = ctx.get_recovery_set();
            let a = ctx.get_allowed();

            [e, rc, a]
        };

        let applied: ThinVec<SyntaxKindBitSet> = matching_bitsets
            .into_iter()
            .zip(context_updates.iter())
            .map(|(bs, rtype)| rtype.apply(bs))
            .collect();

        self.context.borrow().take(applied.as_ref().into());
    }

    pub fn peek(&self) -> Option<MietteResult<Syntax>> {
        let mut peeked = self.raw_peek()?;
        if let Ok(ref syntax) = peeked {
            if syntax.is_trivia() {
                // if already seen before, take it
                if self.buffer.borrow().is_some() {
                    self.clean_buffer();
                }
                *self.buffer.borrow_mut() = Some(syntax.clone());
                self.lexer.borrow_mut().next();
                peeked = self.raw_peek()?
            }
        }
        Some(peeked)
    }

    fn raw_peek(&self) -> Option<MietteResult<Syntax>> {
        let result = match self.lexer.borrow_mut().peek()? {
            Ok(token) => Ok(token.clone().into()),
            Err(err) => Err(err.clone().into()),
        };
        Some(result)
    }

    pub fn is_at_the_end(&self) -> bool {
        self.lexer.borrow_mut().peek().is_none()
    }

    pub fn is_next(&self, expected_kind: SyntaxKind) -> bool {
        let result = if let Some(Ok(token)) = self.peek() {
            let syntax: Syntax = token;
            syntax.is_of_kind(expected_kind)
        } else {
            false
        };
        result
    }

    pub fn is_next_f(&self, expect: fn(Syntax) -> bool) -> bool {
        let result = if let Some(Ok(token)) = self.peek() {
            let syntax: Syntax = token;
            expect(syntax)
        } else {
            false
        };
        result
    }
    // expect injects the syntax kind to expectations if not met
    pub fn expect(&self, kind: SyntaxKind) -> Option<()> {
        self.is_next(kind).then(|| ()).or_else(|| {
            self.context.borrow().expect(kind);
            None
        })
    }
    // expect_and_bump injects the syntax kind to expectations before acting
    pub fn expect_and_bump(&self, expected_kind: SyntaxKind) {
        self.context.borrow().expect(expected_kind);
        if self.is_next(expected_kind) {
            self.bump();
            return;
        }
        self.recover();
    }

    pub fn expect_f_and_bump(&self, expect: fn(Syntax) -> bool) {
        if self.is_next_f(expect) {
            self.bump();
            return;
        }
        self.recover();
    }

    // expect_and_bump_with_marker injects the syntax kind to expectations before acting
    pub fn expect_and_bump_with_marker(
        &self,
        expected_kind: SyntaxKind,
        bump_with: SyntaxKind,
    ) -> Option<Marker<Complete>> {
        self.context.borrow().expect(expected_kind);
        if self.is_next(expected_kind) {
            return Some(self.bump_with_marker(bump_with));
        }
        self.recover();
        None
    }

    pub fn bump_with_marker(&self, kind: SyntaxKind) -> Marker<Complete> {
        let marker = self.start();
        self.bump_no_buffer_action();
        let finished = self.complete_marker_with(marker, kind);
        self.clean_buffer();
        finished
    }

    pub fn bump_no_buffer_action(&self) -> Option<()> {
        if let Ok(token) = self.lexer.borrow_mut().next()? {
            let syntax: Syntax = token.into();
            let kind = syntax.get_kind();
            self.context.borrow().del_expectation(kind);
            self.push_event(Event::AddSyntax { syntax: syntax });
        }
        Some(())
    }
    // bump removes the syntax kind from expectations if succesful
    pub fn bump(&self) -> Option<()> {
        self.clean_buffer();
        if let Ok(token) = self.lexer.borrow_mut().next()? {
            let syntax: Syntax = token.into();
            let kind = syntax.get_kind();
            self.context.borrow().del_expectation(kind);
            self.push_event(Event::AddSyntax { syntax: syntax });
        }
        Some(())
    }

    pub fn ignore_if(&self, kind: SyntaxKind) {
        if self.is_next(kind) {
            self.ignore();
        }
    }

    pub fn ignore(&self) {
        self.lexer.borrow_mut().next();
    }
}
