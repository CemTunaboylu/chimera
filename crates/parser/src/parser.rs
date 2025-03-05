use crate::{
    event::Event,
    event_holder::EventHolder,
    marker::{Complete, Incomplete, Marker},
};

use lexer::lexer::Lexer;
use syntax::{Syntax, bitset::SyntaxKindBitSet, context::ParserContext, syntax_kind::SyntaxKind};

use miette::Result as MietteResult;
use thin_vec::{ThinVec, thin_vec};

#[derive(Debug)]
pub struct Parser<'input> {
    pub(super) lexer: Lexer<'input>,
    pub event_holder: EventHolder,
    pub context: ParserContext,
    // when looking ahead, we omit whitespaces and errors for a second, we store them here to deal with later on
    pub buffer: ThinVec<Syntax>,
}

use SyntaxKind::*;

impl<'input> Parser<'input> {
    pub fn new(program: &'input str) -> Self {
        // let is by default in recovery set, i.e. it won't be bumped in a recovery case
        let mut context = ParserContext::new();
        context.disallow_recovery_of([KwLet].as_ref());

        Self {
            lexer: Lexer::new(program),
            event_holder: EventHolder::new(),
            context,
            buffer: thin_vec![],
        }
    }

    pub fn peek(&mut self) -> Option<MietteResult<Syntax>> {
        let mut peeked = self.raw_peek()?;
        if let Ok(ref syntax) = peeked {
            if syntax.is_trivia() {
                self.buffer.push(syntax.clone());
                self.lexer.next();
                peeked = self.raw_peek()?
            }
        }
        Some(peeked)
    }

    pub fn clean_buffer(&mut self) {
        self.buffer.iter().for_each(|syntax| {
            self.event_holder.push(Event::AddSyntax {
                syntax: syntax.clone(),
            });
        });
        self.buffer.clear();
    }

    fn raw_peek(&mut self) -> Option<MietteResult<Syntax>> {
        let result = match self.lexer.peek()? {
            Ok(token) => Ok(token.clone().into()),
            Err(err) => Err(err.clone().into()),
        };
        Some(result)
    }

    pub fn is_at_the_end(&mut self) -> bool {
        self.lexer.peek().is_none()
    }
    pub fn is_next(&mut self, expected_kind: SyntaxKind) -> bool {
        let result = if let Some(Ok(token)) = self.peek() {
            let syntax: Syntax = token;
            syntax.is_of_kind(expected_kind)
        } else {
            false
        };
        result
    }

    pub fn is_next_f(&mut self, expect: fn(Syntax) -> bool) -> bool {
        let result = if let Some(Ok(token)) = self.peek() {
            let syntax: Syntax = token;
            expect(syntax)
        } else {
            false
        };
        result
    }
    // expect injects the syntax kind to expectations if not met
    pub fn expect(&mut self, kind: SyntaxKind) -> Option<()> {
        self.is_next(kind).then(|| ()).or_else(|| {
            self.context.expect(kind);
            None
        })
    }
    // expect_and_bump injects the syntax kind to expectations before acting
    pub fn expect_and_bump(&mut self, expected_kind: SyntaxKind) {
        self.context.expect(expected_kind);
        if self.is_next(expected_kind) {
            self.bump();
            return;
        }
        self.recover();
    }

    pub fn expect_f_and_bump(&mut self, expect: fn(Syntax) -> bool) {
        if self.is_next_f(expect) {
            self.bump();
            return;
        }
        self.recover();
    }

    pub fn complete_marker_with(
        &mut self,
        marker: Marker<Incomplete>,
        kind: SyntaxKind,
    ) -> Marker<Complete> {
        let completed = marker.complete();
        self.event_holder
            .update_corresponding_marker_event(completed.get_checkpoint(), kind);
        completed
    }
    pub fn precede_marker_with(&mut self, marker: &Marker<Complete>) -> Marker<Incomplete> {
        let new = self.start();
        let forward_parent_index = marker.forward_parent_index_from(&new);
        self.event_holder
            .add_forward_parent_marker_event(marker.get_checkpoint(), forward_parent_index);
        new
    }

    // expect_and_bump_with_marker injects the syntax kind to expectations before acting
    pub fn expect_and_bump_with_marker(
        &mut self,
        expected_kind: SyntaxKind,
        bump_with: SyntaxKind,
    ) -> Option<Marker<Complete>> {
        self.context.expect(expected_kind);
        if self.is_next(expected_kind) {
            return Some(self.bump_with_marker(bump_with));
        }
        self.recover();
        None
    }

    pub fn bump_with_marker(&mut self, kind: SyntaxKind) -> Marker<Complete> {
        let marker = self.start();
        self.bump_no_buffer_action();
        let finished = self.complete_marker_with(marker, kind);
        self.clean_buffer();
        finished
    }
    pub fn impose_restrictions_of(&mut self, syntax: Syntax) {
        let context_updates = syntax.imposed_restrictions();
        let matching_bitsets = [
            self.context.get_expectations(),
            self.context.get_recovery_set(),
            self.context.get_allowed(),
        ];

        let applied: ThinVec<SyntaxKindBitSet> = matching_bitsets
            .iter()
            .zip(context_updates.iter())
            .map(|(bs, rtype)| rtype.apply(*bs))
            .collect();

        self.context = applied.into();
    }

    pub fn start(&mut self) -> Marker {
        let checkpoint = self.event_holder.checkpoint();
        self.event_holder.push(Event::Marker { checkpoint });
        Marker::new(checkpoint)
    }

    pub fn bump_no_buffer_action(&mut self) -> Option<()> {
        if let Ok(token) = self.lexer.next()? {
            let syntax: Syntax = token.into();
            let kind = syntax.get_kind();
            self.context.del_expectation(kind);
            self.event_holder.push(Event::AddSyntax { syntax: syntax });
        }
        Some(())
    }
    // bump removes the syntax kind from expectations if succesful
    pub fn bump(&mut self) -> Option<()> {
        self.clean_buffer();
        if let Ok(token) = self.lexer.next()? {
            let syntax: Syntax = token.into();
            let kind = syntax.get_kind();
            self.context.del_expectation(kind);
            self.event_holder.push(Event::AddSyntax { syntax: syntax });
        }
        Some(())
    }

    pub fn ignore_if(&mut self, kind: SyntaxKind) {
        if self.is_next(kind) {
            self.lexer.next();
        }
    }

    pub fn ignore(&mut self) {
        self.lexer.next();
    }
}
