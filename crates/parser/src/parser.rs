use crate::{
    event::Event,
    event_holder::EventHolder,
    marker::{Complete, Incomplete, Marker},
    parse::CustomExpectationOnSyntax,
};
use std::cell::RefCell;

use lexer::lexer::Lexer;
use syntax::{
    RestrictionType, Syntax, anchor::RollingBackAnchor, bitset::SyntaxKindBitSet,
    context::ParserContext, syntax_kind::SyntaxKind,
};

use miette::Result as MietteResult;
use thin_vec::ThinVec;

#[derive(Debug, PartialEq)]
pub enum IsNext {
    Yes,
    No,
    None,
}

#[derive(Debug)]
pub struct Parser<'input> {
    pub(super) lexer: RefCell<Lexer<'input>>,
    pub event_holder: RefCell<EventHolder>,
    pub context: RefCell<ParserContext>,
}

use SyntaxKind::*;

fn default_context() -> ParserContext {
    let context = ParserContext::new();
    context.disallow_recovery_of([KwLet].as_ref());
    context.forbid_only([RParen, RBrace, RBrack].as_ref());
    context.allow([StructLit].as_ref());
    context
}

impl<'input> Parser<'input> {
    pub fn new(program: &'input str) -> Self {
        // let is by default in recovery set, i.e. it won't be bumped in a recovery case
        let context = default_context();

        Self {
            lexer: RefCell::new(Lexer::new(program)),
            event_holder: RefCell::new(EventHolder::new()),
            context: RefCell::new(context),
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

    pub fn roll_back_context_after_drop(&self) -> RollingBackAnchor {
        let ctx = self.context.as_ptr();
        let anchor = unsafe { ParserContext::rolling_back_anchor(ctx) };
        anchor
    }

    pub fn forbid_all(&self) -> RollingBackAnchor {
        let anchor = self.roll_back_context_after_drop();
        self.context.borrow().forbid_all();
        anchor
    }

    fn rollback_anchor_with_new_restrictions(
        &self,
        context_updates: [RestrictionType; 4],
    ) -> RollingBackAnchor {
        let rollback_when_dropped = self.roll_back_context_after_drop();
        let matching_bitsets = {
            let ctx = self.context.borrow();
            let e = ctx.get_expectations();
            let rc = ctx.get_recovery_set();
            let a = ctx.get_allowed();
            let im = ctx.get_in_the_middle_of();

            [e, rc, a, im]
        };
        let applied: ThinVec<SyntaxKindBitSet> = matching_bitsets
            .into_iter()
            .zip(context_updates.iter())
            .map(|(bs, rtype)| rtype.apply(bs))
            .collect();

        let new: ParserContext = applied.as_ref().into();
        // new.set_in_the_middle_of(self.context.borrow().get_in_the_middle_of());
        self.context.borrow().take(new);
        rollback_when_dropped
    }

    pub fn impose_restrictions_on_context(&self, syntax: Syntax) -> RollingBackAnchor {
        let context_updates = syntax.imposed_restrictions();
        self.rollback_anchor_with_new_restrictions(context_updates)
    }

    pub fn impose_context_for_parsing(&self, kind: SyntaxKind) -> RollingBackAnchor {
        let context_updates = kind.imposed_restrictions();
        let rollback_anchor = self.rollback_anchor_with_new_restrictions(context_updates);
        self.context.borrow().set_in_the_middle_of(kind);
        rollback_anchor
    }

    pub fn impose_restrictions_of_kind_on_context(&self, kind: SyntaxKind) -> RollingBackAnchor {
        let context_updates = kind.imposed_restrictions();
        self.rollback_anchor_with_new_restrictions(context_updates)
    }

    pub fn parsing(&self, kind: SyntaxKind) -> RollingBackAnchor {
        let rollback_anchor = self.roll_back_context_after_drop();
        self.context.borrow().set_in_the_middle_of(kind);
        rollback_anchor
    }

    pub fn expect_in_ctx(&self, e: impl Into<SyntaxKindBitSet>) {
        self.context.borrow().expect(e);
    }
    pub fn mark_expectation_as_satisfied_in_ctx(&self, e: impl Into<SyntaxKindBitSet>) {
        self.context.borrow().del_expectation(e);
    }

    pub fn allow_in_ctx(&self, e: impl Into<SyntaxKindBitSet>) {
        self.context.borrow().allow(e);
    }

    pub fn only_allow_in_ctx(&self, e: impl Into<SyntaxKindBitSet>) {
        self.context.borrow().allow_only(e);
    }

    pub fn forbid_in_ctx(&self, e: impl Into<SyntaxKindBitSet>) {
        self.context.borrow().forbid(e);
    }

    pub fn dont_recover_in_ctx(&self, e: impl Into<SyntaxKindBitSet>) {
        self.context.borrow().disallow_recovery_of(e);
    }

    pub fn allow_all_in_ctx(&self) {
        self.context.borrow().allow_all();
    }

    pub fn allow_only_in_ctx(&self, e: impl Into<SyntaxKindBitSet>) {
        self.context.borrow().allow_only(e);
    }

    pub fn is_expected(&self, e: impl Into<SyntaxKindBitSet>) -> bool {
        self.context.borrow().is_expected(e)
    }

    pub fn get_in_the_middle_of_parsing(&self) -> Option<SyntaxKind> {
        let tv: ThinVec<SyntaxKind> = self.context.borrow().get_in_the_middle_of().into();
        tv.first().cloned()
    }

    pub fn is_in_the_middle_of_parsing(&self, kind: SyntaxKind) -> bool {
        self.context.borrow().is_in_the_middle_of(kind)
    }

    pub fn is_allowed(&self, e: impl Into<SyntaxKindBitSet>) -> bool {
        self.context.borrow().is_allowed(e)
    }

    pub fn peek(&self) -> Option<MietteResult<Syntax>> {
        let mut peeked = self.raw_peek()?;
        while let Ok(ref syntax) = peeked {
            if syntax.is_trivia() {
                self.bump();
                peeked = self.raw_peek()?;
                continue;
            }
            break;
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

    pub fn is_next_strict(&self, expected_kind: SyntaxKind) -> IsNext {
        if let Some(Ok(token)) = self.peek() {
            let syntax: Syntax = token;
            match syntax.is_of_kind(expected_kind) {
                true => IsNext::Yes,
                false => IsNext::No,
            }
        } else {
            IsNext::None
        }
    }

    pub fn is_next_in_strict(&self, set: SyntaxKindBitSet) -> IsNext {
        if let Some(Ok(token)) = self.peek() {
            let syntax: Syntax = token;
            match set.contains(syntax.get_kind()) {
                true => IsNext::Yes,
                false => IsNext::No,
            }
        } else {
            IsNext::None
        }
    }

    pub fn is_next(&self, expected_kind: SyntaxKind) -> bool {
        if let Some(Ok(token)) = self.peek() {
            let syntax: Syntax = token;
            syntax.is_of_kind(expected_kind)
        } else {
            false
        }
    }

    pub fn is_next_in(&self, set: SyntaxKindBitSet) -> bool {
        if let Some(Ok(token)) = self.peek() {
            let syntax: Syntax = token;
            set.contains(syntax.get_kind())
        } else {
            false
        }
    }

    pub fn is_next_f(&self, expect: CustomExpectationOnSyntax) -> bool {
        if let Some(Ok(token)) = self.peek() {
            let syntax: Syntax = token;
            expect(&syntax)
        } else {
            false
        }
    }

    pub fn is_next_f_strict(&self, expect: CustomExpectationOnSyntax) -> IsNext {
        if let Some(Ok(token)) = self.peek() {
            let syntax: Syntax = token;
            match expect(&syntax) {
                true => IsNext::Yes,
                false => IsNext::No,
            }
        } else {
            IsNext::None
        }
    }
    // expect injects the syntax kind to expectations if not met
    pub fn expect_next(&self, kind: SyntaxKind) -> Option<()> {
        matches!(self.is_next_strict(kind), IsNext::Yes)
            .then(|| ())
            .or_else(|| {
                self.expect_in_ctx(kind);
                None
            })
    }
    // expect_and_bump injects the syntax kind to expectations before acting
    pub fn expect_and_bump(&self, expected_kind: SyntaxKind) {
        self.expect_in_ctx(expected_kind);
        if IsNext::Yes == self.is_next_strict(expected_kind) {
            self.bump();
            return;
        }
        self.recover();
    }

    // expect_and_bump injects the syntax kind to expectations before acting
    pub fn expect_in_set_and_bump(&self, set: SyntaxKindBitSet) {
        self.expect_in_ctx(set);
        if IsNext::Yes == self.is_next_in_strict(set) {
            self.bump();
            return;
        }
        self.recover();
    }

    pub fn expect_and_bump_as(&self, expected_kind: SyntaxKind, kind: SyntaxKind) {
        self.expect_in_ctx(expected_kind);
        if IsNext::Yes == self.is_next_strict(expected_kind) {
            self.bump_as(kind);
            return;
        }
        self.recover();
    }

    pub fn expect_f_and_bump(&self, expect: CustomExpectationOnSyntax) {
        if IsNext::Yes == self.is_next_f_strict(expect) {
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
        self.expect_in_ctx(expected_kind);
        if IsNext::Yes == self.is_next_strict(expected_kind) {
            return Some(self.bump_with_marker(bump_with));
        }
        self.recover();
        None
    }

    pub fn bump_with_marker(&self, kind: SyntaxKind) -> Marker<Complete> {
        let marker = self.start();
        self.bump();
        self.complete_marker_with(marker, kind)
    }

    // bump removes the syntax kind from expectations if succesful
    pub fn bump(&self) -> Option<()> {
        if let Ok(token) = self.lexer.borrow_mut().next()? {
            let syntax: Syntax = token.into();
            let kind = syntax.get_kind();
            self.mark_expectation_as_satisfied_in_ctx(kind);
            self.push_event(Event::AddSyntax { syntax });
        }
        Some(())
    }
    pub fn bump_as(&self, kind: SyntaxKind) -> Option<()> {
        if let Ok(token) = self.lexer.borrow_mut().next()? {
            let mut syntax: Syntax = token.into();
            let s_kind = syntax.get_kind();
            self.mark_expectation_as_satisfied_in_ctx(s_kind);
            syntax.set_kind(kind);
            self.push_event(Event::AddSyntax { syntax });
        }
        Some(())
    }
    pub fn bump_if(&self, kind: SyntaxKind) {
        if self.is_next(kind) {
            self.bump();
        }
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
