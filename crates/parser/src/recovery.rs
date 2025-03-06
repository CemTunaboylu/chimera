use miette::Report;
use syntax::syntax_kind::SyntaxKind;
use thin_vec::ThinVec;

use crate::{
    errors::{ParseError, Stringer},
    event::Event,
    parser::Parser,
};

impl<'input> Parser<'input> {
    fn can_recover(&self) -> bool {
        self.peek().is_some_and(|r| {
            r.is_err() || r.is_ok_and(|s| self.context.borrow().is_recovery_allowed(&s.get_kind()))
        })
    }

    pub fn recover_from_err(&self, err: Report) {
        let kinds: ThinVec<SyntaxKind> = self.context.borrow().get_expectations().into();
        self.event_holder.borrow_mut().push(Event::Error {
            err: ParseError::new(
                self.lexer.borrow().span().clone(),
                kinds,
                format!("{:?}", err).as_str(),
            ),
        });
        if self.can_recover() {
            self.lexer.borrow_mut().next();
        }
    }

    pub fn recover_with_msg(&self, msg: &str, got: impl Stringer) {
        self.event_holder.borrow_mut().push(Event::Error {
            err: ParseError::new(self.lexer.borrow().span().clone(), msg, got),
        });
        if self.can_recover() {
            self.lexer.borrow_mut().next();
        }
    }

    pub fn recover_unmet_expectation(&self) {
        let got = self.peek();
        let kinds: ThinVec<SyntaxKind> = self.context.borrow().get_expectations().into();
        self.event_holder.borrow_mut().push(Event::Error {
            err: ParseError::new(self.lexer.borrow_mut().span().clone(), kinds, got),
        });
        if self.can_recover() {
            self.bump_with_marker(SyntaxKind::Recovered);
        }
        // TODO: add restrictions into the error as well
    }

    pub fn recover(&self) {
        let got = self.peek();
        let kinds: ThinVec<SyntaxKind> = self.context.borrow().get_expectations().into();
        self.event_holder.borrow_mut().push(Event::Error {
            err: ParseError::new(self.lexer.borrow().span().clone(), kinds, got),
        });
        if self.can_recover() {
            self.bump_with_marker(SyntaxKind::Recovered);
        }
        // TODO: add restrictions into the error as well
    }

    pub fn recover_restricted(&self, restricted: SyntaxKind) {
        let allowed: ThinVec<SyntaxKind> = self.context.borrow().get_allowed().into();
        self.event_holder.borrow_mut().push(Event::Error {
            err: ParseError::new(self.lexer.borrow().span().clone(), allowed, restricted),
        });
        if self.can_recover() {
            self.bump_with_marker(SyntaxKind::Recovered);
        }
        // TODO: add restrictions into the error as well
    }
}
