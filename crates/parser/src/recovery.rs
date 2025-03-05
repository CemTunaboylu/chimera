use miette::Report;
use syntax::syntax_kind::SyntaxKind;
use thin_vec::ThinVec;

use crate::{
    errors::{ParseError, Stringer},
    event::Event,
    parser::Parser,
};

impl<'input> Parser<'input> {
    fn can_recover(&mut self) -> bool {
        self.peek()
            .is_some_and(|r| r.is_ok_and(|s| self.context.is_recovery_allowed(&s.get_kind())))
    }

    pub fn recover_from_err(&mut self, err: Report) {
        let kinds: ThinVec<SyntaxKind> = self.context.get_expectations().into();
        self.event_holder.push(Event::Error {
            err: ParseError::new(
                self.lexer.span().clone(),
                kinds,
                format!("{:?}", err).as_str(),
            ),
        });
        self.lexer.next();
    }

    pub fn recover_with_msg(&mut self, msg: &str, got: impl Stringer) {
        self.event_holder.push(Event::Error {
            err: ParseError::new(self.lexer.span().clone(), msg, got),
        });
        if self.can_recover() {
            self.lexer.next();
        }
    }

    pub fn recover_unmet_expectation(&mut self) {
        let got = self.peek();
        let kinds: ThinVec<SyntaxKind> = self.context.get_expectations().into();
        self.event_holder.push(Event::Error {
            err: ParseError::new(self.lexer.span().clone(), kinds, got),
        });
        if self.can_recover() {
            self.bump_with_marker(SyntaxKind::Recovered);
        }
        // TODO: add restrictions into the error as well
    }

    pub fn recover(&mut self) {
        let got = self.peek();
        let kinds: ThinVec<SyntaxKind> = self.context.get_expectations().into();
        self.event_holder.push(Event::Error {
            err: ParseError::new(self.lexer.span().clone(), kinds, got),
        });
        if self.can_recover() {
            self.bump_with_marker(SyntaxKind::Recovered);
        }
        // TODO: add restrictions into the error as well
    }

    pub fn recover_restricted(&mut self, restricted: SyntaxKind) {
        let allowed: ThinVec<SyntaxKind> = self.context.get_allowed().into();
        self.event_holder.push(Event::Error {
            err: ParseError::new(self.lexer.span().clone(), allowed, restricted),
        });
        if self.can_recover() {
            self.bump_with_marker(SyntaxKind::Recovered);
        }
        // TODO: add restrictions into the error as well
    }
}
