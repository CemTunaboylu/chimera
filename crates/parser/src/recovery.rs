use miette::Report;
use syntax::syntax_kind::SyntaxKind;
use thin_vec::ThinVec;

use crate::{
    errors::{ParseError, Stringer},
    event::Event,
    parser::Parser,
};

impl Parser<'_> {
    fn can_recover(&self) -> bool {
        self.peek().is_some_and(|r| {
            r.is_err() || r.is_ok_and(|s| self.context.borrow().is_recovery_allowed(s.get_kind()))
        })
    }

    fn move_lexer_to_the_next(&self) {
        if self.can_recover() {
            self.lexer.borrow_mut().next();
        }
    }

    pub fn recover_from_err(&self, err: Report) {
        let err_span = self.lexer.borrow().span().clone();
        let parse_err = ParseError::from(
            err_span,
            format!("{:?}", err).as_str(),
            &self.context.borrow(),
        );
        self.push_event(Event::Error { err: parse_err });
        self.move_lexer_to_the_next();
    }

    pub fn recover_with_msg(&self, msg: &str, got: impl Stringer) {
        let err_span = self.lexer.borrow().span().clone();
        let parse_err = ParseError::from(
            err_span,
            format!("{}, got {}", msg, got.into()).as_str(),
            &self.context.borrow(),
        );
        self.push_event(Event::Error { err: parse_err });
        self.move_lexer_to_the_next();
    }

    pub fn recover(&self) {
        let err_span = self.lexer.borrow().span().clone();
        let parse_err = ParseError::from(err_span, self.peek(), &self.context.borrow());
        self.push_event(Event::Error { err: parse_err });

        if self.can_recover() {
            self.bump_with_marker(SyntaxKind::Recovered);
        }
    }

    pub fn recover_restricted(&self, restricted: SyntaxKind) -> Option<()> {
        let allowed: ThinVec<SyntaxKind> = self.context.borrow().get_allowed().into();
        self.push_event(Event::Error {
            err: ParseError::new(self.lexer.borrow().span().clone(), allowed, restricted),
        });
        if self.can_recover() {
            self.bump_with_marker(SyntaxKind::Recovered);
        }
        None
    }
}
