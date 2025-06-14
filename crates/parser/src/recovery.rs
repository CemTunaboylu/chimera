use miette::Report;
use syntax::{bitset::SyntaxKindBitSet, syntax_kind::SyntaxKind};
use thin_vec::ThinVec;

use crate::{
    errors::{ParseError, Stringer},
    event::Event,
    parser::{IsNext, Parser},
};

impl Parser<'_> {
    fn can_recover(&self) -> bool {
        self.peek().is_some_and(|r| {
            r.is_err() || r.is_ok_and(|s| self.context.borrow().is_recovery_allowed(s.get_kind()))
        })
    }

    fn can_recover_kind(&self, kind: SyntaxKind) -> bool {
        self.context.borrow().is_recovery_allowed(kind)
    }

    fn move_lexer_to_the_next(&self) {
        if self.can_recover() {
            self.lexer.borrow_mut().next();
        }
    }

    pub fn emit_error_event(&self, err_msg: &str) {
        let err_span = self.lexer.borrow().span().clone();
        let parse_err = ParseError::from(err_span, err_msg, &self.context.borrow());
        self.push_event(Event::Error { err: parse_err });
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

    pub fn recover_until(&self, until: impl Into<SyntaxKindBitSet>) {
        let set: SyntaxKindBitSet = until.into();
        while IsNext::No == self.is_next_in_strict(set) {
            self.recover();
        }
    }

    pub fn recover_restricted(&self, restricted: SyntaxKind) -> Option<()> {
        let recover = self.can_recover_kind(restricted);
        if !recover {
            return None;
        }
        let allowed: ThinVec<SyntaxKind> = self.context.borrow().get_allowed().into();
        self.push_event(Event::Error {
            err: ParseError::new(self.lexer.borrow().span().clone(), allowed, restricted),
        });
        if recover {
            self.bump_with_marker(SyntaxKind::Recovered);
        }
        None
    }
}
