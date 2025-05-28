use miette::Report;
use syntax::syntax_kind::SyntaxKind;
use thin_vec::{ThinVec, thin_vec};

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

    // an unmet expectation is meaningful at the first incident, after that it stops being an expectation
    fn take_expectation(&self) -> ThinVec<SyntaxKind> {
        let expectations = self.context.borrow().get_expectations();
        self.context.borrow().del_expectation(expectations);
        expectations.into()
    }

    pub fn recover_from_err(&self, err: Report) {
        let kinds: ThinVec<SyntaxKind> = self.take_expectation();
        let was_in_the_middle_of: ThinVec<SyntaxKind> =
            self.context.borrow().get_in_the_middle_of().into();
        let msg = if let Some(kind) = was_in_the_middle_of.first() {
            format!("{:?} while parsing {:?}", err, kind)
        } else {
            format!("{:?}", err)
        };
        self.push_event(Event::Error {
            err: ParseError::new(self.lexer.borrow().span().clone(), kinds, msg.as_str()),
        });
        if self.can_recover() {
            self.lexer.borrow_mut().next();
        }
    }

    pub fn recover_with_msg(&self, msg: &str, got: impl Stringer) {
        self.push_event(Event::Error {
            err: ParseError::new(self.lexer.borrow().span().clone(), msg, got),
        });
        if self.can_recover() {
            self.lexer.borrow_mut().next();
        }
    }

    pub fn recover(&self) {
        let exps = if let Some(Ok(_)) = self.peek() {
            let ctx = self.context.borrow();
            let kinds: ThinVec<SyntaxKind> = if ctx.get_allowed().has_any() {
                ctx.get_allowed().into()
            } else {
                self.take_expectation()
            };
            kinds
        } else {
            thin_vec![]
        };

        let err_span = self.lexer.borrow().span().clone();

        if let Some(during) = self.get_in_the_middle_of_parsing() {
            self.push_event(Event::Error {
                err: ParseError::during(err_span, exps, self.peek(), during),
            });
        } else {
            self.push_event(Event::Error {
                err: ParseError::new(err_span, exps, self.peek()),
            });
        }

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
