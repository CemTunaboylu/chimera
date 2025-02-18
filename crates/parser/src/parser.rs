use std::mem;

use crate::{
    cst::ConcreteSyntaxTree,
    errors::ParseError,
    event::Event,
    event_holder::EventHolder,
    marker::{Complete, Marker},
    parse_behaviors::ASTBehavior,
    s_expression::pratt_parser,
    sink::Sink,
};

use lexer::lexer::Lexer;
use syntax::syntax::{Syntax, SyntaxKind};

use miette::Result as MietteResult;

pub struct Parser<'input> {
    lexer: Lexer<'input>,
    pub event_holder: EventHolder,
    program: &'input str,
    expected: Vec<SyntaxKind>,
}

const RECOVERY_SET: [SyntaxKind; 1] = [SyntaxKind::LetKw];

impl<'input> Parser<'input> {
    pub fn new(program: &'input str) -> Self {
        Self {
            lexer: Lexer::new(program),
            event_holder: EventHolder::new(),
            program,
            expected: vec![],
        }
    }
    pub fn parse<B: ASTBehavior>(mut self) -> ConcreteSyntaxTree {
        pratt_parser::<B>(&mut self);
        let sink = Sink::new(self.event_holder.into(), self.program);
        ConcreteSyntaxTree::from(sink)
    }

    pub fn peek<B: ASTBehavior>(&mut self) -> Option<MietteResult<Syntax>> {
        B::apply(&mut self.lexer, &mut self.event_holder);
        self._peek()
    }

    fn _peek(&mut self) -> Option<MietteResult<Syntax>> {
        let result = match self.lexer.peek()? {
            Ok(token) => Ok(token.clone().into()),
            Err(err) => Err(err.clone().into()),
        };
        Some(result)
    }

    pub fn is_at_the_end(&mut self) -> bool {
        self.lexer.peek().is_none()
    }

    pub fn bump(&mut self) -> Option<()> {
        if let Ok(token) = self.lexer.next()? {
            self.event_holder.push(Event::AddSyntax {
                syntax: token.into(),
            });
            self.expected.clear();
        }
        Some(())
    }

    pub fn inject_expectations(&mut self, expectations: &[SyntaxKind]) {
        // if !self.expected.ends_with(expectations) {
        self.expected = Vec::from(expectations);
        // self.expected.extend_from_slice(expectations);
        // }
    }

    pub fn is_next(&mut self, expected_kind: SyntaxKind) -> bool {
        self.expected.push(expected_kind);
        if let Some(Ok(token)) = self._peek() {
            let syntax: Syntax = token.into();
            syntax.is_of_kind(expected_kind)
        } else {
            false
        }
    }

    pub fn check_next_syntax(&mut self, check: fn(&Syntax) -> bool) -> bool {
        if let Some(Ok(syntax)) = self._peek() {
            check(&syntax)
        } else {
            false
        }
    }

    pub fn expect_and_bump<B: ASTBehavior>(&mut self, expected_kind: SyntaxKind) {
        B::apply(&mut self.lexer, &mut self.event_holder);
        if self.is_next(expected_kind) {
            self.bump();
            return;
        }

        self.recover();
    }

    fn can_recover(&mut self) -> bool {
        self._peek()
            .is_some_and(|r| r.is_ok_and(|s| !RECOVERY_SET.contains(&s.kind)))
    }

    pub fn recover(&mut self) {
        let got = self._peek();
        self.event_holder.push(Event::Error {
            err: ParseError::unexpected_token(
                self.program.to_string(),
                self.lexer.span().clone(),
                mem::take(&mut self.expected),
                got,
            ),
        });
        if self.can_recover() {
            self.bump_with_marker(SyntaxKind::Recovered);
        }
    }

    pub fn bump_iff_or_panic(&mut self, expectation: fn(&Syntax) -> bool) {
        assert!(self.check_next_syntax(expectation));
        self.bump();
    }

    pub fn bump_with_marker(&mut self, kind: SyntaxKind) -> Marker<Complete> {
        let marker = self.start();
        self.bump();
        marker.complete(&mut self.event_holder, kind)
    }
    pub fn start(&mut self) -> Marker {
        let checkpoint = self.event_holder.checkpoint();
        self.event_holder.push(Event::Marker { checkpoint });
        Marker::new(checkpoint)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ops::Range;

    fn check_err(
        input: &str,
        expected: Vec<SyntaxKind>,
        found: Option<SyntaxKind>,
        range: Range<usize>,
        output: &str,
    ) {
        let error = ParseError::unexpected_token(input.to_string(), range, expected, found);
        assert_eq!(format!("{:?}", error), output);
    }

    #[test]
    fn one_expected_did_find() {
        check_err(
            "let a = ",
            vec![SyntaxKind::Literal],
            None,
            8..9,
            "UnexpectedToken(Inner { src: \"let a = \", err_span: 8..9, expected_but_found: \"expected Literal, but got ''\" })",
        );
    }
}
