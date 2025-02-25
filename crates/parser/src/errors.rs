use miette::{Diagnostic, Report};
use thiserror::Error;

use std::ops::Range;
use syntax::{
    language::{SyntaxNode, SyntaxToken},
    syntax::{Syntax, SyntaxKind},
};

#[derive(Clone, Diagnostic, Debug, PartialEq, Error)]
#[diagnostic()]
#[error("Parsing error")]
pub struct Inner {
    #[source_code]
    pub src: String,
    #[label = "Here"]
    err_span: Range<usize>,
    #[help]
    expected_but_found: String,
}

pub trait HasSpan {
    fn get_span(&self) -> Range<usize>;
}

impl HasSpan for &SyntaxNode {
    fn get_span(&self) -> Range<usize> {
        self.text_range().into()
    }
}

impl HasSpan for &SyntaxToken {
    fn get_span(&self) -> Range<usize> {
        self.text_range().into()
    }
}

pub trait Stringer {
    fn into(self) -> String;
}

impl Stringer for &str {
    fn into(self) -> String {
        self.to_string()
    }
}

impl Stringer for SyntaxKind {
    fn into(self) -> String {
        format!("{:?}", self)
    }
}

impl Stringer for &[SyntaxKind] {
    fn into(self) -> String {
        let expected = self
            .iter()
            .map(|s| <SyntaxKind as Stringer>::into(*s))
            .collect::<Vec<String>>()
            .join(" or ");
        expected
    }
}

impl Stringer for Vec<SyntaxKind> {
    fn into(self) -> String {
        let expected = self
            .iter()
            .map(|s| <SyntaxKind as Stringer>::into(*s))
            .collect::<Vec<String>>()
            .join(" or ");
        expected
    }
}

impl Stringer for Option<SyntaxKind> {
    fn into(self) -> String {
        self.map(|s| format!("{:?}", s)).unwrap_or("''".to_string())
    }
}

impl Stringer for Option<Result<Syntax, Report>> {
    fn into(self) -> String {
        let kind_opt = self.map(|r| {
            r.map(|s| s.kind)
                .expect("expected syntax kind while recovering")
        });
        <Option<SyntaxKind> as Stringer>::into(kind_opt)
    }
}

impl Inner {
    pub fn new(
        src: String,
        err_span: Range<usize>,
        expected_kinds: impl Stringer,
        kind_opt: impl Stringer,
    ) -> Self {
        let expected_but_found = format!(
            "expected {}, but got {}",
            expected_kinds.into(),
            kind_opt.into()
        );
        Self {
            src,
            err_span,
            expected_but_found,
        }
    }
}
#[derive(Clone, Diagnostic, Debug, PartialEq, Error)]
pub enum ParseError {
    #[error(transparent)]
    #[diagnostic(transparent)]
    UnexpectedToken(#[from] Inner),
}

impl ParseError {
    pub fn unexpected_token(
        src: String,
        err_span: Range<usize>,
        expected_kinds: impl Stringer,
        kind_opt: impl Stringer,
    ) -> Self {
        Self::UnexpectedToken(Inner::new(src, err_span, expected_kinds, kind_opt))
    }
}
