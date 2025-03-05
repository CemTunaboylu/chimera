use miette::Diagnostic;
use thiserror::Error;

use std::ops::Range;
use syntax::{
    language::{SyntaxNode, SyntaxToken},
    syntax_kind::SyntaxKind,
};

#[derive(Clone, Diagnostic, Debug, PartialEq, Error)]
#[diagnostic()]
#[error("ASTError")]
// Source will be injected at the end by the caller
pub struct ASTError {
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

impl Stringer for &SyntaxNode {
    fn into(self) -> String {
        format!("{:?}", self)
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

impl ASTError {
    pub fn new(err_span: Range<usize>, expected: impl Stringer, got_opt: impl Stringer) -> Self {
        let expected_but_found =
            format!("expected {}, but got {}", expected.into(), got_opt.into());
        Self {
            err_span,
            expected_but_found,
        }
    }
}
