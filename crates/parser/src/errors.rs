use miette::{Diagnostic, Report};
use thin_vec::ThinVec;
use thiserror::Error;

use std::ops::Range;
use syntax::{Syntax, syntax_kind::SyntaxKind};

#[derive(Clone, Diagnostic, Debug, PartialEq, Error)]
#[diagnostic()]
#[error("Parsing error")]
// Source will be injected at the end by the caller
pub struct ParseError {
    #[label = "Here"]
    err_span: Range<usize>,
    #[help]
    expected_but_found: String,
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

impl Stringer for ThinVec<SyntaxKind> {
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
            r.map(|s| s.get_kind())
                .expect("expected syntax kind while recovering")
        });
        <Option<SyntaxKind> as Stringer>::into(kind_opt)
    }
}

impl ParseError {
    pub fn new(
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
            err_span,
            expected_but_found,
        }
    }
}
