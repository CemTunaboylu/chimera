use miette::{Diagnostic, Report};
use thin_vec::ThinVec;
use thiserror::Error;

use std::ops::Range;
use syntax::{Syntax, context::ParserContext, syntax_kind::SyntaxKind};

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
        if let Some(result) = self {
            match result {
                Ok(syntax) => <SyntaxKind as Stringer>::into(syntax.get_kind()),
                Err(report) => {
                    format!("{:?}", report)
                }
            }
        } else {
            "".to_string()
        }
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
    pub fn during(
        err_span: Range<usize>,
        expected_kinds: impl Stringer,
        kind_opt: impl Stringer,
        during: impl Stringer,
    ) -> Self {
        let expected_but_found = format!(
            "expected {}, but got {} during {}",
            expected_kinds.into(),
            kind_opt.into(),
            during.into()
        );
        Self {
            err_span,
            expected_but_found,
        }
    }
    pub fn from(err_span: Range<usize>, got: impl Stringer, ctx: &ParserContext) -> Self {
        let kinds: ThinVec<SyntaxKind> = if ctx.get_allowed().has_any() {
            ctx.get_allowed().into()
        } else {
            let expectations = ctx.get_expectations();
            ctx.del_expectation(expectations);
            expectations.into()
        };

        let during = ctx.get_in_the_middle_of();
        if during.has_any() {
            let during_kinds: ThinVec<SyntaxKind> = during.into();
            Self::during(err_span, kinds, got, during_kinds)
        } else {
            Self::new(err_span, kinds, got)
        }
    }
}
