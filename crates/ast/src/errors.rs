use miette::Diagnostic;
use thiserror::Error;

use std::{fmt::Debug, ops::Range};

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

impl ASTError {
    pub fn new(err_span: Range<usize>, expected: impl Debug, got_opt: impl Debug) -> Self {
        let expected_but_found = format!("expected {:?}, but got {:?}", expected, got_opt);
        Self {
            err_span,
            expected_but_found,
        }
    }
    pub fn with_err_msg(err_span: Range<usize>, msg: String) -> Self {
        Self {
            err_span,
            expected_but_found: msg,
        }
    }
}
