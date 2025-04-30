use std::ops::Range;

use miette::Diagnostic;
use thiserror::Error;

#[derive(Clone, Default, Debug, Diagnostic, Error, PartialEq)]
#[error("LexError")]
#[diagnostic()]
// Source does not have to be on the error, it can be mapped with with_source_code to save space and performance
pub struct LexError {
    #[label = "Here"]
    err_span: Range<usize>,
    #[help]
    help: String,
}

impl LexError {
    pub(crate) fn new(err_span: Range<usize>, help: String) -> Self {
        LexError { err_span, help }
    }
}
