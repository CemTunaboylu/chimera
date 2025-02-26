use std::ops::Range;

use miette::Diagnostic;
use thiserror::Error;

#[derive(Clone, Default, Diagnostic, Debug, PartialEq, Error)]
#[diagnostic()]
#[error("LexError")]
pub struct LexError {
    #[source_code]
    src: String,

    #[label = "Here"]
    err_span: Range<usize>,

    #[help]
    help: String,
}

impl LexError {
    pub(crate) fn new(src: String, err_span: Range<usize>, help: String) -> Self {
        LexError {
            src,
            err_span,
            help,
        }
    }
}
