use std::ops::Range;

use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

#[derive(Clone, Default, Diagnostic, Debug, PartialEq, Error)]
#[diagnostic()]
#[error("LexError")]
pub struct LexError {
    #[source_code]
    src: String,

    #[label = "Here"]
    err_span: Range<usize>,

    #[label("Related")]
    related: Option<SourceSpan>,
}

impl LexError {
    pub(crate) fn new(src: String, err_span: Range<usize>) -> Self {
        LexError {
            src,
            err_span,
            related: None,
        }
    }
}
