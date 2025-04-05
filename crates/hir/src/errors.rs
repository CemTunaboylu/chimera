use miette::Diagnostic;
use thin_vec::ThinVec;
use thiserror::Error;

use std::{fmt::Debug, ops::Range};
use syntax::{
    bitset::SyntaxKindBitSet,
    language::{NodeOrToken, SyntaxNode, SyntaxToken},
    syntax_kind::SyntaxKind,
};

#[derive(Clone, Diagnostic, Debug, PartialEq, Error)]
#[diagnostic()]
#[error("HIRError")]
// Source will be injected at the end by the caller
pub struct HIRError {
    #[help]
    expected_but_found: String,
}

impl HIRError {
    pub fn new(expected: impl Debug, got_opt: impl Debug) -> Self {
        let expected_but_found = format!("expected {:?}, but got {:?}", expected, got_opt);
        Self { expected_but_found }
    }
    pub fn for_ast(elm: impl Debug, expected: impl Debug) -> Self {
        let expected_but_found = format!("expected {:?}, but got {:?}", expected, elm);
        Self { expected_but_found }
    }
    pub fn from_err(err: impl Debug) -> Self {
        let expected_but_found = format!("{:?}", err);
        Self { expected_but_found }
    }
    pub fn with_msg(msg: impl Debug) -> Self {
        let expected_but_found = format!("{:?}", msg);
        Self { expected_but_found }
    }
}
