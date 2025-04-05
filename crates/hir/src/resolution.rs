use la_arena::Idx;
use miette::{Diagnostic, Report};
use smol_str::SmolStr;
use thiserror::Error;

use std::fmt::Debug;

use crate::hir::{HIRBuilder, NameToIndexTrie};

pub type ResolutionResult<T> = Result<T, Report>;

#[derive(Clone, Diagnostic, Debug, PartialEq, Error)]
#[diagnostic()]
#[error("ResolutionError")]
// Source will be injected at the end by the caller
pub struct ResolutionError {
    #[help]
    msg: String,
}

impl ResolutionError {
    pub fn new(cannot_resolve: impl Debug, guestimate_help: impl Debug) -> Self {
        let msg = format!(
            "cannot resolve {:?}, did you mean {:?}",
            cannot_resolve, guestimate_help
        );
        ResolutionError { msg }
    }
}

impl HIRBuilder {
    pub fn resolve_with_err<T>(
        trie: &NameToIndexTrie<Idx<T>>,
        key: &SmolStr,
    ) -> ResolutionResult<Idx<T>> {
        if let Some(idx) = trie.get(key) {
            Ok(*idx)
        } else {
            let empty: &[u8] = &[];
            let guesstimate = trie
                .get_longest_common_prefix(&key)
                .map(|(chars, _)| chars)
                .unwrap_or(empty);
            Err(ResolutionError::new(key, guesstimate).into())
        }
    }
}

mod private {
    pub trait Sealed {}
}
pub trait ResolutionStatus: private::Sealed {}

#[derive(Clone, Debug, PartialEq)]
pub enum Unresolved {}
impl private::Sealed for Unresolved {}
impl ResolutionStatus for Unresolved {}

#[derive(Clone, Debug, PartialEq)]
pub enum Resolved {}
impl private::Sealed for Resolved {}
impl ResolutionStatus for Resolved {}
