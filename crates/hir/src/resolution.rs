use crate::{
    climbing::climb,
    delimited::Indexing,
    scope::{Selector, Span},
};
use la_arena::{Arena, Idx};
use miette::{Diagnostic, Report};
use smol_str::SmolStr;
use thin_vec::ThinVec;
use thiserror::Error;

use core::hash::Hash;

use std::{borrow::Cow, fmt::Debug};

use crate::{
    builder::HIRBuilder,
    function::FnArg,
    scope::{NameToIndexTrie, Scope, ScopeIdx},
};

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
    pub fn already_resolved(cannot_resolve: impl Debug) -> Self {
        let msg = format!(
            "cannot resolve {:?}, it is already resolved",
            cannot_resolve
        );
        ResolutionError { msg }
    }

    pub fn new_with_guestimate(cannot_resolve: impl Debug, guestimate_help: impl Debug) -> Self {
        let msg = format!(
            "cannot resolve {:?}, did you mean {:?}",
            cannot_resolve, guestimate_help
        );
        ResolutionError { msg }
    }
}
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum ResolutionType {
    Container(Span),
    Fn,
    Struct,
    Var(Span),
}

impl ResolutionType {
    fn get_span(&self) -> Option<Span> {
        match self {
            ResolutionType::Container(range) => Some(range.clone()),
            ResolutionType::Var(range) => Some(range.clone()),
            _ => None,
        }
    }
}

fn span_check(
    scope: &Scope,
    key: &SmolStr,
    resolution_type: &ResolutionType,
) -> ResolutionResult<()> {
    match (resolution_type.get_span(), scope.name_to_spans.get(key)) {
        (None, None) => Ok(()),
        (None, Some(_)) => Ok(()),
        (Some(_), None) => Err(ResolutionError {
            msg: format!("cannot resolve binding {:?}", key),
        }
        .into()),
        (Some(ref_scope), Some(def_scope)) => {
            if ref_scope.start <= def_scope.end {
                Err(ResolutionError {
                    msg: format!(
                        "binding {:?} references a value that is not yet defined",
                        key
                    ),
                }
                .into())
            } else {
                Ok(())
            }
        }
    }
}

pub fn resolve<E, S: Selector<E>>(
    current: ScopeIdx,
    scopes: &Arena<Scope>,
    unresolved_ref: &Reference<E>,
) -> ResolutionResult<(ScopeIdx, Idx<E>)>
where
    E: Clone + Debug + PartialEq,
{
    let mut scope_climber = climb(current, scopes);
    let mut guesstimates = ThinVec::new();
    let (key, resolution_type) = if let Some(scope) = scope_climber.next() {
        let idx = unresolved_ref.get_unresolved_index()?;
        let unresolved = &scope.to_resolve[idx];
        (&unresolved.name, &unresolved.for_type)
    } else {
        return Err(ResolutionError {
            msg: format!("cannot find scope {:?} for {:?}", current, unresolved_ref),
        }
        .into());
    };
    for scope in scope_climber {
        if let Some(idx) = scope.resolve_in::<E, S>(key) {
            _ = span_check(scope, key, resolution_type)?;
            return Ok((current, *idx));
        } else {
            guesstimates.push(guess(&S::select(scope).name_to_idx_trie, key));
        }
    }
    return Err(ResolutionError::new_with_guestimate(key, guesstimates).into());
}

#[derive(Clone, Debug, PartialEq)]
pub enum Baggage {
    None,
    Arg(ThinVec<FnArg>),
    Index(ThinVec<Indexing>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Unresolved {
    pub name: SmolStr,
    baggage: Baggage,
    for_type: ResolutionType,
}

impl Unresolved {
    pub fn baggaged(name: SmolStr, baggage: Baggage, t: ResolutionType) -> Self {
        Self {
            name,
            baggage: baggage,
            for_type: t,
        }
    }
    pub fn new(name: SmolStr, t: ResolutionType) -> Self {
        Self {
            name,
            baggage: Baggage::None,
            for_type: t,
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Reference<R> {
    Unresolved(Idx<Unresolved>),
    Resolved { at: ScopeIdx, idx: Idx<R> },
}

impl<R: Debug> Reference<R> {
    pub fn get_unresolved_index(&self) -> ResolutionResult<Idx<Unresolved>> {
        match self {
            Reference::Resolved { .. } => Err(ResolutionError::already_resolved(self).into()),
            Reference::Unresolved(idx) => Ok(*idx),
        }
    }
}
impl HIRBuilder {
    pub fn allocate_for_resolution(&mut self, unresolved: Unresolved) -> Idx<Unresolved> {
        let current_scope = self.get_current_scope_mut();
        current_scope.to_resolve.alloc(unresolved)
    }
}

fn guess<'caller, T>(
    trie: &'caller NameToIndexTrie<T>,
    key: &'caller SmolStr,
) -> Cow<'caller, str> {
    let empty: &[u8] = &[];
    let guesstimate = trie
        .get_longest_common_prefix(key)
        .map(|(chars, _)| chars)
        .unwrap_or(empty);
    String::from_utf8_lossy(guesstimate)
}
pub fn resolve_with_err<T>(trie: &NameToIndexTrie<T>, key: &SmolStr) -> ResolutionResult<Idx<T>> {
    if let Some(idx) = trie.get(key) {
        Ok(*idx)
    } else {
        let guesstimate = guess::<T>(trie, key);
        Err(ResolutionError::new_with_guestimate(key, guesstimate).into())
    }
}
