use crate::delimited::Indexing;
use la_arena::{Arena, Idx};
use miette::{Diagnostic, Report};
use smol_str::SmolStr;
use thin_vec::ThinVec;
use thiserror::Error;

use core::hash::Hash;

use std::{borrow::Cow, fmt::Debug};

use crate::{
    HIRResult,
    builder::HIRBuilder,
    function::FnArg,
    scope::{NameToIndexTrie, Scope, ScopeIdx},
    variable::VarDef,
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
    Container,
    Fn,
    Struct,
    Var,
}

pub trait ScopeTrieClimber<E: Clone + Debug + PartialEq> {
    fn corresponding_trie_in(scope: &Scope) -> &NameToIndexTrie<E>;
}
// note: container refs are also variable refs.
pub struct VarRefClimber {}
impl ScopeTrieClimber<VarDef> for VarRefClimber {
    fn corresponding_trie_in(scope: &Scope) -> &NameToIndexTrie<VarDef> {
        &scope.var_name_to_idx_trie
    }
}

fn resolve<E, TC: ScopeTrieClimber<E>>(
    mut current: ScopeIdx,
    scopes: &Arena<Scope>,
    key: &SmolStr,
) -> ResolutionResult<(ScopeIdx, Idx<E>)>
where
    E: Clone + Debug + PartialEq,
{
    let mut scope = &scopes[current];
    let mut guesstimates = ThinVec::new();
    loop {
        if let Some(idx) = TC::corresponding_trie_in(scope).get(key) {
            return Ok((current, *idx));
        } else {
            let parent = scope.parent;
            if parent == current {
                return Err(ResolutionError::new_with_guestimate(key, guesstimates).into());
            }
            guesstimates.push(guess(&scope.var_name_to_idx_trie, key));
            scope = &scopes[parent];
            current = parent;
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Baggage {
    None,
    Arg(ThinVec<FnArg>),
    Index(ThinVec<Indexing>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Unresolved {
    name: SmolStr,
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
    Resolved { at: ScopeIdx, element: R },
}

// can implement a trait for masking Scope

impl HIRBuilder {
    pub fn allocate_for_resolution(&mut self, unresolved: Unresolved) -> Idx<Unresolved> {
        let current_scope = self.get_current_scope_mut();
        current_scope.to_resolve.alloc(unresolved)
    }

    pub fn resolve<R: Clone + Debug + PartialEq>(
        &mut self,
        unresolved_reference: Reference<R>,
    ) -> ResolutionResult<R> {
        if matches!(unresolved_reference, Reference::Resolved { .. }) {
            return Err(ResolutionError::already_resolved(unresolved_reference).into());
        }

        use ResolutionType::*;

        if let Reference::Unresolved(idx) = unresolved_reference {
            let current_scope = self.get_current_scope();
            let unresolved = &current_scope.to_resolve[idx];
            let trie = match unresolved.for_type {
                Container => todo!(),
                Fn => todo!(),
                Struct => todo!(),
                Var => todo!(),
            };
        }
        todo!()
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
