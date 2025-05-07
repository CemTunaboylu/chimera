use crate::{
    climbing::EnumeratedScope,
    delimited::Indexing,
    scope::{NameIndexed, Selector, Span, placeholder_idx},
    unwrap_or_err,
};
use la_arena::Idx;
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
    pub fn unresolved(unresolved: impl Debug) -> Self {
        let msg = format!("{:?} is unresolved", unresolved);
        ResolutionError { msg }
    }
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

pub fn resolve<'caller, E, S: Selector<E>>(
    mut scopes: impl Iterator<Item = EnumeratedScope<'caller>>,
    unresolved_ref: &Reference<E>,
) -> ResolutionResult<Reference<E>>
where
    E: Clone + Debug + PartialEq + NameIndexed,
{
    let current_scope = scopes.next();
    let (key, resolution_type, baggage) = {
        let enumerated_scope = *unwrap_or_err(
            current_scope.as_ref(),
            format!("cannot find scope for {:?}", unresolved_ref).as_str(),
        )?;
        let idx = unresolved_ref.get_unresolved_index()?;
        let unresolved = &enumerated_scope.1.to_resolve[idx];
        (&unresolved.name, &unresolved.for_type, &unresolved.baggage)
    };

    let mut guesstimates = ThinVec::new();
    for (scope_idx, scope) in scopes {
        if let Some((name_idx, obj_idx)) = scope.resolve_in::<E, S>(key) {
            span_check(scope, key, resolution_type)?;
            let resolved = Reference::<E>::Resolved {
                at: scope_idx,
                baggage: baggage.clone(),
                name_idx,
                obj_idx,
            };
            return Ok(resolved);
        } else {
            guesstimates.push(guess(&S::select_alloc(scope).name_to_idx_trie, key));
        }
    }
    Err(ResolutionError::new_with_guestimate(key, guesstimates).into())
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Baggage {
    None,
    Arg(ThinVec<FnArg>),
    Index(ThinVec<Indexing>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Unresolved {
    pub name: SmolStr,
    pub baggage: Baggage,
    pub for_type: ResolutionType,
}

impl Unresolved {
    pub fn baggaged(name: SmolStr, baggage: Baggage, t: ResolutionType) -> Self {
        Self {
            name,
            baggage,
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
    Resolved {
        at: ScopeIdx,
        baggage: Baggage,
        name_idx: Idx<SmolStr>,
        obj_idx: Idx<R>,
    },
}

impl<D: Default> Default for Reference<D> {
    fn default() -> Self {
        Self::Unresolved(placeholder_idx())
    }
}

impl<R: Debug> Reference<R> {
    pub fn get_unresolved_index(&self) -> ResolutionResult<Idx<Unresolved>> {
        match self {
            Reference::Resolved { .. } => Err(ResolutionError::already_resolved(self).into()),
            Reference::Unresolved(idx) => Ok(*idx),
        }
    }
    pub fn get_obj_index(&self) -> ResolutionResult<Idx<R>> {
        match self {
            Reference::Resolved {
                at: _,
                baggage: _,
                name_idx: _,
                obj_idx,
            } => Ok(*obj_idx),
            Reference::Unresolved(_) => Err(ResolutionError::unresolved(self).into()),
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
