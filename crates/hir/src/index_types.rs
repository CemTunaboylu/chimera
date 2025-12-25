use crate::{
    collection::{canonical::CanonicalBuffer, uninit::LazyInit},
    expression::Expr,
    function::FnDef,
    let_binding::LetBinding,
    scope::Scope,
    statement::Stmt,
    structure::StructDef,
};
use la_arena::{Idx, RawIdx};
use smol_str::SmolStr;

use std::fmt::Debug;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub struct Scoped<A> {
    pub scope_idx: ScopeIdx,
    pub elm: A,
}

impl<A> Scoped<A> {
    pub fn new(scope_idx: ScopeIdx, elm: A) -> Self {
        Self { scope_idx, elm }
    }
}

pub type ExprIdx = Idx<Expr>;
pub type FnDefIdx = Idx<FnDef>;
pub type ScopeIdx = Idx<Scope>;
pub type ScopedExprIdx = Scoped<ExprIdx>;
pub type ScopedStmtIdx = Scoped<StmtIdx>;
pub type StrIdx = Idx<SmolStr>;
pub type StmtIdx = Idx<Stmt>;
pub type StructDefIdx = Idx<StructDef>;
pub type LazyInitIdx = Idx<LazyInit>;
pub type CollectionLiteralIdx = Idx<CanonicalBuffer>;
pub type ScopedCollectionLiteralIdx = Scoped<CollectionLiteralIdx>;
pub type LetBindingIdx = Idx<LetBinding>;

pub(crate) fn into_idx<T>(from: u32) -> Idx<T> {
    Idx::from_raw(RawIdx::from_u32(from))
}

pub(crate) fn placeholder_idx<FOR>() -> Idx<FOR> {
    Idx::from_raw(RawIdx::from_u32(0))
}
