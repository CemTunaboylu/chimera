use crate::{
    HIRResult,
    collection::{
        canonical::CanonicalBuffer,
        meta::CollectionMeta,
        storage::{Storage, StorageIdx},
        uninit::LazyInit,
    },
    errors::HIRError,
    expression::Expr,
    function::FnDef,
    let_binding::LetBinding,
    metadata::{FnMeta, StructMeta, VarMeta},
    resolution::Unresolved,
    statement::Stmt,
    structure::StructDef,
};
use la_arena::{Arena, Idx, RawIdx};
use patricia_tree::PatriciaMap;
use smol_str::SmolStr;

use std::{cmp::Ordering, collections::HashMap, fmt::Debug, ops::Range};
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

pub type NameToIndexTrie<T> = PatriciaMap<Idx<T>>;

#[derive(Clone, Copy, Default, Eq, Hash, PartialEq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}..{}", self.start, self.end))
    }
}

impl PartialOrd for Span {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if let Some(result) = self.start.partial_cmp(&other.end) {
            match result {
                Ordering::Greater | Ordering::Less => Some(result),
                Ordering::Equal => self.end.partial_cmp(&other.start),
            }
        } else {
            self.end.partial_cmp(&other.start)
        }
    }
}

impl From<Range<usize>> for Span {
    fn from(value: Range<usize>) -> Self {
        Self {
            start: value.start,
            end: value.end,
        }
    }
}

pub(crate) fn into_idx<T>(from: u32) -> Idx<T> {
    Idx::from_raw(RawIdx::from_u32(from))
}

pub(crate) fn placeholder_idx<FOR>() -> Idx<FOR> {
    Idx::from_raw(RawIdx::from_u32(0))
}

fn expr_arena_with_missing() -> Arena<Expr> {
    let mut arena = Arena::new();
    _ = arena.alloc(Expr::Missing);
    arena
}

pub trait NameIndexed {
    fn set_name_index(&mut self, ix: StrIdx);
    fn get_name_index(&self) -> StrIdx;
}

#[derive(Debug)]
pub enum ScopeKind {
    Block,
    Function,
    Impl,
    Master,
    Struct,
}

#[derive(Debug, Default)]
pub struct DefAllocator<D: NameIndexed> {
    pub names: Arena<SmolStr>,
    pub definitions: Arena<D>,
    pub name_to_idx_trie: NameToIndexTrie<D>,
}

impl<D: NameIndexed> DefAllocator<D> {
    pub fn alloc(&mut self, name: &SmolStr, mut d: D) -> HIRResult<Idx<D>> {
        if self.name_to_idx_trie.get(name).is_some() {
            return Err(HIRError::with_msg(format!(
                "shadowing is not allowed, {:?} is already defined",
                name
            ))
            .into());
        }
        let name_index = self.names.alloc(name.clone());
        d.set_name_index(name_index);

        let ix = self.definitions.alloc(d);
        self.name_to_idx_trie.insert(&name, ix);
        Ok(ix)
    }
}

pub type MetaHolder<L, M> = HashMap<Idx<L>, M>;

#[derive(Debug, Default)]
pub struct MetadataStore {
    pub vars: MetaHolder<LetBinding, VarMeta>,
    pub fns: MetaHolder<FnDef, FnMeta>,
    pub structs: MetaHolder<StructDef, StructMeta>,
    pub buffers: MetaHolder<CanonicalBuffer, CollectionMeta>,
}
pub trait Selector<E: Clone + Debug + PartialEq + NameIndexed> {
    fn select_alloc(scope: &Scope) -> &DefAllocator<E>;
    fn select_alloc_mut(scope: &mut Scope) -> &mut DefAllocator<E>;
}
// note: container refs are also variable refs.
pub struct VarSelector {}
impl Selector<LetBinding> for VarSelector {
    fn select_alloc(scope: &Scope) -> &DefAllocator<LetBinding> {
        &scope.variable_allocator
    }
    fn select_alloc_mut(scope: &mut Scope) -> &mut DefAllocator<LetBinding> {
        &mut scope.variable_allocator
    }
}
pub struct FnSelector {}
impl Selector<FnDef> for FnSelector {
    fn select_alloc(scope: &Scope) -> &DefAllocator<FnDef> {
        &scope.fn_allocator
    }
    fn select_alloc_mut(scope: &mut Scope) -> &mut DefAllocator<FnDef> {
        &mut scope.fn_allocator
    }
}

pub struct StructSelector {}
impl Selector<StructDef> for StructSelector {
    fn select_alloc(scope: &Scope) -> &DefAllocator<StructDef> {
        &scope.struct_allocator
    }
    fn select_alloc_mut(scope: &mut Scope) -> &mut DefAllocator<StructDef> {
        &mut scope.struct_allocator
    }
}

#[derive(Debug)]
pub struct Scope {
    pub(crate) kind: ScopeKind,
    pub(crate) parent: ScopeIdx,

    pub(crate) exprs: Arena<Expr>,
    pub(crate) fn_allocator: DefAllocator<FnDef>,
    pub(crate) struct_allocator: DefAllocator<StructDef>,
    pub(crate) variable_allocator: DefAllocator<LetBinding>,

    pub(crate) metadata: MetadataStore,

    // ! TODO: have literals in a separate arena
    pub(crate) buffer_literals: Arena<CanonicalBuffer>,
    pub(crate) lazy_inits: Arena<LazyInit>,
    pub(crate) tensor_literals: Arena<CanonicalBuffer>,

    pub(crate) to_resolve: Arena<Unresolved>,
    pub(crate) name_to_spans: PatriciaMap<Span>,

    pub(crate) statements: Arena<Stmt>,
}

impl Scope {
    pub fn new(parent: ScopeIdx, kind: ScopeKind) -> Self {
        let exprs = expr_arena_with_missing();

        let variable_allocator = DefAllocator::<LetBinding>::default();
        let struct_allocator = DefAllocator::<StructDef>::default();
        let fn_allocator = DefAllocator::<FnDef>::default();

        let metadata = MetadataStore::default();

        let buffer_literals = Arena::<CanonicalBuffer>::new();
        let lazy_inits = Arena::<LazyInit>::new();
        let tensor_literals = Arena::<CanonicalBuffer>::new();

        let to_resolve = Arena::<Unresolved>::new();
        let name_to_spans = PatriciaMap::<Span>::new();

        let statements = Arena::new();

        Self {
            kind,
            parent,
            exprs,
            fn_allocator,
            struct_allocator,
            variable_allocator,
            metadata,
            buffer_literals,
            lazy_inits,
            tensor_literals,
            to_resolve,
            name_to_spans,
            statements,
        }
    }

    pub fn resolve_in<E, S: Selector<E>>(&self, key: &SmolStr) -> Option<(StrIdx, Idx<E>)>
    where
        E: Clone + Debug + PartialEq + NameIndexed,
    {
        let allocator = S::select_alloc(self);

        // * Note: to avoid code duplication , we 'mess' with the internals of allocators
        // * rather than having them implement the same thing themselves
        allocator.name_to_idx_trie.get(key).map(|idx| {
            let d = &allocator.definitions[*idx];
            let name_idx = d.get_name_index();
            (name_idx, *idx)
        })
    }
    pub fn allocate<E, S: Selector<E>>(&mut self, name: &SmolStr, elm: E) -> HIRResult<Idx<E>>
    where
        E: Clone + Debug + PartialEq + NameIndexed,
    {
        let allocator = S::select_alloc_mut(self);
        allocator.alloc(&name, elm)
    }
    pub fn allocate_expr(&mut self, expr: Expr) -> ExprIdx {
        self.exprs.alloc(expr)
    }
    pub fn allocate_stmt(&mut self, stmt: Stmt) -> StmtIdx {
        self.statements.alloc(stmt)
    }
    pub fn allocate_span(&mut self, name: &SmolStr, span: Span) {
        self.name_to_spans.insert(name, span);
    }
    pub fn allocate_tensor_literal(
        &mut self,
        tensor_literal: CanonicalBuffer,
    ) -> CollectionLiteralIdx {
        self.tensor_literals.alloc(tensor_literal)
    }
    pub fn allocate_lazy_init(&mut self, lazy_init: LazyInit) -> LazyInitIdx {
        self.lazy_inits.alloc(lazy_init)
    }
}
