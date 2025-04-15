use la_arena::{Arena, Idx, RawIdx};
use patricia_tree::PatriciaMap;
use smol_str::SmolStr;

use crate::{
    HIRResult,
    errors::HIRError,
    expression::Expr,
    function::FnDef,
    metadata::{FnMeta, StructMeta, VarMeta},
    resolution::Unresolved,
    structure::StructDef,
    tensor::CanonicalTensor,
    variable::VarDef,
};

use std::{collections::HashMap, fmt::Debug, ops::Range};

pub type ExprIdx = Idx<Expr>;
pub type FnDefIdx = Idx<FnDef>;
pub type ScopeIdx = Idx<Scope>;
pub type StrIdx = Idx<SmolStr>;
pub type StructDefIdx = Idx<StructDef>;
pub type TensorLiteralIdx = Idx<CanonicalTensor>;
pub type VarDefIdx = Idx<VarDef>;

pub type NameToIndexTrie<T> = PatriciaMap<Idx<T>>;

pub type Span = Range<usize>;

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
    fn set_name_index(&mut self, ix: Idx<SmolStr>);
}

#[derive(Debug)]
pub enum ScopeKind {
    Block,
    Function,
    Impl,
    Master,
    Struct,
}

#[derive(Debug)]
pub struct DefAllocator<D: NameIndexed> {
    pub names: Arena<SmolStr>,
    pub definitions: Arena<D>,
    pub name_to_idx_trie: NameToIndexTrie<D>,
}

impl<D: NameIndexed> DefAllocator<D> {
    pub fn new() -> Self {
        Self {
            names: Arena::<SmolStr>::new(),
            definitions: Arena::<D>::new(),
            name_to_idx_trie: NameToIndexTrie::<D>::new(),
        }
    }
    pub fn alloc(&mut self, name: SmolStr, mut d: D) -> HIRResult<Idx<D>> {
        if self.name_to_idx_trie.get(&name).is_some() {
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

#[derive(Debug)]
pub struct MetadataStore {
    pub vars: MetaHolder<VarDef, VarMeta>,
    pub fns: MetaHolder<FnDef, FnMeta>,
    pub structs: MetaHolder<StructDef, StructMeta>,
}
impl MetadataStore {
    pub fn new() -> Self {
        Self {
            vars: MetaHolder::<VarDef, VarMeta>::new(),
            fns: MetaHolder::<FnDef, FnMeta>::new(),
            structs: MetaHolder::<StructDef, StructMeta>::new(),
        }
    }
}

pub trait Selector<E: Clone + Debug + PartialEq + NameIndexed> {
    fn select_alloc(scope: &Scope) -> &DefAllocator<E>;
    fn select_alloc_mut(scope: &mut Scope) -> &mut DefAllocator<E>;
}
// note: container refs are also variable refs.
pub struct VarSelector {}
impl Selector<VarDef> for VarSelector {
    fn select_alloc(scope: &Scope) -> &DefAllocator<VarDef> {
        &scope.variable_allocator
    }
    fn select_alloc_mut(scope: &mut Scope) -> &mut DefAllocator<VarDef> {
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
    pub(crate) variable_allocator: DefAllocator<VarDef>,

    pub(crate) metadata: MetadataStore,

    pub(crate) strs: Arena<SmolStr>,
    pub(crate) tensor_literals: Arena<CanonicalTensor>,

    pub(crate) to_resolve: Arena<Unresolved>,
    pub(crate) name_to_spans: PatriciaMap<Span>,
}

impl Scope {
    pub fn new(parent: ScopeIdx, kind: ScopeKind) -> Self {
        let exprs = expr_arena_with_missing();

        let variable_allocator = DefAllocator::<VarDef>::new();
        let struct_allocator = DefAllocator::<StructDef>::new();
        let fn_allocator = DefAllocator::<FnDef>::new();

        let metadata = MetadataStore::new();

        let strs = Arena::<SmolStr>::new();
        let tensor_literals = Arena::<CanonicalTensor>::new();

        let to_resolve = Arena::<Unresolved>::new();
        let name_to_spans = PatriciaMap::<Span>::new();

        Self {
            kind,
            parent,
            exprs,
            fn_allocator,
            struct_allocator,
            variable_allocator,
            metadata,
            strs,
            tensor_literals,
            to_resolve,
            name_to_spans,
        }
    }

    pub fn resolve_in<E, S: Selector<E>>(&self, key: &SmolStr) -> Option<&Idx<E>>
    where
        E: Clone + Debug + PartialEq + NameIndexed,
    {
        S::select_alloc(self).name_to_idx_trie.get(key)
    }
    pub fn allocate<E, S: Selector<E>>(&mut self, name: SmolStr, elm: E) -> HIRResult<Idx<E>>
    where
        E: Clone + Debug + PartialEq + NameIndexed,
    {
        let allocator = S::select_alloc_mut(self);
        allocator.alloc(name, elm)
    }

    pub fn allocate_expr(&mut self, expr: Expr) -> ExprIdx {
        self.exprs.alloc(expr)
    }
    pub fn allocate_span(&mut self, name: &SmolStr, span: Span) {
        self.name_to_spans.insert(name, span);
    }
    pub fn allocate_string(&mut self, string: SmolStr) -> StrIdx {
        self.strs.alloc(string)
    }
    pub fn allocate_tensor_literal(&mut self, tensor_literal: CanonicalTensor) -> TensorLiteralIdx {
        self.tensor_literals.alloc(tensor_literal)
    }
}
