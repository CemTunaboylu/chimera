use la_arena::{Arena, Idx, RawIdx};
use patricia_tree::PatriciaMap;
use smol_str::SmolStr;

use crate::{
    HIRResult,
    container::canonical::CanonicalBuffer,
    errors::HIRError,
    expression::Expr,
    function::FnDef,
    let_binding::LetBinding,
    metadata::{FnMeta, StructMeta, VarMeta},
    resolution::Unresolved,
    structure::StructDef,
};

use std::{cmp::Ordering, collections::HashMap, fmt::Debug, ops::Range};

pub type ExprIdx = Idx<Expr>;
pub type FnDefIdx = Idx<FnDef>;
pub type ScopeIdx = Idx<Scope>;
pub type StrIdx = Idx<SmolStr>;
pub type StructDefIdx = Idx<StructDef>;
pub type ContainerLiteralIdx = Idx<CanonicalBuffer>;
pub type LetBindingIdx = Idx<LetBinding>;

pub type NameToIndexTrie<T> = PatriciaMap<Idx<T>>;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
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
    pub name_to_name_idx: HashMap<SmolStr, StrIdx>,
    pub definitions: Arena<D>,
    pub name_to_idx_trie: NameToIndexTrie<D>,
}

impl<D: NameIndexed> DefAllocator<D> {
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

#[derive(Debug, Default)]
pub struct MetadataStore {
    pub vars: MetaHolder<LetBinding, VarMeta>,
    pub fns: MetaHolder<FnDef, FnMeta>,
    pub structs: MetaHolder<StructDef, StructMeta>,
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

    pub(crate) strs: Arena<SmolStr>,
    pub(crate) tensor_literals: Arena<CanonicalBuffer>,
    pub(crate) buffer_literals: Arena<CanonicalBuffer>,

    pub(crate) to_resolve: Arena<Unresolved>,
    pub(crate) name_to_spans: PatriciaMap<Span>,
}

impl Scope {
    pub fn new(parent: ScopeIdx, kind: ScopeKind) -> Self {
        let exprs = expr_arena_with_missing();

        let variable_allocator = DefAllocator::<LetBinding>::default();
        let struct_allocator = DefAllocator::<StructDef>::default();
        let fn_allocator = DefAllocator::<FnDef>::default();

        let metadata = MetadataStore::default();

        let strs = Arena::<SmolStr>::new();
        let tensor_literals = Arena::<CanonicalBuffer>::new();
        let buffer_literals = Arena::<CanonicalBuffer>::new();

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
            buffer_literals,
            to_resolve,
            name_to_spans,
        }
    }

    pub fn resolve_in<E, S: Selector<E>>(&self, key: &SmolStr) -> Option<(StrIdx, Idx<E>)>
    where
        E: Clone + Debug + PartialEq + NameIndexed,
    {
        let allocator = S::select_alloc(self);

        allocator.name_to_idx_trie.get(key).map(|idx| {
            let d = &allocator.definitions[*idx];
            let name_idx = d.get_name_index();
            (name_idx, *idx)
        })
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
    pub fn allocate_tensor_literal(
        &mut self,
        tensor_literal: CanonicalBuffer,
    ) -> ContainerLiteralIdx {
        self.tensor_literals.alloc(tensor_literal)
    }
}
