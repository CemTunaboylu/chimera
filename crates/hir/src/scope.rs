use la_arena::{Arena, Idx, RawIdx};
use patricia_tree::PatriciaMap;
use smol_str::SmolStr;

use crate::{
    expression::Expr, function::FnDef, resolution::Unresolved, structure::StructDef,
    tensor::CanonicalTensor, variable::VarDef,
};

pub type ExprIdx = Idx<Expr>;
pub type FnDefIdx = Idx<FnDef>;

pub type StrIdx = Idx<SmolStr>;
pub type StructDefIdx = Idx<StructDef>;
// pub type UnresolvedStructAsTypeIdx = Idx<UnresolvedStructAsType>;
pub type TensorLiteralIdx = Idx<CanonicalTensor>;
// pub type UnresolvedFnCallIdx = Idx<UnresolvedFnCall>;
// pub type UnresolvedVarRefIdx = Idx<UnresolvedVarRef>;
// pub type UnresolvedContainerRefIdx = Idx<UnresolvedContainerRef>;

pub type VarDefIdx = Idx<VarDef>;

pub type NameToIndexTrie<T> = PatriciaMap<Idx<T>>;

pub(crate) fn into_idx<T>(from: u32) -> Idx<T> {
    Idx::from_raw(RawIdx::from_u32(from))
}

pub fn expr_arena_with_missing() -> Arena<Expr> {
    let mut arena = Arena::new();
    _ = arena.alloc(Expr::Missing);
    arena
}
pub type ScopeIdx = Idx<Scope>;

#[derive(Debug)]
pub enum ScopeKind {
    Block,
    Function,
    Impl,
    Master,
    Struct,
}

// TODO: I may keep all symbols in one place?
#[derive(Debug)]
pub struct Scope {
    pub(crate) parent: ScopeIdx,
    pub(crate) kind: ScopeKind,

    pub(crate) exprs: Arena<Expr>,

    pub(crate) var_names: Arena<SmolStr>,
    pub(crate) var_defs: Arena<VarDef>,
    pub(crate) var_name_to_idx_trie: NameToIndexTrie<VarDef>,

    pub(crate) struct_names: Arena<SmolStr>,
    pub(crate) struct_defs: Arena<StructDef>,
    pub(crate) struct_name_to_idx_trie: NameToIndexTrie<StructDef>,

    pub(crate) strs: Arena<SmolStr>,
    pub(crate) tensor_literals: Arena<CanonicalTensor>,

    pub(crate) fn_defs: Arena<FnDef>,
    pub(crate) fn_names: Arena<SmolStr>,
    pub(crate) fn_name_to_idx_trie: NameToIndexTrie<FnDef>,

    // pub(crate) vars_to_resolve: Arena<UnresolvedVarRef>,
    // pub(crate) struct_as_types_to_resolve: Arena<UnresolvedStructAsType>,
    // pub(crate) containers_to_resolve: Arena<UnresolvedContainerRef>,
    // pub(crate) fns_to_resolve: Arena<UnresolvedFnCall>,
    pub(crate) to_resolve: Arena<Unresolved>,
}

impl Scope {
    pub fn new(parent: ScopeIdx, kind: ScopeKind) -> Self {
        let exprs = expr_arena_with_missing();

        let var_names = Arena::<SmolStr>::new();
        let var_defs = Arena::<VarDef>::new();
        let var_name_to_idx_trie = NameToIndexTrie::<VarDef>::new();

        let struct_names = Arena::<SmolStr>::new();
        let struct_defs = Arena::<StructDef>::new();
        let struct_name_to_idx_trie = NameToIndexTrie::<StructDef>::new();

        let strs = Arena::<SmolStr>::new();
        let tensor_literals = Arena::<CanonicalTensor>::new();

        let fn_defs = Arena::<FnDef>::new();
        let fn_names = Arena::<SmolStr>::new();
        let fn_name_to_idx_trie = NameToIndexTrie::<FnDef>::new();

        // let vars_to_resolve = Arena::<UnresolvedVarRef>::new();
        // let struct_as_types_to_resolve = Arena::<UnresolvedStructAsType>::new();
        // let containers_to_resolve = Arena::<UnresolvedContainerRef>::new();
        // let fns_to_resolve = Arena::<UnresolvedFnCall>::new();

        let to_resolve = Arena::<Unresolved>::new();

        Self {
            parent,
            kind,
            exprs,
            var_names,
            var_defs,
            var_name_to_idx_trie,
            struct_names,
            struct_defs,
            struct_name_to_idx_trie,
            strs,
            tensor_literals,
            fn_defs,
            fn_names,
            fn_name_to_idx_trie,
            // vars_to_resolve,
            // struct_as_types_to_resolve,
            // fns_to_resolve,
            // containers_to_resolve,
            to_resolve,
        }
    }

    // pub fn allocate_unresolved_var(
    //     &mut self,
    //     unresolved_var: UnresolvedVarRef,
    // ) -> UnresolvedVarRefIdx {
    //     self.vars_to_resolve.alloc(unresolved_var)
    // }
    // pub fn allocate_unresolved_container(
    //     &mut self,
    //     unresolved_container: UnresolvedContainerRef,
    // ) -> UnresolvedContainerRefIdx {
    //     self.containers_to_resolve.alloc(unresolved_container)
    // }
    // pub fn allocate_unresolved_fn_call(
    //     &mut self,
    //     unresolved_fn_call: UnresolvedFnCall,
    // ) -> UnresolvedFnCallIdx {
    //     self.fns_to_resolve.alloc(unresolved_fn_call)
    // }
    // pub fn allocate_unresolved_struct_ref(
    //     &mut self,
    //     unresolved_struct_ref: UnresolvedStructAsType,
    // ) -> UnresolvedStructAsTypeIdx {
    //     self.struct_as_types_to_resolve.alloc(unresolved_struct_ref)
    // }

    pub fn allocate_var_def_with_name(&mut self, name: &SmolStr, var_def: VarDef) -> VarDefIdx {
        let idx = self.var_defs.alloc(var_def);
        self.var_name_to_idx_trie.insert(name, idx);
        idx
    }
    pub fn allocate_struct_def_with_name(
        &mut self,
        name: &SmolStr,
        struct_def: StructDef,
    ) -> StructDefIdx {
        let idx = self.struct_defs.alloc(struct_def);
        self.struct_name_to_idx_trie.insert(name, idx);
        idx
    }

    pub fn allocate_expr(&mut self, expr: Expr) -> ExprIdx {
        self.exprs.alloc(expr)
    }
    pub fn allocate_string(&mut self, string: SmolStr) -> StrIdx {
        self.strs.alloc(string)
    }
    pub fn allocate_tensor_literal(&mut self, tensor_literal: CanonicalTensor) -> TensorLiteralIdx {
        self.tensor_literals.alloc(tensor_literal)
    }
    pub fn insert_fn_def_in_trie(&mut self, key: &SmolStr, value: FnDefIdx) {
        self.fn_name_to_idx_trie.insert(key, value);
    }
}
