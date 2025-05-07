use hir_macro::scoped;
use thin_vec::ThinVec;

use ast::impl_block::Impl as ASTImpl;

use crate::{
    HIRResult,
    builder::HIRBuilder,
    scope::{FnDefIdx, ScopeIdx, ScopeKind},
    typing::hindley_milner::types::Type,
};

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub struct Impl {
    pub scope_idx: ScopeIdx,
    pub of: Type,
    pub methods: ThinVec<FnDefIdx>,
}

impl HIRBuilder {
    #[scoped(ScopeKind::Impl)]
    pub fn lower_impl_block(&mut self, impl_block: &ASTImpl) -> HIRResult<Impl> {
        let scope_idx = self.current_scope_cursor;
        let of = self.lower_type(&impl_block.of)?;
        let mut lowered_methods = ThinVec::with_capacity(impl_block.methods.len());
        for method in impl_block.methods.iter() {
            let mtd_ix = self.lower_fn_def(method)?;
            lowered_methods.push(mtd_ix);
        }
        Ok(Impl {
            scope_idx,
            of,
            methods: lowered_methods,
        })
    }
}

#[cfg(test)]
mod tests {
    use ast::cast_node_into_type;

    use super::*;
    use crate::{
        builder::tests::ast_root_from, scope::into_idx, typing::hindley_milner::types::Status,
    };

    #[test]
    fn impl_block() {
        let program = "impl Point { fn translate(&mut self, by: Point) { self.x += by.x; self.y += by.y; } fn rotate(&mut self, by: Point) { self.rotate_around(&by);} \n}";
        let method_names = ["translate", "rotate"];

        let ast_root = ast_root_from(program);
        let ast_impl_block =
            cast_node_into_type::<ASTImpl>(ast_root.get_root().first_child().as_ref().unwrap());
        let mut hir_builder = HIRBuilder::new(ast_root);
        let impl_block = hir_builder
            .lower_impl_block(&ast_impl_block)
            .expect("should have been ok");

        let scope_idx = impl_block.scope_idx;
        let scope = &hir_builder.scopes[scope_idx];
        let fn_defs = &scope.fn_allocator.definitions;
        let fn_names = &scope.fn_allocator.names;

        assert_eq!(
            Type::StructAsType(Status::Pending(into_idx(0))),
            impl_block.of
        );
        for (fn_def_idx, exp_fn_name) in impl_block.methods.iter().zip(method_names.iter()) {
            let fn_def = &fn_defs[*fn_def_idx];
            let fn_name = &fn_names[fn_def.name_index];
            assert_eq!(*exp_fn_name, fn_name);
        }
    }
}
