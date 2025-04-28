use std::collections::HashMap;

use la_arena::Arena;
use smol_str::SmolStr;

use ast::{
    structure::{
        StructDef as ASTStructDef, StructField as ASTStructField, StructInit as ASTStructInit,
    },
    types::Type as ASTType,
};

use crate::{
    HIRResult,
    builder::HIRBuilder,
    climbing::climb,
    errors::HIRError,
    resolution::{Reference, ResolutionType, Unresolved, resolve},
    scope::{
        ExprIdx, NameIndexed, NameToIndexTrie, ScopeIdx, StrIdx, StructDefIdx, StructSelector,
        placeholder_idx,
    },
    typing::hindley_milner::types::Type,
};

#[derive(Clone, Debug)]
pub struct StructDef {
    pub field_name_to_arena: NameToIndexTrie<StructField>,
    pub fields: Arena<StructField>,
    pub name_index: StrIdx,
    pub scope_idx: ScopeIdx,
}
impl NameIndexed for StructDef {
    fn set_name_index(&mut self, ix: StrIdx) {
        self.name_index = ix;
    }
    fn get_name_index(&self) -> StrIdx {
        self.name_index
    }
}

impl PartialEq for StructDef {
    fn eq(&self, other: &Self) -> bool {
        self.name_index == other.name_index
            && self.scope_idx == other.scope_idx
            && self.fields == other.fields
            && self
                .field_name_to_arena
                .iter()
                .eq(other.field_name_to_arena.iter())
    }
}
#[derive(Clone, Debug, PartialEq)]
pub struct StructField {
    pub name: SmolStr,
    pub field_type: Type,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct StructRef(pub StructDefIdx);

#[derive(Clone, Debug, PartialEq)]
pub struct StructInit {
    pub struct_ref: Reference<StructRef>,
    pub scope_idx: ScopeIdx,
    // note: can use patriciatrie here as well for better error reporting
    pub field_name_to_values: HashMap<SmolStr, ExprIdx>,
}

impl HIRBuilder {
    fn lower_struct_field(
        &mut self,
        field: &ASTStructField,
        into_field_name_to_arena: &mut NameToIndexTrie<StructField>,
        into_fields: &mut Arena<StructField>,
    ) -> HIRResult<()> {
        let field_type = self.lower_type(&field.field_type)?;
        let lowered_field = StructField {
            name: field.name.clone(),
            field_type,
        };

        let idx = into_fields.alloc(lowered_field);
        into_field_name_to_arena.insert(&field.name, idx);

        Ok(())
    }
    pub fn lower_struct_def(&mut self, struct_def: &ASTStructDef) -> HIRResult<StructDefIdx> {
        let name = struct_def.name.clone();
        let scope_idx = self.current_scope_cursor;

        let mut field_name_to_arena = NameToIndexTrie::<StructField>::new();
        let mut fields = Arena::<StructField>::with_capacity(struct_def.fields.len());

        for field in struct_def.fields.iter() {
            self.lower_struct_field(field, &mut field_name_to_arena, &mut fields)?;
        }

        let struct_def = StructDef {
            field_name_to_arena,
            fields,
            name_index: placeholder_idx(),
            scope_idx,
        };

        self.allocate::<StructDef, StructSelector>(name, struct_def)
    }
    pub fn lower_struct_ref(&mut self, struct_as_type: &ASTType) -> HIRResult<Unresolved> {
        if let ASTType::Struct(name) = struct_as_type {
            Ok(Unresolved::new(name.clone(), ResolutionType::Struct))
        } else {
            Err(HIRError::with_msg(format!("expects a struct type")).into())
        }
    }
    pub fn resolve_struct_ref(
        &mut self,
        unresolved: &Reference<StructDef>,
    ) -> HIRResult<Reference<StructDef>> {
        let scope_climbing_iter = climb(self.current_scope_cursor, &self.scopes);
        Ok(resolve::<StructDef, StructSelector>(
            scope_climbing_iter,
            unresolved,
        )?)
    }

    pub fn lower_struct_init(&mut self, struct_init: &ASTStructInit) -> HIRResult<StructInit> {
        let unresolved = Unresolved::new(struct_init.name.clone(), ResolutionType::Struct);
        let unresolved_idx = self.allocate_for_resolution(unresolved);
        let struct_ref = Reference::Unresolved(unresolved_idx);
        let scope_idx = self.current_scope_cursor;

        let mut field_name_to_values =
            HashMap::<SmolStr, ExprIdx>::with_capacity(struct_init.fields.len());

        for field in struct_init.fields.iter() {
            let name = field.name.clone();
            let expr_idx = self.lower_expr_as_idx(&field.value)?;
            field_name_to_values.insert(name, expr_idx);
        }

        let struct_init = StructInit {
            field_name_to_values,
            scope_idx,
            struct_ref,
        };
        Ok(struct_init)
    }
}
#[cfg(test)]
mod tests {

    use ast::{cast_node_into_type, structure::StructDef as ASTStructDef};
    use la_arena::Idx;

    use crate::{builder::tests::ast_root_from, expression::Expr, scope::into_idx};

    use super::*;

    #[test]
    fn struct_def() {
        let field_names = ["x", "y", "item"];
        let field_types = [Type::I32, Type::I32];
        let program = "struct Tester { x: i32, y: i32, item: Item }";
        let ast_root = ast_root_from(program);
        let ast_struct_def = cast_node_into_type::<ASTStructDef>(
            ast_root.get_root().first_child().as_ref().unwrap(),
        );
        let mut hir_builder = HIRBuilder::new(ast_root);
        let idx = hir_builder
            .lower_struct_def(&ast_struct_def)
            .expect("should have been ok");
        let scope = hir_builder.get_current_scope();
        let struct_def = &scope.struct_allocator.definitions[idx];

        assert_eq!(
            "Tester",
            scope.struct_allocator.names[struct_def.name_index]
        );
        let fields = &struct_def.fields;
        for (ix, (n, t)) in field_names.iter().zip(field_types.iter()).enumerate() {
            let i: Idx<StructField> = into_idx(ix as u32);
            assert_eq!(*n, fields[i].name);
            assert_eq!(*t, fields[i].field_type);
        }
    }
    // #[test]
    // fn struct_init() {
    //     let field_names = ["x", "y", "item"];
    //     let program = "Tester { x: 0, y: 0, item: origin_item}";
    //     let ast_root = ast_root_from(program);
    //     let ast_struct_init = cast_node_into_type::<ASTStructInit>(
    //         ast_root.get_root().first_child().as_ref().unwrap(),
    //     );
    //     let mut hir_builder = HIRBuilder::new(ast_root);
    //     let struct_init = hir_builder
    //         .lower_struct_init(&ast_struct_init)
    //         .expect("should have been ok");
    //     let unresolved_idx = struct_init
    //         .struct_ref
    //         .get_unresolved_index()
    //         .expect("should have been unresolved");
    //     let scope = hir_builder.get_current_scope();
    //     let unresolved = &scope.to_resolve[unresolved_idx];

    //     assert_eq!("Tester", unresolved.name);
    //     let fields = &struct_init.field_name_to_values;
    //     assert_eq!(field_names.len(), fields.len());
    //     for (ix, n) in field_names.iter().enumerate() {
    //         let i: Idx<Expr> = into_idx((ix + 1) as u32);
    //         assert_eq!(i, fields[*n]);
    //     }
    // }
}
