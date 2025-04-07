use la_arena::Arena;
use smol_str::SmolStr;

use ast::{
    structure::{StructDef as ASTStructDef, StructField as ASTStructField},
    types::Type as ASTType,
};

use crate::{
    HIRResult,
    builder::HIRBuilder,
    errors::HIRError,
    expression::MISSING,
    resolution::{ResolutionType, Unresolved},
    scope::{ExprIdx, NameToIndexTrie, ScopeIdx, StrIdx, StructDefIdx, into_idx},
    types::Type,
};

#[derive(Clone, Debug)]
pub struct StructDef {
    pub name_index: StrIdx,
    pub scope_idx: ScopeIdx,
    pub fields: Arena<StructField>,
    pub field_name_to_arena: NameToIndexTrie<StructField>,
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
    pub value: ExprIdx,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct StructRef(pub StructDefIdx);

impl HIRBuilder {
    fn lower_struct_field(
        &mut self,
        field: &ASTStructField,
        into_field_name_to_arena: &mut NameToIndexTrie<StructField>,
        into_fields: &mut Arena<StructField>,
        missing: ExprIdx,
    ) -> HIRResult<()> {
        let field_type = self.lower_type(&field.field_type)?;
        let lowered_field = StructField {
            name: field.name.clone(),
            field_type,
            value: missing,
        };

        let idx = into_fields.alloc(lowered_field);
        into_field_name_to_arena.insert(&field.name, idx);

        Ok(())
    }
    pub fn lower_struct_def(&mut self, struct_def: &ASTStructDef) -> HIRResult<StructDefIdx> {
        let name = struct_def.name.clone();
        let scope_idx = self.current_scope_cursor;
        let name_index = {
            let current_scope = self.get_current_scope_mut();
            current_scope.struct_names.alloc(name.clone())
        };

        let mut field_name_to_arena = NameToIndexTrie::<StructField>::new();
        let mut fields = Arena::<StructField>::with_capacity(struct_def.fields.len());
        let missing: ExprIdx = into_idx(MISSING);

        for field in struct_def.fields.iter() {
            self.lower_struct_field(field, &mut field_name_to_arena, &mut fields, missing)?;
        }

        let struct_def = StructDef {
            name_index,
            scope_idx,
            fields,
            field_name_to_arena,
        };

        let idx = {
            let current_scope = self.get_current_scope_mut();
            current_scope.allocate_struct_def_with_name(&name, struct_def)
        };
        Ok(idx)
    }
    pub fn lower_struct_ref(&mut self, struct_as_type: &ASTType) -> HIRResult<Unresolved> {
        if let ASTType::Struct(name) = struct_as_type {
            Ok(Unresolved::new(name.clone(), ResolutionType::Struct))
        } else {
            Err(HIRError::with_msg(format!("expects a struct type")).into())
        }
    }
}
#[cfg(test)]
mod tests {

    use ast::{cast_node_into_type, structure::StructDef as ASTStructDef};
    use la_arena::Idx;

    use crate::{builder::tests::ast_root_from, resolution::Reference};

    use super::*;

    #[test]
    fn struct_def() {
        let field_names = ["x", "y", "item"];
        let field_types = [
            Type::Integer32,
            Type::Integer32,
            // Type::Struct(SmolStr::new("Item")),
            Type::Struct(Reference::Unresolved(into_idx(0))),
        ];
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
        let struct_def = &scope.struct_defs[idx];

        assert_eq!("Tester", scope.struct_names[struct_def.name_index]);
        let fields = &struct_def.fields;
        let missing: ExprIdx = into_idx(MISSING);
        for (ix, (n, t)) in field_names.iter().zip(field_types.iter()).enumerate() {
            let i: Idx<StructField> = into_idx(ix as u32);
            assert_eq!(*n, fields[i].name);
            assert_eq!(*t, fields[i].field_type);
            assert_eq!(missing, fields[i].value);
        }
    }
}
