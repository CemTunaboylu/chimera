use core::hash::Hash;
use std::fmt::Debug;

use la_arena::Arena;
use patricia_tree::PatriciaMap;
use smol_str::SmolStr;

use ast::{
    structure::{StructDef as ASTStructDef, StructLiteral as ASTStructLiteral},
    types::Type as ASTType,
};

use crate::{
    HIRResult,
    builder::HIRBuilder,
    climbing::climb,
    errors::HIRError,
    metadata::VarMeta,
    resolution::{Reference, ResolutionType, Unresolved, resolve},
    scope::{
        ExprIdx, MetaHolder, NameIndexed, ScopeIdx, StrIdx, StructDefIdx, StructSelector,
        placeholder_idx,
    },
    typing::hindley_milner::types::Type,
    variable::VarDef,
};

#[derive(Clone, Debug)]
pub struct InternalStructure<TorV: Clone + Debug + Hash + PartialEq + PartialOrd> {
    pub field_name_to_index: PatriciaMap<u32>,
    pub field_names: Arena<SmolStr>,
    pub data: Arena<TorV>,
    pub scope_idx: ScopeIdx,
}
impl<TorV: Clone + Debug + Hash + PartialEq + PartialOrd> PartialEq for InternalStructure<TorV> {
    fn eq(&self, other: &Self) -> bool {
        self.scope_idx == other.scope_idx
            && self.data == other.data
            && self.field_names == other.field_names
            && self
                .field_name_to_index
                .iter()
                .eq(other.field_name_to_index.iter())
    }
}

impl<TorV: Clone + Debug + Hash + PartialEq + PartialOrd> Eq for InternalStructure<TorV> {}
impl<TorV: Clone + Debug + Hash + PartialEq + PartialOrd> Default for InternalStructure<TorV> {
    fn default() -> Self {
        Self {
            field_name_to_index: Default::default(),
            field_names: Default::default(),
            data: Default::default(),
            scope_idx: placeholder_idx(),
        }
    }
}

impl<TorV: Clone + Debug + Hash + PartialEq + PartialOrd> Hash for InternalStructure<TorV> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.field_name_to_index
            .iter()
            .for_each(|tuple| tuple.hash(state));
        self.field_names.hash(state);
        self.data.iter().for_each(|d| d.hash(state));
        self.scope_idx.hash(state);
    }
}

impl<TorV: Clone + Debug + Hash + PartialEq + PartialOrd> PartialOrd for InternalStructure<TorV> {
    fn partial_cmp(&self, _other: &Self) -> Option<std::cmp::Ordering> {
        None
    }
}

impl<TorV: Clone + Debug + Hash + PartialEq + PartialOrd> InternalStructure<TorV> {
    pub fn new(scope_idx: ScopeIdx) -> Self {
        Self {
            scope_idx,
            ..Default::default()
        }
    }
    pub fn add(&mut self, name: SmolStr, data: TorV) -> HIRResult<()> {
        if self.field_names.len() != self.data.len() {
            return Err(HIRError::with_msg(format!(
                "Struct field allocation encountered mismatched indices: 'name index: {:?}' != 'data index: {:?}'",
                self.field_names.len(),
                self.data.len()
            )).into());
        }
        let index = self.field_names.alloc(name.clone());
        self.field_name_to_index
            .insert(name, index.into_raw().into_u32());
        self.data.alloc(data);
        Ok(())
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct StructDef {
    pub internal_with_field_types: InternalStructure<Type>,
    pub name_index: StrIdx,
}

impl Default for StructDef {
    fn default() -> Self {
        Self {
            internal_with_field_types: Default::default(),
            name_index: placeholder_idx(),
        }
    }
}

impl NameIndexed for StructDef {
    fn set_name_index(&mut self, ix: StrIdx) {
        self.name_index = ix;
    }
    fn get_name_index(&self) -> StrIdx {
        self.name_index
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub struct StructRef(pub StructDefIdx);

impl Default for StructRef {
    fn default() -> Self {
        Self(placeholder_idx())
    }
}
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct StructLiteral {
    pub struct_ref: Reference<StructRef>,
    pub internal_with_field_values: InternalStructure<ExprIdx>,
    pub field_metadata: MetaHolder<VarDef, VarMeta>,
}

impl PartialOrd for StructLiteral {
    fn partial_cmp(&self, _: &Self) -> Option<std::cmp::Ordering> {
        None
    }
}

impl Hash for StructLiteral {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.struct_ref.hash(state);
        self.internal_with_field_values.hash(state);
        self.field_metadata
            .iter()
            .for_each(|tuple| tuple.hash(state));
    }
}

impl HIRBuilder {
    pub fn lower_struct_def(&mut self, struct_def: &ASTStructDef) -> HIRResult<StructDefIdx> {
        let name = struct_def.name.clone();
        let mut internal = InternalStructure::<Type>::new(self.current_scope_cursor);

        for field in struct_def.fields.iter() {
            let field_type = self.lower_type(&field.field_type)?;
            let field_name = field.name.clone();
            internal.add(field_name, field_type)?;
        }

        let struct_def = StructDef {
            name_index: placeholder_idx(),
            internal_with_field_types: internal,
        };

        self.allocate::<StructDef, StructSelector>(name, struct_def)
    }
    pub fn lower_struct_ref(&mut self, struct_as_type: &ASTType) -> HIRResult<Unresolved> {
        if let ASTType::Struct(name) = struct_as_type {
            Ok(Unresolved::new(name.clone(), ResolutionType::Struct))
        } else {
            Err(HIRError::with_msg("expects a struct type").into())
        }
    }
    pub fn resolve_struct_ref(
        &mut self,
        unresolved: &Reference<StructDef>,
    ) -> HIRResult<Reference<StructDef>> {
        let scope_climbing_iter = climb(self.current_scope_cursor, &self.scopes);
        resolve::<StructDef, StructSelector>(scope_climbing_iter, unresolved)
    }

    pub fn lower_struct_literal(
        &mut self,
        struct_literal: &ASTStructLiteral,
    ) -> HIRResult<StructLiteral> {
        let unresolved = Unresolved::new(struct_literal.name.clone(), ResolutionType::Struct);
        let unresolved_idx = self.allocate_for_resolution(unresolved);
        let struct_ref = Reference::Unresolved(unresolved_idx);
        let mut internal = InternalStructure::<ExprIdx>::new(self.current_scope_cursor);

        for field in struct_literal.fields.iter() {
            let name = field.name.clone();
            let expr_idx = self.lower_expr_as_idx(&field.value)?;
            internal.add(name, expr_idx)?;
        }

        let struct_literal = StructLiteral {
            field_metadata: MetaHolder::default(),
            internal_with_field_values: internal,
            struct_ref,
        };
        Ok(struct_literal)
    }
}
#[cfg(test)]
mod tests {

    use ast::{cast_node_into_type, structure::StructDef as ASTStructDef};

    use crate::{builder::tests::ast_root_from, scope::into_idx};

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
        let field_name_to_idx = &struct_def.internal_with_field_types.field_name_to_index;
        let internal_field_names = &struct_def.internal_with_field_types.field_names;
        let internal_field_types = &struct_def.internal_with_field_types.data;
        for (n, t) in field_names.iter().zip(field_types.iter()) {
            let idx = field_name_to_idx.get(n).expect("field name to exist");
            assert_eq!(*n, internal_field_names[into_idx(*idx)]);
            assert_eq!(*t, internal_field_types[into_idx(*idx)]);
        }
    }
    #[test]
    fn struct_init() {
        let field_names = ["x", "y", "item"];
        let program = "Tester { x: 0, y: 0, item: origin_item}";
        let ast_root = ast_root_from(program);
        let literal_node = ast_root.get_root().first_child();
        let ast_struct_literal =
            cast_node_into_type::<ASTStructLiteral>(literal_node.as_ref().unwrap());
        let mut hir_builder = HIRBuilder::new(ast_root);
        let struct_literal = hir_builder
            .lower_struct_literal(&ast_struct_literal)
            .expect("should have been ok");
        let unresolved_idx = struct_literal
            .struct_ref
            .get_unresolved_index()
            .expect("should have been unresolved");
        let scope = hir_builder.get_current_scope();
        let unresolved = &scope.to_resolve[unresolved_idx];

        assert_eq!("Tester", unresolved.name);
        assert_eq!(
            field_names.len(),
            struct_literal
                .internal_with_field_values
                .field_name_to_index
                .len()
        );
        assert_eq!(
            field_names.len(),
            struct_literal.internal_with_field_values.field_names.len()
        );
        assert_eq!(
            field_names.len(),
            struct_literal.internal_with_field_values.data.len()
        );
        let field_name_to_idx = &struct_literal
            .internal_with_field_values
            .field_name_to_index;
        let internal_field_names = &struct_literal.internal_with_field_values.field_names;
        let internal_field_expr_ids = &struct_literal.internal_with_field_values.data;
        for n in field_names.iter() {
            let idx = field_name_to_idx.get(n).expect("field name to exist");
            assert_eq!(*n, internal_field_names[into_idx(*idx)]);
            assert_eq!(into_idx(*idx + 1), internal_field_expr_ids[into_idx(*idx)]);
        }
    }
}
