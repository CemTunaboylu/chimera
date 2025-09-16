use thin_vec::{ThinVec, thin_vec};

use ast::{
    collection::{BufferTree as ASTBufferTree, buffer_tree_from},
    expression::Expr as ASTExpr,
};

use crate::{
    HIRResult,
    builder::HIRBuilder,
    clone_from_iter_with_err,
    collection::{ScopedCanonicalLiteralIdx, storage::ScopedStorageIdx},
    literal::Value,
    metadata::Common,
    purity::Purity,
    scope::{ExprIdx, Scoped},
    typing::hindley_milner::types::{Maybe, Type},
};

use super::{
    CanonicalLiteralIdx,
    layout::Layout,
    meta::{CollectionExamination, CollectionMeta},
    shape::{Shape, ShapeFormer},
    storage::Storage,
};

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
/// CanonicalBuffer is the enclosing structure that holds the
/// flattened buffer (from shape [d_1, d_2, ...] to shape [d_1*d_2*..])
/// that is stored in the memory acc. to the provided Layout,
pub struct CanonicalBuffer {
    // data: Cow<'arena, [PrimitiveValue]>,
    pub data: Storage,
    pub data_type: Maybe<Type>,
    pub layout: Layout,
    // TODO: put metadata in an arena and have the idx here
    pub metadata: CollectionMeta,
    pub shape: Shape,
}

impl CanonicalBuffer {
    pub fn new(
        flattened: Storage,
        layout_f: fn(&Shape) -> Layout,
        metadata: &CollectionMeta,
    ) -> Self {
        let shape = metadata.shape.clone();
        // let data_type = metadata.data_type.clone();
        let data_type = Maybe::Checked(Box::new(Type::I32)); // TODO: FIX ME: for now a dummy default value to make tests pass

        let layout = layout_f(&shape);
        CanonicalBuffer {
            data: flattened,
            data_type,
            layout,
            metadata: metadata.clone(),
            shape,
        }
    }
    /// change_layout only sets the CanonicalBuffer layout lazily. We avoid runtime memory layout changes
    /// and aim for fusing layout changes with operations. A separate pass has a heuristic to determine the
    /// best layout for the buffer depending on affinities of operations to certain layouts and frequency of
    /// those operations. The main approach nonetheless is tiled thus, almost always the buffer will be stored
    /// in a block layout that is layed out in a row-major internally with the block size compatible and fitting
    /// to the caches and SIMD vector registers.
    pub fn change_layout(&mut self, layout_f: fn(&Shape) -> Layout) {
        let new_layout = layout_f(&self.shape);
        self.layout = new_layout;
    }
    // ! obsolete or private
    pub fn apply_layout(&mut self) {
        todo!()
    }
}

impl HIRBuilder {
    pub fn flatten_buffer_tree(
        &mut self,
        tensor_tree: &ASTBufferTree,
    ) -> HIRResult<(CollectionMeta, Storage)> {
        let mut flattened: ThinVec<ExprIdx> = thin_vec![];
        let to_stack = tensor_tree
            .sub_tree()
            .or(tensor_tree.values())
            .expect("should have been ok");
        let len = to_stack.len();
        let mut shape_former = ShapeFormer::new(len);
        let mut stack =
            clone_from_iter_with_err(to_stack.into_iter().rev(), len, |e: ASTExpr| Ok((1, e)))?;

        let mut collection_examination = CollectionExamination::new(self.get_current_scope_idx());

        'outer: while !stack.is_empty() {
            let mut ast_expr = {
                let (d, ae) = stack.pop().unwrap();
                shape_former.new_depth(d);
                ae
            };
            while let Some(tree) = buffer_tree_from(&ast_expr) {
                if let Some(mut sub_tree) = tree.sub_tree() {
                    shape_former.push_dim_for_shape(sub_tree.len());
                    sub_tree.reverse();
                    ast_expr = sub_tree.pop().expect("expected a buffer tree");
                    shape_former.deepen();
                    stack.reserve(sub_tree.len());
                    let depth = shape_former.get_depth();
                    for s in sub_tree {
                        stack.push((depth, s));
                    }
                } else if let Some(values) = tree.values() {
                    shape_former.push_dim_for_shape(values.len());
                    for expr in values {
                        let idx_for_value = self.lower_expr_as_idx(&expr)?;
                        collection_examination.add_with_id(idx_for_value.elm)?;
                        flattened.push(idx_for_value.elm);
                    }
                    continue 'outer;
                }
            }
            let idx_for_value = self.lower_expr_as_idx(&ast_expr)?;
            collection_examination.add_with_id(idx_for_value.elm)?;
            flattened.push(idx_for_value.elm);
        }

        let common = Common {
            purity: Purity::Pure,
            refs_as_stmt_indices: thin_vec![],
        };
        let ten_meta = CollectionMeta::with(common, collection_examination, shape_former.form());
        Ok((ten_meta, Storage::Indexed(flattened)))
    }

    pub fn get_canonical_collection_with(
        &self,
        idx: &ScopedCanonicalLiteralIdx,
    ) -> Option<&CanonicalBuffer> {
        let scope = &self.scopes[idx.scope_idx];
        Some(&scope.tensor_literals[idx.elm])
    }
}

#[cfg(test)]
mod tests {
    use std::ops::Range;

    use parameterized_test::create;
    use thin_vec::ThinVec;

    use super::*;
    use ast::{ast_root_from_assert_no_err, cast_node_into_type, literal::Literal as ASTLiteral};

    use crate::{collection::Shape, metadata::Common};
    use crate::{literal::Value, scope::into_idx};

    fn get_tensor_literal_for(program: &str) -> CanonicalBuffer {
        let ast_root = ast_root_from_assert_no_err(program);
        let literal_node = ast_root.get_root().first_child().unwrap();
        let ast_literal = cast_node_into_type::<ASTLiteral>(&literal_node);
        let mut hir_builder = HIRBuilder::new(ast_root);
        _ = hir_builder
            .lower_literal(&ast_literal)
            .expect("should have been ok");
        let idx = into_idx::<CanonicalBuffer>(0);
        hir_builder.get_current_scope().tensor_literals[idx].clone()
    }

    create! {
        tensor_literal_happy_path_test,
        (program, exp_shape, exp_metadata, exp_data), {
            let tensor_literal = get_tensor_literal_for(program);
            assert_eq!(exp_shape, tensor_literal.shape);
            assert_eq!(exp_metadata, tensor_literal.metadata);
            assert_eq!(exp_data, tensor_literal.data);
        }
    }

    fn test_metadata(max: Value, is_sparse: bool) -> CollectionMeta {
        let dtype: Type = Type::from(&max);
        let shape = Shape::Buffer(thin_vec![3, 3, 3]);
        CollectionMeta {
            common: Common {
                purity: Purity::Pure,
                refs_as_stmt_indices: thin_vec![],
            },
            sparse: is_sparse,
            layout: Layout::row_major(&shape),
            shape,
            is_allocated: true,
            group_id: 0,
        }
    }

    fn i_val(i: i32) -> Value {
        Value::Int(i)
    }

    #[test]
    fn test_canonical_equality() {
        let square_tensor_2d = "[[1,0,0],[0,1,0],[0,0,1]]";
        let non_square_tensor_2d = "[[1,0],[0],[0],[1],[0],[0,0,1]]";
        let square_tensor_literal = get_tensor_literal_for(square_tensor_2d);
        let non_square_tensor_literal = get_tensor_literal_for(non_square_tensor_2d);
        assert_eq!(square_tensor_literal.data, non_square_tensor_literal.data);
    }

    // tensor_literal_happy_path_test! {
    //     tensor_2d_literal: ("[[1,0,0],[0,1,0],[0,0,1]]", Shape::Buffer(thin_vec![3,3]), test_metadata(Value::Int(1), true), thin_vec![into_idx(1), into_idx(2),into_idx(2), into_idx(2),into_idx(1), into_idx(2), into_idx(2), into_idx(2), into_idx(1)]),
    //     tensor_3d_literal: ("[[[1,0,0],[0,1,0],[0,0,1]], [[1,0,0],[0,1,0],[0,0,1]], [[1,0,0],[0,1,0],[0,0,2]] ]", Shape::Buffer(thin_vec![3,3,3]), test_metadata(Value::Int(2), true), thin_vec![into_idx(1), into_idx(2),into_idx(2), into_idx(2),into_idx(1), into_idx(2), into_idx(2), into_idx(2), into_idx(1),into_idx(1), into_idx(2),into_idx(2), into_idx(2),into_idx(1), into_idx(2), into_idx(2), into_idx(2), into_idx(1),into_idx(1), into_idx(2),into_idx(2), into_idx(2),into_idx(1), into_idx(2), into_idx(2), into_idx(2), into_idx(3)]),
    //     tensor_3d_literal_non_square: ("[[[1,0],[1,1],[0,1]], [[1,0],[1,1],[0,1]], [[1,0],[1,1],[0,1]] ]", Shape::Buffer(thin_vec![3,3,2]), test_metadata(Value::Int(1), true), thin_vec![into_idx(1), into_idx(2),into_idx(1), into_idx(1), into_idx(2), into_idx(1),into_idx(1), into_idx(2), into_idx(1),into_idx(1), into_idx(2), into_idx(1),into_idx(1), into_idx(2),into_idx(1), into_idx(1),into_idx(2), into_idx(1)]),
    // }
}
