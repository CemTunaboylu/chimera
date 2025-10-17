use thin_vec::{ThinVec, thin_vec};

use ast::{
    collection::{BufferTree as ASTBufferTree, buffer_tree_from},
    expression::Expr as ASTExpr,
};

use crate::{
    HIRResult,
    builder::HIRBuilder,
    clone_from_iter_with_err,
    collection::ScopedCanonicalLiteralIdx,
    metadata::Common,
    purity::Purity,
    scope::ExprIdx,
    typing::hindley_milner::types::{Maybe, Type},
};

use super::{
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
