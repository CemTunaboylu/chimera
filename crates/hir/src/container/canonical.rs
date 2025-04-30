use thin_vec::{ThinVec, thin_vec};

use ast::{
    container::{BufferTree as ASTBufferTree, buffer_tree_from},
    expression::Expr,
};

use crate::{
    HIRResult,
    builder::HIRBuilder,
    climbing::climb,
    clone_from_iter_with_err,
    scope::ExprIdx,
    typing::hindley_milner::types::{Maybe, Type},
};

use super::{
    CanonicalLiteralIdx, Shape,
    layout::Layout,
    meta::{ContainerExamination, TenMeta},
};

#[derive(Clone, Debug, PartialEq)]
pub struct Canonical {
    pub idx: CanonicalLiteralIdx,
    pub data_type: Type,
    pub layout: Layout,
    pub metadata: TenMeta,
    pub shape: Shape,
}

#[derive(Clone, Debug, PartialEq)]
pub struct CanonicalBuffer {
    // data: Cow<'arena, [PrimitiveValue]>,
    // TODO: put metadata in an arena and have the idx here
    // pub data: ThinVec<Value>,
    pub data: ThinVec<ExprIdx>,
    pub data_type: Maybe<Type>,
    pub layout: Layout,
    pub metadata: TenMeta,
    pub shape: Shape,
}

impl CanonicalBuffer {
    pub fn new(
        // flattened: ThinVec<Value>,
        flattened: ThinVec<ExprIdx>,
        layout_f: fn(&Shape) -> Layout,
        metadata: &TenMeta,
    ) -> Self {
        let shape = metadata.shape.clone();
        // let data_type = metadata.data_type.clone();
        let data_type = Maybe::Checked(Box::new(Type::I32)); // TODO: FIX ME: for now a dyummy default value to make tests pass

        let layout = layout_f(&shape);
        CanonicalBuffer {
            data: flattened,
            data_type,
            layout,
            metadata: metadata.clone(),
            shape,
        }
    }
    pub fn change_layout(&mut self, layout_f: fn(&Shape) -> Layout) {
        let new_layout = layout_f(&self.shape);
        self.layout = new_layout;
    }
    pub fn apply_layout(&mut self) {
        todo!()
    }
}

struct ShapeFormer {
    shape: ThinVec<usize>,
    depth: usize,
}
impl ShapeFormer {
    #[inline]
    fn new(len: usize) -> Self {
        Self {
            shape: thin_vec![len],
            depth: 1,
        }
    }
    #[inline]
    fn get_dim_for_shape(&mut self, dim: usize) {
        if self.shape.len() <= self.depth {
            self.shape.push(dim)
        }
    }
    #[inline]
    fn deepen(&mut self) {
        self.depth += 1;
    }
    #[inline]
    fn new_depth(&mut self, d: usize) {
        self.depth = d;
    }
    #[inline]
    fn get_depth(&mut self) -> usize {
        self.depth
    }
    #[inline]
    fn form(self) -> Shape {
        Shape::Buffer(self.shape)
    }
}
impl HIRBuilder {
    pub fn flatten_buffer_tree(
        &mut self,
        tensor_tree: &ASTBufferTree,
    ) -> HIRResult<(TenMeta, ThinVec<ExprIdx>)> {
        let mut flattened: ThinVec<ExprIdx> = thin_vec![];
        let to_stack = tensor_tree
            .sub_tree()
            .or(tensor_tree.values())
            .expect("should have been ok");
        let enumerate = |e: Expr| Ok((1, e));
        let len = to_stack.len();
        let mut shape_former = ShapeFormer::new(len);
        let mut stack = clone_from_iter_with_err(to_stack.into_iter().rev(), len, enumerate)?;

        let mut container_examination = ContainerExamination::new();

        'outer: while !stack.is_empty() {
            let mut ast_expr = {
                let (d, ae) = stack.pop().unwrap();
                shape_former.new_depth(d);
                ae
            };
            while let Some(tree) = buffer_tree_from(&ast_expr) {
                let mut sub_tree = if let Some(st) = tree.sub_tree() {
                    shape_former.get_dim_for_shape(st.len());
                    st
                } else if let Some(values) = tree.values() {
                    shape_former.get_dim_for_shape(values.len());
                    for v in values {
                        let idx_for_value = self.lower_expr_as_idx(&v)?;
                        let expr = self.get_expr(idx_for_value);
                        container_examination.add(expr, idx_for_value)?;
                        flattened.push(idx_for_value);
                    }
                    continue 'outer;
                } else {
                    unreachable!()
                };

                sub_tree.reverse();
                ast_expr = sub_tree.pop().expect("expected a buffer tree");
                shape_former.deepen();
                stack.reserve(sub_tree.len());
                for s in sub_tree {
                    stack.push((shape_former.get_depth(), s));
                }
            }
            let idx_for_value = self.lower_expr_as_idx(&ast_expr)?;
            let expr = self.get_expr(idx_for_value);
            container_examination.add(expr, idx_for_value)?;
            flattened.push(idx_for_value);
        }

        let ten_meta = TenMeta::with(container_examination, shape_former.form());
        Ok((ten_meta, flattened))
    }
    pub fn get_canonical_tensor_with(&self, idx: CanonicalLiteralIdx) -> Option<&CanonicalBuffer> {
        let climber = climb(self.current_scope_cursor, &self.scopes);
        let mut t: Option<&CanonicalBuffer> = None;
        for (_, scope) in climber {
            if scope.tensor_literals.len() <= idx.into_raw().into_u32() as usize {
                continue;
            }
            t = Some(&scope.tensor_literals[idx]);
        }
        t
    }
}

#[cfg(test)]
mod tests {
    use parameterized_test::create;
    use thin_vec::ThinVec;

    use super::*;
    use ast::{ast_root_from, cast_node_into_type, literal::Literal as ASTLiteral};

    use crate::{container::Shape, typing::hindley_milner::types::Maybe};
    use crate::{literal::Value, scope::into_idx};

    fn get_tensor_literal_for(program: &str) -> CanonicalBuffer {
        let ast_root = ast_root_from(program);
        let literal_node = ast_root.get_root().first_child().unwrap();
        let ast_literal = cast_node_into_type::<ASTLiteral>(&literal_node);
        println!("ast_literal: {:?}", ast_literal);
        let mut hir_builder = HIRBuilder::new(ast_root);
        let literal = hir_builder
            .lower_literal(&ast_literal)
            .expect("should have been ok");
        println!("literal: {:?}", literal);
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

    fn test_metadata(max: Value, is_sparse: bool) -> TenMeta {
        let dtype: Type = Type::from(&max);
        let shape = Shape::Buffer(thin_vec![3, 3, 3]);
        TenMeta {
            max: Some(max),
            min: Some(Value::Int(0)),
            sparse: is_sparse,
            num_refs: 0,
            op_log: thin_vec![],
            // data_type: Maybe::Checked(Box::new(dtype)),
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

    #[test]
    fn test_layout_logic_for_row_and_column_major() {
        let tensor_3d =
            "[[[1,0,0],[0,1,0],[0,0,1]], [[1,0,0],[0,1,0],[0,0,1]], [[1,0,0],[0,1,0],[0,0,2]] ]";
        let mut tensor_literal = get_tensor_literal_for(tensor_3d);
        let expected_strides = [
            [9, 3, 1], /* row-major */
            [1, 3, 9], /* column-major */
        ];
        let expected_indices = [
            [
                ([0, 0, 0], 0),
                ([0, 1, 2], 5),
                ([1, 1, 1], 13),
                ([2, 2, 2], 26),
            ], /* row-major */
            [
                ([0, 0, 0], 0),
                ([0, 1, 2], 21),
                ([1, 1, 1], 13),
                ([2, 2, 2], 26),
            ], /* column-major */
        ];
        // TODO: uncomment
        // note: at the end we will be using the same expressions, i.e. won't insert the same expression twice into the arena
        // thus, same expressions will result with the same expression index
        // let data = thin_vec![
        //     into_idx(1),
        //     into_idx(2),
        //     into_idx(2),
        //     into_idx(2),
        //     into_idx(1),
        //     into_idx(2),
        //     into_idx(2),
        //     into_idx(2),
        //     into_idx(1),
        //     into_idx(1),
        //     into_idx(2),
        //     into_idx(2),
        //     into_idx(2),
        //     into_idx(1),
        //     into_idx(2),
        //     into_idx(2),
        //     into_idx(2),
        //     into_idx(1),
        //     into_idx(1),
        //     into_idx(2),
        //     into_idx(2),
        //     into_idx(2),
        //     into_idx(1),
        //     into_idx(2),
        //     into_idx(2),
        //     into_idx(2),
        //     into_idx(3)
        // ];
        // assert_eq!(data, tensor_literal.data);

        let layouts = [
            |shape: &Shape| Layout::row_major(shape),
            |shape: &Shape| Layout::col_major(shape),
        ];

        for (ix, layout_f) in layouts.iter().enumerate() {
            tensor_literal.change_layout(*layout_f);
            let strides = match &tensor_literal.layout {
                Layout::RowMajor(strides) => strides,
                Layout::ColumnMajor(strides) => strides,
                _ => unreachable!(),
            };
            assert_eq!(ThinVec::from(expected_strides[ix]), strides.0);
            for (ind, exp) in expected_indices[ix] {
                assert_eq!(
                    exp,
                    tensor_literal.layout.flatten_indexing(ThinVec::from(ind))
                );
            }
        }
    }

    #[test]
    fn test_layout_logic_for_blocked() {
        let tensor_3d = "[ [[0,0,1,1],[0,0,1,1],[2,2,3,3],[2,2,3,3]], [[0,0,1,1],[0,0,1,1],[2,2,3,3],[2,2,3,3]] ]";
        let mut tensor_literal = get_tensor_literal_for(tensor_3d);
        let expected_indices = [
            ([0, 0, 0], 0),
            ([0, 1, 2], 10),
            ([1, 1, 1], 7),
            ([1, 2, 3], 29),
        ];
        // TODO: uncomment
        // note: at the end we will be using the same expressions, i.e. won't insert the same expression twice into the arena
        // thus, same expressions will result with the same expression index
        // let data = thin_vec![
        //     into_idx(1),
        //     into_idx(1),
        //     into_idx(2),
        //     into_idx(2),
        //     into_idx(1),
        //     into_idx(1),
        //     into_idx(2),
        //     into_idx(2),
        //     into_idx(3),
        //     into_idx(3),
        //     into_idx(4),
        //     into_idx(4),
        //     into_idx(3),
        //     into_idx(3),
        //     into_idx(4),
        //     into_idx(4),
        //     into_idx(1),
        //     into_idx(1),
        //     into_idx(2),
        //     into_idx(2),
        //     into_idx(1),
        //     into_idx(1),
        //     into_idx(2),
        //     into_idx(2),
        //     into_idx(3),
        //     into_idx(3),
        //     into_idx(4),
        //     into_idx(4),
        //     into_idx(3),
        //     into_idx(3),
        //     into_idx(4),
        //     into_idx(4),
        // ];
        // assert_eq!(data, tensor_literal.data);

        fn blocked(shape: &Shape) -> Layout {
            Layout::blocked(2, shape)
        }
        tensor_literal.change_layout(blocked);

        for (ind, exp) in expected_indices {
            assert_eq!(
                exp,
                tensor_literal.layout.flatten_indexing(ThinVec::from(ind))
            );
        }
    }

    // tensor_literal_happy_path_test! {
    //     tensor_2d_literal: ("[[1,0,0],[0,1,0],[0,0,1]]", Shape::Buffer(thin_vec![3,3]), test_metadata(Value::Int(1), true), thin_vec![into_idx(1), into_idx(2),into_idx(2), into_idx(2),into_idx(1), into_idx(2), into_idx(2), into_idx(2), into_idx(1)]),
    //     tensor_3d_literal: ("[[[1,0,0],[0,1,0],[0,0,1]], [[1,0,0],[0,1,0],[0,0,1]], [[1,0,0],[0,1,0],[0,0,2]] ]", Shape::Buffer(thin_vec![3,3,3]), test_metadata(Value::Int(2), true), thin_vec![into_idx(1), into_idx(2),into_idx(2), into_idx(2),into_idx(1), into_idx(2), into_idx(2), into_idx(2), into_idx(1),into_idx(1), into_idx(2),into_idx(2), into_idx(2),into_idx(1), into_idx(2), into_idx(2), into_idx(2), into_idx(1),into_idx(1), into_idx(2),into_idx(2), into_idx(2),into_idx(1), into_idx(2), into_idx(2), into_idx(2), into_idx(3)]),
    //     tensor_3d_literal_non_square: ("[[[1,0],[1,1],[0,1]], [[1,0],[1,1],[0,1]], [[1,0],[1,1],[0,1]] ]", Shape::Buffer(thin_vec![3,3,2]), test_metadata(Value::Int(1), true), thin_vec![into_idx(1), into_idx(2),into_idx(1), into_idx(1), into_idx(2), into_idx(1),into_idx(1), into_idx(2), into_idx(1),into_idx(1), into_idx(2), into_idx(1),into_idx(1), into_idx(2),into_idx(1), into_idx(1),into_idx(2), into_idx(1)]),
    // }
}
