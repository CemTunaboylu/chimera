use ast::literal::{Literal as ASTLiteral, TensorTree as ASTTensorTree, Value as ASTValue};

use rust_decimal::Decimal;
use thin_vec::{ThinVec, thin_vec};

use crate::{
    HIRResult,
    builder::HIRBuilder,
    metadata::{MinMax, Sparsity, TenMeta, TypeCheck},
    scope::{StrIdx, TensorLiteralIdx},
    tensor::{CanonicalTensor, Layout},
    types::Type,
    unwrap_or_err,
};

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub enum Value {
    Bool(bool),
    Char(char),
    Float(Decimal),
    Int(i32),
    Str(StrIdx),
    Tensor(TensorLiteralIdx),
}

impl From<&ASTValue> for Value {
    fn from(value: &ASTValue) -> Self {
        match value {
            ASTValue::Bool(b) => Value::Bool(*b),
            ASTValue::Char(c) => Value::Char(*c),
            ASTValue::Float(f) => Value::Float(Decimal::from_f32_retain(*f).unwrap()),
            ASTValue::Int(i) => Value::Int(*i),
            ASTValue::Str(_) => unreachable!(),
            ASTValue::Tensor(_) => unreachable!(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Literal(Value);

impl HIRBuilder {
    fn flatten_tensor_tree(
        tensor_tree: ASTTensorTree,
    ) -> HIRResult<(ThinVec<usize>, Type, TenMeta, ThinVec<Value>)> {
        let shape = tensor_tree.shape().0;
        let mut flattened: ThinVec<Value> = thin_vec![];
        let mut stack = thin_vec![tensor_tree];

        let mut sparsity = Sparsity::new();
        let mut type_check = TypeCheck::new();
        let mut min_max = MinMax::new();

        'outer: while !stack.is_empty() {
            let mut node = stack.pop().unwrap();
            while matches!(node, ASTTensorTree::Node(_, _)) {
                let mut sub_tree = node.sub_tree();
                if sub_tree.is_empty() {
                    continue 'outer;
                };
                let mut to_stack = sub_tree.split_off(1);
                to_stack.reverse();
                stack.extend(to_stack);
                node = sub_tree.first().unwrap().clone();
            }
            let ast_values = unwrap_or_err(node.values(), "expected a leaf")?;
            for ast_value in ast_values {
                let value = Value::from(ast_value);
                sparsity.count(&value);
                type_check.add(&value);
                min_max.min_max(&value);
                flattened.push(value);
            }
        }

        _ = type_check.type_check()?;
        let data_type = type_check.get_only_type();
        let ten_meta = TenMeta::with(sparsity, min_max);

        Ok((shape, data_type, ten_meta, flattened))
    }
    pub fn lower_literal(&mut self, literal: &ASTLiteral) -> HIRResult<Literal> {
        let value = match literal.value() {
            ASTValue::Str(string) => {
                let idx = self.allocate_string(string);
                Value::Str(idx)
            }
            ASTValue::Tensor(tensor_tree) => {
                let (shape, data_type, ten_meta, flattened) =
                    Self::flatten_tensor_tree(tensor_tree)?;
                let tensor_literal =
                    CanonicalTensor::new(data_type, flattened, Layout::row_major, ten_meta, shape);
                let tensor_idx = self.allocate_tensor_literal(tensor_literal);
                Value::Tensor(tensor_idx)
            }
            primitive => Value::from(&primitive),
        };
        Ok(Literal(value))
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::tensor::Shape;
    use ast::{ast_root_from, cast_node_into_type};
    use parameterized_test::create;

    use crate::scope::into_idx;

    fn get_tensor_literal_for(program: &str) -> CanonicalTensor {
        let ast_root = ast_root_from(program);
        let literal_node = ast_root.get_root().first_child().unwrap();
        let ast_literal = cast_node_into_type::<ASTLiteral>(&literal_node);
        let mut hir_builder = HIRBuilder::new(ast_root);
        let _ = hir_builder
            .lower_literal(&ast_literal)
            .expect("should have been ok");
        let idx = into_idx::<CanonicalTensor>(0);
        hir_builder.get_current_scope().tensor_literals[idx].clone()
    }

    create! {
        tensor_literal_happy_path_test,
        (program, exp_shape, exp_metadata, exp_data), {
            let tensor_literal = get_tensor_literal_for(program);
            assert_eq!(Shape(ThinVec::from(exp_shape)), tensor_literal.shape);
            assert_eq!(exp_metadata, tensor_literal.metadata);
            assert_eq!(exp_data, tensor_literal.data);
        }
    }

    fn test_metadata(max: Value, is_sparse: bool) -> TenMeta {
        TenMeta {
            max: Some(max),
            min: Some(Value::Int(0)),
            sparse: is_sparse,
            num_refs: 0,
            op_log: thin_vec![],
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
        let data = thin_vec![
            i_val(1),
            i_val(0),
            i_val(0),
            i_val(0),
            i_val(1),
            i_val(0),
            i_val(0),
            i_val(0),
            i_val(1),
            i_val(1),
            i_val(0),
            i_val(0),
            i_val(0),
            i_val(1),
            i_val(0),
            i_val(0),
            i_val(0),
            i_val(1),
            i_val(1),
            i_val(0),
            i_val(0),
            i_val(0),
            i_val(1),
            i_val(0),
            i_val(0),
            i_val(0),
            i_val(2)
        ];
        assert_eq!(data, tensor_literal.data);

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
        let data = thin_vec![
            i_val(0),
            i_val(0),
            i_val(1),
            i_val(1),
            i_val(0),
            i_val(0),
            i_val(1),
            i_val(1),
            i_val(2),
            i_val(2),
            i_val(3),
            i_val(3),
            i_val(2),
            i_val(2),
            i_val(3),
            i_val(3),
            i_val(0),
            i_val(0),
            i_val(1),
            i_val(1),
            i_val(0),
            i_val(0),
            i_val(1),
            i_val(1),
            i_val(2),
            i_val(2),
            i_val(3),
            i_val(3),
            i_val(2),
            i_val(2),
            i_val(3),
            i_val(3),
        ];
        assert_eq!(data, tensor_literal.data);

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

    tensor_literal_happy_path_test! {
        tensor_2d_literal: ("[[1,0,0],[0,1,0],[0,0,1]]", [3,3], test_metadata(Value::Int(1), true), thin_vec![i_val(1), i_val(0),i_val(0), i_val(0),i_val(1), i_val(0), i_val(0), i_val(0), i_val(1)]),
        tensor_3d_literal: ("[[[1,0,0],[0,1,0],[0,0,1]], [[1,0,0],[0,1,0],[0,0,1]], [[1,0,0],[0,1,0],[0,0,2]] ]", [3,3,3], test_metadata(Value::Int(2), true), thin_vec![i_val(1), i_val(0),i_val(0), i_val(0),i_val(1), i_val(0), i_val(0), i_val(0), i_val(1),i_val(1), i_val(0),i_val(0), i_val(0),i_val(1), i_val(0), i_val(0), i_val(0), i_val(1),i_val(1), i_val(0),i_val(0), i_val(0),i_val(1), i_val(0), i_val(0), i_val(0), i_val(2)]),
        tensor_3d_literal_non_square: ("[[[1,0],[1,1],[0,1]], [[1,0],[1,1],[0,1]], [[1,0],[1,1],[0,1]] ]", [3,3,2], test_metadata(Value::Int(1), true), thin_vec![i_val(1), i_val(0),i_val(1), i_val(1), i_val(0), i_val(1),i_val(1), i_val(0), i_val(1),i_val(1), i_val(0), i_val(1),i_val(1), i_val(0),i_val(1), i_val(1),i_val(0), i_val(1)]),
    }
}
