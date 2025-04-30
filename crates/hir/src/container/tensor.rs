use crate::{HIRResult, builder::HIRBuilder, unwrap_or_err};

use super::{canonical::Canonical, uninit::Uninitialized};

use ast::{container::BufferTree, errors::ASTError, literal::Value as ASTValue};

#[derive(Clone, Debug, PartialEq)]
pub enum Tensor {
    Initialized(Canonical),
    Uninitialized(Uninitialized),
}

impl HIRBuilder {
    pub fn lower_tensor_construct(&mut self, tensor_value: &ASTValue) -> HIRResult<Tensor> {
        let buffer_tree = unwrap_or_err(
            tensor_value.get_buffer_tree(),
            "tensor value should have a buffer tree",
        )?;
        let (_meta, _canonical_idx) = self.flatten_buffer_tree(buffer_tree)?;
        Ok(todo!())
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use ast::{ast_root_from, cast_node_into_type};
    use parameterized_test::create;
    use thin_vec::{ThinVec, thin_vec};

    use crate::{expression::Expr, scope::into_idx};

    create! {
        create_tensor_struct_test,
        (program, exp_dims, exp_type, exp_init), {
        let ast_root = ast_root_from(program);
        let tensor_node = ast_root.get_root().first_child().unwrap();
        let ast_tensor_struct = cast_node_into_type::<ASTTensorStruct>(&tensor_node);
        let mut hir_builder = HIRBuilder::new(ast_root);

        // let tensor_struct = hir_builder.lower_tensor_init(&ast_tensor_struct).expect("should have been ok");
        // assert_eq!(exp_dims, tensor_struct.dims.0);
        // assert_eq!(exp_type, tensor_struct.type_hint);
        // assert_eq!(exp_init, tensor_struct.init);
        }
    }

    create_tensor_struct_test! {
        // tensor_struct: ("Tensor<i32><3,3,3>", thin_vec![3;3], Some(Hint::Type(Type::I32)), None),
        // tensor_struct_typed: ("Tensor<i32>", ThinVec::<usize>::new(), Some(Hint::Type(Type::I32)), None),
        // tensor_struct_dim_hinted: ("Tensor<3,3,3>", thin_vec![3;3], None, None),
        // tensor_struct_dim_hinted_init: ("Tensor<3,3,3>[0.0]", thin_vec![3;3], None, Some(TensorInit{default_value: into_idx::<Expr>(1), dim: None})),
        // tensor_struct_type_hinted_full_init: ("Tensor<f32>[1.0;1000]", ThinVec::<usize>::new(), Some(Hint::Type(Type::F32)), Some(TensorInit{default_value: into_idx::<Expr>(1), dim: Some(1000)})),
    }
}
