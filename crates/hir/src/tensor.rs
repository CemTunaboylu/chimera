use std::collections::HashMap;

use thin_vec::{ThinVec, thin_vec};

use ast::tensor::{Hint as ASTHint, TensorInit as ASTTensorInit, TensorStruct as ASTTensorStruct};

use crate::{
    HIRResult, builder::HIRBuilder, literal::Value, metadata::TenMeta, scope::ExprIdx, types::Type,
};
#[derive(Clone, Debug, PartialEq)]
pub enum TensorOp {
    Transpose,
    Reshape(ThinVec<usize>),
    Max,
    Min,
    Sum,
    Scale(Value),
    Add(Value),
    Mul,
    MatMul,
}
/// Represents a dense block in a block-sparse tensor.
struct DenseBlock<T> {
    // Flattened dense data for the block.
    data: Vec<T>,
}

/// Coordinates identifying a block's position.
type BlockIndex = Vec<usize>;

/// The block-sparse tensor structure.
struct BlockSparseTensor<T> {
    // Overall shape of the tensor.
    shape: Vec<usize>,
    // Size of each block.
    block_shape: Vec<usize>,
    // Mapping from block coordinates to the dense block.
    blocks: HashMap<BlockIndex, DenseBlock<T>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Strides(pub ThinVec<usize>);

#[derive(Clone, Debug, PartialEq)]
pub enum Layout {
    RowMajor(Strides),
    ColumnMajor(Strides),
    Blocked {
        block_size: usize,
        block_span: usize,
        num_blocks_in_shape: ThinVec<usize>,
        outer_strides: Strides,
        inner_strides: Strides,
    }, // for tiled access
}

impl Layout {
    pub fn row_major(shape: &Shape) -> Self {
        let length = shape.0.len();
        let mut strides = thin_vec![1; length];

        for rev_ix in (0..length - 1).rev() {
            strides[rev_ix] = strides[rev_ix + 1] * shape.0[rev_ix + 1];
        }
        Self::RowMajor(Strides(strides))
    }
    pub fn col_major(shape: &Shape) -> Self {
        let length = shape.0.len();
        let mut strides = thin_vec![1; length];

        for ix in 1..length {
            strides[ix] = strides[ix - 1] * shape.0[ix - 1];
        }
        Self::ColumnMajor(Strides(strides))
    }
    pub fn blocked(block_size: usize, shape: &Shape) -> Self {
        let dimensionality = shape.0.len();
        let block_span = block_size.pow(dimensionality as u32);
        let mut num_blocks_in_shape: ThinVec<usize> =
            shape.0.iter().map(|e| e / block_size).collect();
        let mut outer_strides = thin_vec![block_span; dimensionality];
        let mut inner_strides = thin_vec![1; dimensionality];

        for ix in (0..num_blocks_in_shape.len() - 1).rev() {
            num_blocks_in_shape[ix] = num_blocks_in_shape[ix + 1] * num_blocks_in_shape[ix];
            outer_strides[ix] *= num_blocks_in_shape[ix + 1];
            inner_strides[ix] = inner_strides[ix + 1] * block_size;
        }

        let outer_strides = Strides(outer_strides);
        let inner_strides = Strides(inner_strides);

        Self::Blocked {
            block_size,
            block_span,
            num_blocks_in_shape,
            outer_strides,
            inner_strides,
        }
    }
    pub fn flatten_indexing(&self, indices: ThinVec<usize>) -> usize {
        let strides = match self {
            Layout::RowMajor(strides) => strides,
            Layout::ColumnMajor(strides) => strides,
            Layout::Blocked {
                block_size,
                block_span: _,
                num_blocks_in_shape: _,
                outer_strides,
                inner_strides,
            } => {
                let block_offsets_along_dims = indices
                    .iter()
                    .map(|i| *i / block_size)
                    .collect::<ThinVec<usize>>();

                let inner_offsets_along_dims: ThinVec<usize> =
                    indices.iter().map(|i| i % block_size).collect();
                // assuming all the blocks are layed out in row-major format
                let block_index = block_offsets_along_dims
                    .iter()
                    .zip(outer_strides.0.iter())
                    .fold(0, |acc, (offset, stride)| acc + (*offset * *stride));

                let inner_index = inner_offsets_along_dims
                    .iter()
                    .zip(inner_strides.0.iter())
                    .fold(0, |acc, (offset, stride)| acc + (*offset * *stride));

                return block_index + inner_index;
            }
        };

        indices
            .iter()
            .zip(strides.0.iter())
            .fold(0, |acc, (ix, stride)| acc + stride * ix)
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Shape(pub ThinVec<usize>);

#[derive(Clone, Debug, PartialEq)]
pub struct CanonicalTensor {
    // data: Cow<'arena, [PrimitiveValue]>,
    pub data: ThinVec<Value>,
    pub data_type: Type,
    pub layout: Layout,
    pub metadata: TenMeta,
    pub shape: Shape,
}

impl CanonicalTensor {
    pub fn new(
        data_type: Type,
        flattened: ThinVec<Value>,
        layout_f: fn(&Shape) -> Layout,
        metadata: TenMeta,
        shape: ThinVec<usize>,
    ) -> Self {
        let shape = Shape(shape);
        let layout = layout_f(&shape);
        CanonicalTensor {
            data: flattened,
            data_type,
            layout,
            metadata,
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

#[derive(Clone, Debug, PartialEq)]
pub enum Hint {
    Dim(usize),
    Type(Type),
}

#[derive(Clone, Debug, PartialEq)]
pub struct TensorInit {
    default_value: ExprIdx,
    dim: Option<usize>,
}

impl HIRBuilder {
    pub fn lower_hint(&mut self, hint: &ASTHint) -> HIRResult<Hint> {
        let hint = match hint {
            ASTHint::Dim(dim) => Hint::Dim(*dim),
            ASTHint::Type(typ) => Hint::Type(self.lower_type(typ)?),
        };
        Ok(hint)
    }
    pub fn lower_dim_hints(&mut self, dim_hints: &ThinVec<ASTHint>) -> HIRResult<ThinVec<Hint>> {
        let mut low_dim_hints = ThinVec::new();
        for dim_hint in dim_hints {
            let hint = self.lower_hint(dim_hint)?;
            low_dim_hints.push(hint);
        }
        Ok(low_dim_hints)
    }
    pub fn lower_tensor_init(&mut self, tensor_init: &ASTTensorInit) -> HIRResult<TensorInit> {
        let default_value = self.try_lowering_expr_as_idx(tensor_init.get_default_value())?;
        let dim = tensor_init.get_dim();
        Ok(TensorInit { default_value, dim })
    }
    pub fn lower_tensor_struct(
        &mut self,
        tensor_struct: &ASTTensorStruct,
    ) -> HIRResult<TensorStruct> {
        let low_dim_hints = self.lower_dim_hints(&tensor_struct.dims)?;
        let type_hint = if let Some(type_hint) = &tensor_struct.type_hint {
            Some(self.lower_hint(type_hint)?)
        } else {
            None
        };
        let init = if let Some(init) = &tensor_struct.init {
            Some(self.lower_tensor_init(init)?)
        } else {
            None
        };
        Ok(TensorStruct {
            dims: low_dim_hints,
            type_hint,
            init,
        })
    }
}

#[derive(Clone, Debug, PartialEq)]
// TODO: this should finally be able to fill all the fields
pub struct TensorStruct {
    dims: ThinVec<Hint>,
    type_hint: Option<Hint>,
    init: Option<TensorInit>,
}

#[cfg(test)]
mod tests {

    use super::*;
    use ast::{ast_root_from, cast_node_into_type};
    use parameterized_test::create;

    use crate::{expression::Expr, scope::into_idx};

    create! {
        create_tensor_struct_test,
        (program, exp_dims, exp_type, exp_init), {
        let ast_root = ast_root_from(program);
        let tensor_node = ast_root.get_root().first_child().unwrap();
        let ast_tensor_struct = cast_node_into_type::<ASTTensorStruct>(&tensor_node);
        let mut hir_builder = HIRBuilder::new(ast_root);

        let tensor_struct = hir_builder.lower_tensor_struct(&ast_tensor_struct).expect("should have been ok");
        assert_eq!(exp_dims, tensor_struct.dims);
        assert_eq!(exp_type, tensor_struct.type_hint);
        assert_eq!(exp_init, tensor_struct.init);
        }
    }

    create_tensor_struct_test! {
        tensor_struct: ("Tensor<i32><3,3,3>", thin_vec![Hint::Dim(3);3], Some(Hint::Type(Type::Integer32)), None),
        tensor_struct_typed: ("Tensor<i32>", ThinVec::<Hint>::new(), Some(Hint::Type(Type::Integer32)), None),
        tensor_struct_dim_hinted: ("Tensor<3,3,3>", thin_vec![Hint::Dim(3);3], None, None),
        tensor_struct_dim_hinted_init: ("Tensor<3,3,3>[0.0]", thin_vec![Hint::Dim(3);3], None, Some(TensorInit{default_value: into_idx::<Expr>(1), dim: None})),
        tensor_struct_type_hinted_full_init: ("Tensor<f32>[1.0;1000]", ThinVec::<Hint>::new(), Some(Hint::Type(Type::Float32)), Some(TensorInit{default_value: into_idx::<Expr>(1), dim: Some(1000)})),
    }
}
