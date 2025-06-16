use thin_vec::{ThinVec, thin_vec};

use super::{Shape, Strides};

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
        let length = shape.dimensionality();
        let shape_vec = shape.get().expect("expected a known shape");
        let mut strides = thin_vec![1; length];

        for rev_ix in (0..length - 1).rev() {
            strides[rev_ix] = strides[rev_ix + 1] * shape_vec[rev_ix + 1];
        }
        Self::RowMajor(Strides(strides))
    }
    pub fn col_major(shape: &Shape) -> Self {
        let length = shape.dimensionality();
        let shape_vec = shape.get().expect("expected a known shape");
        let mut strides = thin_vec![1; length];

        for ix in 1..length {
            strides[ix] = strides[ix - 1] * shape_vec[ix - 1];
        }
        Self::ColumnMajor(Strides(strides))
    }
    pub fn blocked(block_size: usize, shape: &Shape) -> Self {
        let dimensionality = shape.dimensionality();
        let block_span = block_size.pow(dimensionality as u32);
        let shape_vec = shape.get().expect("expected a known shape");

        let mut num_blocks_in_shape: ThinVec<usize> =
            shape_vec.iter().map(|e| e / block_size).collect();
        let mut outer_strides = thin_vec![block_span; dimensionality];
        let mut inner_strides = thin_vec![1; dimensionality];

        for ix in (0..num_blocks_in_shape.len() - 1).rev() {
            num_blocks_in_shape[ix] *= num_blocks_in_shape[ix + 1];
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
