use smol_str::{SmolStr, ToSmolStr};
use thin_vec::{ThinVec, thin_vec};
use thiserror::Error;

use super::{Shape, Strides};

#[derive(Clone, Debug, Error, PartialEq)]
#[error("LayoutError")]
pub enum LayoutError {
    NotAMajorLayout(SmolStr),
    UnknownShape(SmolStr),
}

impl LayoutError {
    pub fn not_a_major_layout(layout: &Layout) -> Self {
        Self::NotAMajorLayout(format!("{:?} is not a Major layout", layout).to_smolstr())
    }
    pub fn unknown_shape() -> Self {
        Self::UnknownShape("expected a known shape".to_smolstr())
    }
}

pub trait LayoutIndexing {
    fn indexing(&self, indices: &[usize]) -> usize;
}

// ! einops-like shape for layout
// ! a [r,c,d] should be transposable to
//   - [c,r,d],
//   - [r,d,c],
//   - [c,d,r],
//   - [d,r,c],
//   - [d,c,r],

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub enum Layout {
    /// C order
    RowMajor(Strides),
    /// Fortran order
    ColumnMajor(Strides),
    /// Enables the tiled access
    // ! Blocked layout has its intrinsic row-major order, make it configurable
    Blocked {
        block_size: usize,
        block_span: usize,
        num_blocks_in_shape: ThinVec<usize>,
        outer_strides: Strides,
        inner_strides: Strides,
    },
}

impl Default for Layout {
    /// Returns the default layout, which is RowMajor with default strides as C-order.
    // ! default should be Blocked with dims that are SIMD compatible
    fn default() -> Self {
        Self::RowMajor(Default::default())
    }
}

impl Layout {
    /// Constructs a RowMajor layout for the given shape.
    /// In RowMajor, the last dimension is contiguous in memory.
    pub fn row_major(shape: &Shape) -> Self {
        let length = shape.dimensionality();
        let shape_vec = shape
            .get()
            .expect(&LayoutError::unknown_shape().to_smolstr());
        let mut strides = thin_vec![1; length];

        for ix in 1..length {
            strides[ix] = strides[ix - 1] * shape_vec[ix - 1];
        }
        Self::RowMajor(Strides(strides))
    }
    /// Constructs a ColumnMajor layout for the given shape.
    /// In ColumnMajor, the first dimension is contiguous in memory.
    pub fn col_major(shape: &Shape) -> Self {
        let length = shape.dimensionality();
        let shape_vec = shape
            .get()
            .expect(&LayoutError::unknown_shape().to_smolstr());
        let mut strides = thin_vec![1; length];

        for rev_ix in (0..length - 1).rev() {
            strides[rev_ix] = strides[rev_ix + 1] * shape_vec[rev_ix + 1];
        }
        Self::ColumnMajor(Strides(strides))
    }
    /// Switches between RowMajor and ColumnMajor layouts by reversing the strides.
    /// Returns an error if called on a Blocked layout.
    pub fn switch_major(&mut self) -> Result<(), LayoutError> {
        let strides = match self {
            Self::Blocked { .. } => return Err(LayoutError::not_a_major_layout(self)),
            Self::ColumnMajor(strides) | Self::RowMajor(strides) => strides,
        };

        strides.reverse();
        Ok(())
    }
    /// Constructs a Blocked layout for the given shape and block size.
    /// Blocked layout enables tiled access for efficient memory usage.
    pub fn blocked(block_dim: usize, shape: &Shape) -> Self {
        let dimensionality = shape.dimensionality();
        let block_span = block_dim.pow(dimensionality as u32);
        let shape_vec = shape.get().expect("expected a known shape");

        let mut num_blocks_in_shape: ThinVec<usize> =
            shape_vec.iter().map(|e| e / block_dim).collect();
        // strides to jump-ahead along the dimensions in a block_dim=2 blocked layout is as:
        // [
        //     [0,0,1,1],
        //     [0,0,1,1],
        //     [2,2,3,3],
        //     [2,2,3,x]
        // ]
        // to reach x, we need to jump 3 blocks, namely the 0th, 1st and 2nd blocks
        // the outer_strides thus will be after the loop below computes all : [block_dim^3, block_dim^2, block_dim]
        let mut outer_strides = thin_vec![block_span; dimensionality];
        // strides to jump-ahead within the dimension e.g. in a cube 2nd row jumps len(first_row) * len(first_column) cells ahead
        // to reach x, we need to jump 3 elements, namely the first row (2 cells) along the horizontal axis, and single cell along the vertical.
        // the inner_strides thus will be after the loop below computes all : [block_dim^2, block_dim, 1]
        let mut inner_strides = thin_vec![1; dimensionality];

        for ix in (1..num_blocks_in_shape.len()).rev() {
            num_blocks_in_shape[ix - 1] *= num_blocks_in_shape[ix];
            outer_strides[ix - 1] *= num_blocks_in_shape[ix];
            inner_strides[ix - 1] = inner_strides[ix] * block_dim;
        }

        let outer_strides = Strides(outer_strides);
        let inner_strides = Strides(inner_strides);

        Self::Blocked {
            block_size: block_dim,
            block_span,
            num_blocks_in_shape,
            outer_strides,
            inner_strides,
        }
    }
}

impl LayoutIndexing for Layout {
    /// Converts multi-dimensional indices into the corresponding index of the flattened buffer.
    /// Handles RowMajor, ColumnMajor, and Blocked layouts.
    fn indexing(&self, indices: &[usize]) -> usize {
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
                // For Blocked layout, calculate block and inner offsets for each dimension.
                let block_offsets_along_dims = indices
                    .iter()
                    .map(|i| *i / block_size)
                    .collect::<ThinVec<usize>>();

                let inner_offsets_along_dims: ThinVec<usize> =
                    indices.iter().map(|i| i % block_size).collect();
                // assuming all the blocks are layed out in row-major format
                // ! Blocked layout has its intrinsic row-major order
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

#[cfg(test)]
mod tests {
    use thin_vec::{ThinVec, thin_vec};

    use super::*;

    use crate::collection::Shape;

    #[test]
    fn test_layout_logic_for_row_and_column_major() {
        /* tensor for testing is as follows "[
                [
                    [1,0,0],
                    [0,1,0],
                    [0,0,1]
                ], 
                [
                    [1,0,0],
                    [0,1,0],
                    [0,0,1]
                ], 
                [
                    [1,0,0],
                    [0,1,0],
                    [0,0,1]
                ] 
            ] 
        */
        let shape = &Shape::Buffer(thin_vec![3, 3, 3]);
        let expected_strides = [
            [1, 3, 9], /* row-major */
            [9, 3, 1], /* column-major */
        ];
        // tensor[x][y][z] into the corresponding index of the flattened buffer
        let expected_indices = [
            [
                ([0, 0, 0], 0),
                ([0, 0, 1], 9),
                ([0, 1, 2], 21),
                ([1, 1, 1], 13),
                ([2, 2, 2], 26),
            ], /* row-major */
            [
                ([0, 0, 0], 0),
                ([0, 0, 1], 1),
                ([0, 1, 2], 5),
                ([1, 1, 1], 13),
                ([2, 2, 2], 26),
            ], /* column-major */
        ];
        let layouts = [Layout::row_major(shape), Layout::col_major(shape)];

        for (ix, layout) in layouts.iter().enumerate() {
            let strides = match &layout {
                Layout::RowMajor(strides) => strides,
                Layout::ColumnMajor(strides) => strides,
                _ => unreachable!(),
            };
            assert_eq!(ThinVec::from(expected_strides[ix]), strides.0);
            for (ind, exp) in expected_indices[ix] {
                assert_eq!(exp, layout.indexing(&ind));
            }
        }
    }

    #[test]
    fn test_layout_logic_for_blocked() {
        /*
        [
            [
                [0,0,1,1],      --> [0,1,8,9]
                [0,0,1,1],      --> [2,3,10,11]
                [2,2,3,3],      --> [16,17,24,25]
                [2,2,3,3],      --> [18,19,26,27]
            ],
            [
                [0,0,1,1],      --> [4,5,12,13]
                [0,0,1,1],      --> [6,7,14,15]
                [2,2,3,3],      --> [20,21,28,29]
                [2,2,3,3],      --> [22,23,30,31]
            ],
        ]
        */
        let shape = &Shape::Buffer(thin_vec![2, 4, 4]);

        // tensor[x][y][z] into the corresponding index of the flattened buffer
        let expected_indices = [
            ([0, 0, 0], 0),
            ([0, 2, 3], 25),
            ([0, 3, 2], 26),
            ([1, 1, 0], 6),
            ([1, 1, 1], 7),
            ([1, 2, 0], 20),
            ([1, 2, 1], 21),
            ([1, 2, 3], 29),
            ([1, 3, 3], 31),
        ];
        // blocked layout with blocks 2x2x2 (cube)
        let layout = Layout::blocked(2, shape);

        for (ind, exp) in expected_indices {
            assert_eq!(exp, layout.indexing(&ind));
        }
    }

    // tensor_literal_happy_path_test! {
    //     tensor_2d_literal: ("[[1,0,0],[0,1,0],[0,0,1]]", Shape::Buffer(thin_vec![3,3]), test_metadata(Value::Int(1), true), thin_vec![into_idx(1), into_idx(2),into_idx(2), into_idx(2),into_idx(1), into_idx(2), into_idx(2), into_idx(2), into_idx(1)]),
    //     tensor_3d_literal: ("[[[1,0,0],[0,1,0],[0,0,1]], [[1,0,0],[0,1,0],[0,0,1]], [[1,0,0],[0,1,0],[0,0,2]] ]", Shape::Buffer(thin_vec![3,3,3]), test_metadata(Value::Int(2), true), thin_vec![into_idx(1), into_idx(2),into_idx(2), into_idx(2),into_idx(1), into_idx(2), into_idx(2), into_idx(2), into_idx(1),into_idx(1), into_idx(2),into_idx(2), into_idx(2),into_idx(1), into_idx(2), into_idx(2), into_idx(2), into_idx(1),into_idx(1), into_idx(2),into_idx(2), into_idx(2),into_idx(1), into_idx(2), into_idx(2), into_idx(2), into_idx(3)]),
    //     tensor_3d_literal_non_square: ("[[[1,0],[1,1],[0,1]], [[1,0],[1,1],[0,1]], [[1,0],[1,1],[0,1]] ]", Shape::Buffer(thin_vec![3,3,2]), test_metadata(Value::Int(1), true), thin_vec![into_idx(1), into_idx(2),into_idx(1), into_idx(1), into_idx(2), into_idx(1),into_idx(1), into_idx(2), into_idx(1),into_idx(1), into_idx(2), into_idx(1),into_idx(1), into_idx(2),into_idx(1), into_idx(1),into_idx(2), into_idx(1)]),
    // }
}
