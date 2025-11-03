use smol_str::{SmolStr, ToSmolStr};
use thin_vec::{ThinVec, thin_vec};
use thiserror::Error;

use crate::collection::block::BlockShape;

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
//   - [r,d,c],
//   - [c,r,d],
//   - [c,d,r],
//   - [d,r,c],
//   - [d,c,r],
/*
   Since we don't allow the user to pick/change the memory layout, the only concern is the Ops/methods that
   inherently change the layout or compiler-inserted memory layout change (if we can confidently say that
   changing the layout beforehand would be better than delegating to LayoutIndexing).

   Layout changing Ops:
   - t.reshape(new_shape) or reshape(t, new_shape) -> changes the shape thus
       ? can we not change the shape but just arrange the strides and get away with it?
   - layout_change(a, to_layout)            -> compiler-inserted
   - t.transpose() or transpose(t)          -> permutes axes -> implies a layout change
   - t.broadcast(shape) or broadcast(t)     -> may insert virtual dimensions with new layout
   - t.pad(...) or pad(t, ...)              -> can lead to offset views or reallocation
   - gather/scatter                         -> can create arbitrarily disordered layouts

   Tiling-Requiring Ops
   (don't semantically change the Layout BUT require a certain layout to be fused efficiently e.g. Blocked)
   - matmul                         -> Blocked improves cache reuse, vectorization
   - conv2d                         -> strongly prefers NHWC or NCHW layout depending on backend
   - batch_norm, group_norm         -> reduced axes to be aligned
   - softmax, argmax                -> need contiguousness across axis of reduction

    Compiler-inserted layout transforms
    - layout_adapt(t, to_layout)    -> convert into exp. layout of next Op i.e. satisfy the LayoutContract
    - copy_if_needed(t)             -> prevents incorrect alising of reused memory
*/
// ! if tensors share the same layout, I want a method to call with their strides and index vectors once
// to receive the resulting indexings at once

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub enum Layout {
    /// C order
    RowMajor(Strides),
    /// Fortran order
    ColumnMajor(Strides),
    /// Enables the tiled access
    // ! Blocked layout has its intrinsic row-major order, make it configurable
    Blocked {
        block_shape: BlockShape,
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

fn row_major_strides_of(shape: &Shape) -> Strides {
    let length = shape.dimensionality();
    let shape_vec = shape
        .get()
        .expect(&LayoutError::unknown_shape().to_smolstr());
    let mut strides = thin_vec![1; length];

    for rev_ix in (0..length - 1).rev() {
        strides[rev_ix] = strides[rev_ix + 1] * shape_vec[rev_ix + 1];
    }
    Strides(strides)
}

impl Layout {
    /// Constructs a RowMajor layout for the given shape.
    /// In RowMajor, the last dimension is contiguous in memory.
    pub fn row_major(shape: &Shape) -> Self {
        let strides = row_major_strides_of(shape);
        Self::RowMajor(strides)
    }
    /// Constructs a ColumnMajor layout for the given shape.
    /// In ColumnMajor, the dimension before the last is contiguous in memory.
    pub fn col_major(shape: &Shape) -> Self {
        let mut strides = row_major_strides_of(shape);
        strides.swap();
        Self::ColumnMajor(strides)
    }
    /// Switches between RowMajor and ColumnMajor layouts by reversing the last two strides in place,
    /// and changes the enum variant accordingly. Returns an error if called on a Blocked layout.
    pub fn switch_major(&mut self) -> Result<(), LayoutError> {
        match self {
            Layout::Blocked { .. } => Err(LayoutError::not_a_major_layout(self)),
            Layout::RowMajor(strides) => {
                strides.swap();
                let new_strides = strides.clone();
                *self = Layout::ColumnMajor(new_strides);
                Ok(())
            }
            Layout::ColumnMajor(strides) => {
                strides.swap();
                let new_strides = strides.clone();
                *self = Layout::RowMajor(new_strides);
                Ok(())
            }
        }
    }
    /// Constructs a Blocked layout for the given shape and block size.
    /// Blocked layout enables tiled access for efficient memory usage.
    pub fn blocked(block_shape: impl Into<BlockShape>, collection_shape: &[usize]) -> Self {
        let block_shape: BlockShape = block_shape.into();
        let block_span = block_shape.span();

        let mut num_blocks_in_shape = block_shape.num_blocks_in_shape(collection_shape);
        let dimensionality = block_shape.dimensionality();
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
            inner_strides[ix - 1] = inner_strides[ix] * block_shape[ix];
        }

        let outer_strides = Strides(outer_strides);
        let inner_strides = Strides(inner_strides);

        Self::Blocked {
            block_shape,
            block_span,
            num_blocks_in_shape,
            outer_strides,
            inner_strides,
        }
    }
}
// ! needs:
// - inverse layout projection (flat output index -> multidim index -> input layout index)
// - broadcast/index alignment between tensors with different layouts
// - index expression synthesis across multiple layouts
/*
trait Layout {
    fn index_to_coords(&self, flat_idx: usize) -> ThinVec<usize>;
}

let coords = output_layout.index_to_coords(output_idx);
let lhs_idx = lhs_layout.indexing(&coords);
let rhs_idx = rhs_layout.indexing(&coords);

 */
impl LayoutIndexing for Layout {
    /// Converts multi-dimensional indices into the corresponding index of the flattened buffer.
    /// Handles RowMajor, ColumnMajor, and Blocked layouts.
    fn indexing(&self, indices: &[usize]) -> usize {
        let strides = match self {
            Layout::RowMajor(strides) => strides,
            Layout::ColumnMajor(strides) => strides,
            Layout::Blocked {
                block_shape,
                block_span: _,
                num_blocks_in_shape: _,
                outer_strides,
                inner_strides,
            } => {
                let block_and_inner_offsets_along_dims: ThinVec<(usize, usize)> = indices
                    .iter()
                    .enumerate()
                    .map(|(ix, ind)| {
                        let dim = block_shape[ix];
                        (*ind / dim, *ind % dim)
                    })
                    .collect();

                // assuming all the blocks are layed out in row-major format
                // ! Blocked layout has its intrinsic row-major order
                let (block_index, inner_index) = block_and_inner_offsets_along_dims
                    .iter()
                    .zip(outer_strides.0.iter().zip(inner_strides.0.iter()))
                    .fold((0_usize, 0_usize), |acc, (offsets, strides)| {
                        (
                            acc.0 + (offsets.0 * strides.0),
                            acc.1 + (offsets.1 * strides.1),
                        )
                    });
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

    fn test_layout_indexing_on_3d_shape(
        shape: &[usize],
        layout: &Layout,
        expected_indices: &[&[&[usize]]],
    ) {
        for ix in 0..shape[0] {
            for iy in 0..shape[1] {
                for iz in 0..shape[2] {
                    let indexing = layout.indexing(&[ix, iy, iz]);
                    let expected_index = expected_indices[ix][iy][iz];
                    assert_eq!(
                        expected_index, indexing,
                        "[layout: {:?}], {:?} != {:?}",
                        layout, expected_index, indexing
                    );
                }
            }
        }
    }

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
        let shape = thin_vec![3, 3, 3];
        let expected_strides = [
            [9, 3, 1], /* row-major */
            [9, 1, 3], /* column-major */
        ];
        let expected_indices_for_row_major: &[&[&[usize]]] = &[
            &[&[0, 1, 2], &[3, 4, 5], &[6, 7, 8]],
            &[&[9, 10, 11], &[12, 13, 14], &[15, 16, 17]],
            &[&[18, 19, 20], &[21, 22, 23], &[24, 25, 26]],
        ];

        let expected_indices_for_col_major: &[&[&[usize]]] = &[
            &[&[0, 3, 6], &[1, 4, 7], &[2, 5, 8]],
            &[&[9, 12, 15], &[10, 13, 16], &[11, 14, 17]],
            &[&[18, 21, 24], &[19, 22, 25], &[20, 23, 26]],
        ];
        // tensor[x][y][z] into the corresponding index of the flattened buffer
        let expected_indices = [
            expected_indices_for_row_major,
            expected_indices_for_col_major,
        ];
        let layouts = [
            Layout::row_major(&Shape::Buffer(shape.clone())),
            Layout::col_major(&Shape::Buffer(shape.clone())),
        ];

        for (ix, layout) in layouts.iter().enumerate() {
            let strides = match &layout {
                Layout::RowMajor(strides) => strides,
                Layout::ColumnMajor(strides) => strides,
                _ => unreachable!(),
            };
            assert_eq!(ThinVec::from(expected_strides[ix]), strides.0);
            test_layout_indexing_on_3d_shape(shape.as_slice(), layout, &expected_indices[ix]);
        }
    }
    #[test]
    fn test_layout_logic_for_uniformly_blocked() {
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
        let shape = thin_vec![2, 4, 4];

        // tensor[x][y][z] into the corresponding index of the flattened buffer
        let expected_indices: &[&[&[usize]]] = &[
            &[
                &[0, 1, 8, 9],
                &[2, 3, 10, 11],
                &[16, 17, 24, 25],
                &[18, 19, 26, 27],
            ],
            &[
                &[4, 5, 12, 13],
                &[6, 7, 14, 15],
                &[20, 21, 28, 29],
                &[22, 23, 30, 31],
            ],
        ];
        // blocked layouts with blocks 2x2x2 (cube)
        let same_block_shapes: [BlockShape; 2] = [
            BlockShape::uniform_with_shape(2, 3),
            [2_usize; 3].as_slice().into(),
        ];
        for into_block_shape in same_block_shapes {
            let layout = Layout::blocked(into_block_shape, &shape);
            test_layout_indexing_on_3d_shape(shape.as_slice(), &layout, &expected_indices);
        }
    }
    #[test]
    fn test_layout_logic_for_non_uniformly_blocked_single_on_outmost_axis() {
        /*
        [
            [
                [0,0,1,1],      --> [0,1,4,5]
                [0,0,1,1],      --> [2,3,6,7]
                [2,2,3,3],      --> [8,9,12,13]
                [2,2,3,3],      --> [10,11,14,15]
            ],
            [
                [4,4,5,5],      --> [16,17,20,21]
                [4,4,5,5],      --> [18,19,22,23]
                [6,6,7,7],      --> [24,25,28,29]
                [6,6,7,7],      --> [26,27,30,31]
            ],
        ]
        */
        let shape = thin_vec![2, 4, 4];

        // tensor[x][y][z] into the corresponding index of the flattened buffer
        let expected_indices: &[&[&[usize]]] = &[
            &[
                &[0, 1, 4, 5],
                &[2, 3, 6, 7],
                &[8, 9, 12, 13],
                &[10, 11, 14, 15],
            ],
            &[
                &[16, 17, 20, 21],
                &[18, 19, 22, 23],
                &[24, 25, 28, 29],
                &[26, 27, 30, 31],
            ],
        ];
        // blocked layouts with blocks 1x2x2 (cube)
        let block_shapes: [BlockShape; 1] = [[1, 2, 2].as_slice().into()];
        for into_block_shape in block_shapes {
            let layout = Layout::blocked(into_block_shape, &shape);
            test_layout_indexing_on_3d_shape(shape.as_slice(), &layout, &expected_indices);
        }
    }
    #[test]
    fn test_layout_logic_for_non_uniformly_blocked_single_on_middle_axis() {
        /*
        [
            [
                [0,0,1,1],      --> [0,1,4,5]
                [2,2,3,3],      --> [8,9,12,13]
                [4,4,5,5],      --> [16,17,20,21]
                [6,6,7,7],      --> [24,25,28,29]
            ],
            [
                [0,0,1,1],      --> [2,3,6,7]
                [2,2,3,3],      --> [10,11,14,15]
                [4,4,5,5],      --> [18,19,22,23]
                [6,6,7,7],      --> [26,27,30,31]
            ],
        ]
        */
        let shape = thin_vec![2, 4, 4];

        // tensor[x][y][z] into the corresponding index of the flattened buffer
        let expected_indices: &[&[&[usize]]] = &[
            &[
                &[0, 1, 4, 5],
                &[8, 9, 12, 13],
                &[16, 17, 20, 21],
                &[24, 25, 28, 29],
            ],
            &[
                &[2, 3, 6, 7],
                &[10, 11, 14, 15],
                &[18, 19, 22, 23],
                &[26, 27, 30, 31],
            ],
        ];
        // blocked layouts with blocks 2x1x2 (cube)
        let block_shapes: [BlockShape; 1] = [[2, 1, 2].as_slice().into()];
        for into_block_shape in block_shapes {
            let layout = Layout::blocked(into_block_shape, &shape);
            test_layout_indexing_on_3d_shape(shape.as_slice(), &layout, &expected_indices);
        }
    }
    #[test]
    fn test_layout_logic_for_non_uniformly_blocked_single_on_innermost_axis() {
        /*
        [
            [
                [0,1,2,3],      --> [0,4,8,12]
                [0,1,2,3],      --> [1,5,9,13]
                [4,5,6,7],      --> [16,20,24,28]
                [4,5,6,7],      --> [17,21,25,29]
            ],
            [
                [0,1,2,3],      --> [2,6,10,14]
                [0,1,2,3],      --> [3,7,11,15]
                [4,5,6,7],      --> [18,22,26,30]
                [4,5,6,7],      --> [19,23,27,31]
            ],
        ]
        */
        let shape = thin_vec![2, 4, 4];

        // tensor[x][y][z] into the corresponding index of the flattened buffer
        let expected_indices: &[&[&[usize]]] = &[
            &[
                &[0, 4, 8, 12],
                &[1, 5, 9, 13],
                &[16, 20, 24, 28],
                &[17, 21, 25, 29],
            ],
            &[
                &[2, 6, 10, 14],
                &[3, 7, 11, 15],
                &[18, 22, 26, 30],
                &[19, 23, 27, 31],
            ],
        ];
        // blocked layouts with blocks 2x2x1 (cube)
        // Note how because of the block layout, now it became internally column-major
        let block_shapes: [BlockShape; 1] = [[2, 2, 1].as_slice().into()];
        for into_block_shape in block_shapes {
            let layout = Layout::blocked(into_block_shape, &shape);
            test_layout_indexing_on_3d_shape(shape.as_slice(), &layout, &expected_indices);
        }
    }
    #[test]
    fn test_block_with_singular_dimensions_is_equal_to_row_major() {
        /* tensor for testing is as follows "[
                [
                    [0, 1, 2],
                    [3, 4, 5],
                    [6, 7, 8]
                ],
                [
                    [9, 10, 11],
                    [12, 13, 14],
                    [15, 16, 17]
                ],
                [
                    [18, 19, 20],
                    [21, 22, 23],
                    [24, 25, 26]
                ]
            ] 
        */
        let shape = &[3, 3, 3];

        // tensor[x][y][z] into the corresponding index of the flattened buffer
        let expected_indices: &[&[&[usize]]] = &[
            &[&[0, 1, 2], &[3, 4, 5], &[6, 7, 8]],
            &[&[9, 10, 11], &[12, 13, 14], &[15, 16, 17]],
            &[&[18, 19, 20], &[21, 22, 23], &[24, 25, 26]],
        ];
        // blocked layouts with blocks 1x1x1 (cube)
        // Note how because of the unit block layout, it became row-major
        let block_shapes: [BlockShape; 1] = [[1, 1, 1].as_slice().into()];
        for into_block_shape in block_shapes {
            let layout = Layout::blocked(into_block_shape, shape);
            test_layout_indexing_on_3d_shape(shape.as_slice(), &layout, &expected_indices);
        }
    }
}
