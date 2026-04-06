use std::{collections::HashMap, ops::Index};

use thin_vec::ThinVec;

/// Represents a block's shape with which the layout will be formed
#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub enum BlockShape {
    Uniform { cross_section: usize, dim: usize },
    NonUniform(ThinVec<usize>),
}

const DIFFERENT_DIMENSIONAL_ERR: &'static str =
    "blocks with different dimensionality w.r.t. the collections are not implemented";

impl BlockShape {
    pub fn uniform_with_shape(uniform: usize, dim: usize) -> Self {
        Self::Uniform {
            cross_section: uniform,
            dim: dim,
        }
    }
    pub fn dimensionality(&self) -> usize {
        match self {
            Self::Uniform { dim, .. } => *dim,
            Self::NonUniform(thin_vec) => thin_vec.len(),
        }
    }
    pub fn span(&self) -> usize {
        match self {
            Self::Uniform { cross_section, dim } => cross_section.pow(*dim as u32),
            Self::NonUniform(thin_vec) => thin_vec.iter().fold(1, |acc, d| acc * d),
        }
    }
    pub fn num_blocks_in_shape(&self, collection_shape: &[usize]) -> ThinVec<usize> {
        match self {
            Self::Uniform { cross_section, dim } => {
                // note that the shape of the block can be different from the shape of the collection
                if *dim != collection_shape.len() {
                    unimplemented!("{}", DIFFERENT_DIMENSIONAL_ERR);
                }
                collection_shape
                    .iter()
                    .map(|cs| cs / cross_section)
                    .collect()
            }
            Self::NonUniform(thin_vec) => collection_shape
                .iter()
                .zip(thin_vec.iter())
                .map(|(cs, d)| cs / d)
                .collect(),
        }
    }
}

impl From<&[usize]> for BlockShape {
    fn from(value: &[usize]) -> Self {
        Self::NonUniform(ThinVec::from(value))
    }
}

impl Index<usize> for BlockShape {
    type Output = usize;

    fn index(&self, index: usize) -> &Self::Output {
        match self {
            Self::Uniform { cross_section, dim } => {
                if index >= *dim {
                    unimplemented!("{}", DIFFERENT_DIMENSIONAL_ERR);
                }
                cross_section
            }
            Self::NonUniform(thin_vec) => thin_vec.get(index).expect("a valid index"),
        }
    }
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
