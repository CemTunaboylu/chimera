use std::collections::HashMap;

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
