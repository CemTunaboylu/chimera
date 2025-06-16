use thin_vec::ThinVec;

use crate::literal::Value;

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
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
