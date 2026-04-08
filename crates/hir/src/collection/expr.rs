use thin_vec::ThinVec;

use crate::{
    index_types::ScopedExprIdx,
    operation::{BinaryOp, UnaryOp},
};

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub enum TensorExpr {
    Binary {
        op: BinaryOp,
        lhs: ScopedExprIdx,
        rhs: ScopedExprIdx,
    },
    Unary {
        op: UnaryOp,
        expr: ScopedExprIdx,
    },
    FromExpr(ScopedExprIdx),
}
