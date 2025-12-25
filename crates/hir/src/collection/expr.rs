use thin_vec::ThinVec;

use crate::{
    collection::builtin::BuiltinMethod,
    index_types::ScopedExprIdx,
    operation::{BinaryOp, UnaryOp},
};

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub enum TensorExpr {
    Method {
        method: BuiltinMethod,
        args: ThinVec<ScopedExprIdx>,
    },
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

/*
typing sketch
match expr {
    Expr::Binary { op, lhs, rhs } => {
        let lhs_ty = type_of(lhs);
        let rhs_ty = type_of(rhs);

        match (lhs_ty, rhs_ty) {
            (Tensor, Tensor) => Expr::Tensor(Binary(op, lhs, rhs)),
            (Scalar, Tensor) => Expr::Tensor(Binary(op, promote(lhs), rhs)),
            (Tensor, Scalar) => Expr::Tensor(Binary(op, lhs, promote(rhs))),
            _ => Expr::Binary(op, lhs, rhs), // pure scalar
        }
    }
}
 */

/*
match tensor_expr {
    TensorExpr::Method { method, args } => {
        let behavior = method_behavior(&method);
        if behavior.returns_allocated_tensor {
            let layout = layout_pass.decide_for(&method);
            let buffer = materialize_buffer(method, args, layout);
            return TensorExpr::Literal(buffer);
        }
    }
}
 */
