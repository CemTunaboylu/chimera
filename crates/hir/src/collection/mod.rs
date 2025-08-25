use canonical::CanonicalBuffer;
use la_arena::Idx;
use thin_vec::ThinVec;

use crate::{
    HIRResult,
    builder::HIRBuilder,
    expression::Expr,
    literal::{Literal, Value},
    scope::{Scoped, ScopedExprIdx},
};
use ast::collection::Shape as ASTShape;
use shape::{Dim, Shape};
use storage::Storage;

pub mod block;
pub mod buffer;
pub mod canonical;
pub mod layout;
pub mod meta;
pub mod op;
pub mod shape;
pub mod storage;
pub mod tensor;
pub mod uninit;

pub type CanonicalLiteralIdx = Idx<CanonicalBuffer>;
pub type ScopedCanonicalLiteralIdx = Scoped<CanonicalLiteralIdx>;

#[derive(Clone, Debug, Default, Eq, Hash, PartialEq, PartialOrd)]
pub struct Strides(pub ThinVec<usize>);

impl HIRBuilder {
    pub fn lower_shape(&mut self, shape: &ASTShape) -> HIRResult<Shape> {
        match shape {
            ASTShape::Known(shape) => Ok(Shape::Buffer(shape.clone())),
            ASTShape::MaybeUnknown(shape) => {
                let mut dims = ThinVec::new();
                for expr in shape.iter() {
                    if let Some(expr) = expr {
                        let expr_idx = self.lower_expr_as_idx(expr)?;
                        let expr = self.get_expr(&expr_idx);
                        let d = if let Expr::Literal(Literal(Value::Int(i))) = expr {
                            Dim::Static(*i as usize)
                        } else {
                            Dim::Dynamic(expr_idx)
                        };
                        dims.push(d);
                    } else {
                        dims.push(Dim::Unknown);
                    }
                }
                Ok(Shape::Tensor(dims))
            }
        }
    }
}
