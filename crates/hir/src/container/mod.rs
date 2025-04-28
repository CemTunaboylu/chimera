use canonical::CanonicalBuffer;
use la_arena::Idx;
use thin_vec::{ThinVec, thin_vec};

use crate::{
    HIRResult,
    builder::HIRBuilder,
    expression::Expr,
    literal::{Literal, Value},
    scope::ExprIdx,
    typing::hindley_milner::{
        expression::HMExpr,
        types::{Maybe, Type},
    },
};
use ast::container::Shape as ASTShape;

pub mod block;
pub mod canonical;
pub mod layout;
pub mod meta;
pub mod op;
pub mod tensor;
pub mod uninit;

pub type CanonicalLiteralIdx = Idx<CanonicalBuffer>;

#[derive(Clone, Debug, PartialEq)]
pub struct Strides(pub ThinVec<usize>);

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub enum Dim {
    Static(usize),
    Dynamic(ExprIdx),
    Unknown,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub enum Shape {
    Buffer(ThinVec<usize>),
    Tensor(ThinVec<Dim>),
}
impl Shape {
    pub fn get(&self) -> Option<&ThinVec<usize>> {
        match self {
            Shape::Buffer(shape) => Some(shape),
            Shape::Tensor(_) => None,
        }
    }
}
impl Shape {
    pub fn dimensionality(&self) -> usize {
        match self {
            Shape::Buffer(shape) => shape.len(),
            Shape::Tensor(shape) => shape.len(),
        }
    }
}

impl HIRBuilder {
    pub fn lower_shape(&mut self, shape: &ASTShape) -> HIRResult<Shape> {
        match shape {
            ASTShape::Known(shape) => Ok(Shape::Buffer(shape.clone())),
            ASTShape::MaybeUnknown(shape) => {
                let mut dims = ThinVec::new();
                for expr in shape.iter() {
                    if let Some(expr) = expr {
                        let expr_idx = self.lower_expr_as_idx(expr)?;
                        let expr = self.get_expr(expr_idx);
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
