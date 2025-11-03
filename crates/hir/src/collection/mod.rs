use canonical::CanonicalBuffer;
use la_arena::Idx;
use thin_vec::ThinVec;

use crate::{
    HIRResult,
    builder::HIRBuilder,
    collection::uninit::{LazyInit, Uninitialized},
    errors::HIRError,
    expression::Expr,
    literal::{LazyCollection, Literal, Value},
    scope::Scoped,
};
use ast::collection::Shape as ASTShape;
use shape::{Dim, Shape};

pub mod block;
pub mod builtin;
pub mod canonical;
pub mod contract;
pub mod expr;
pub mod fusion;
pub mod layout;
pub mod meta;
pub mod shape;
pub mod storage;
pub mod uninit;

pub type CanonicalLiteralIdx = Idx<CanonicalBuffer>;
pub type ScopedCanonicalLiteralIdx = Scoped<CanonicalLiteralIdx>;

#[derive(Clone, Debug, Default, Eq, Hash, PartialEq, PartialOrd)]
pub struct Strides(pub ThinVec<usize>);

impl Strides {
    pub(crate) fn swap(&mut self) {
        let len = self.0.len();
        if len >= 2 {
            self.0.swap(len - 1, len - 2);
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub enum State {
    Initialized(ScopedCanonicalLiteralIdx),
    /// [<value>; <shape array>] is uninitialized
    Uninitialized(Uninitialized),
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub enum Collection {
    Buffer(State),
    Tensor(State),
}

impl HIRBuilder {
    pub fn from_literal(&mut self, literal: Literal) -> HIRResult<Collection> {
        if let Literal(Value::LazyInit(lazy_collection)) = literal {
            let is_buffer = matches!(lazy_collection, LazyCollection::Buffer(_));
            let lazy_init = LazyInit::FromLiteral(lazy_collection);
            let uninitialized = Uninitialized::from_unlowered_literal(lazy_init);
            let collection_f = if is_buffer {
                Collection::Buffer
            } else {
                Collection::Tensor
            };
            return Ok(collection_f(State::Uninitialized(uninitialized)));
        }

        let (collection_f, idx) = if let Literal(Value::Tensor { idx, .. }) = literal {
            (Collection::Tensor as fn(State) -> Collection, idx)
        } else if let Literal(Value::Buffer { idx, .. }) = literal {
            (Collection::Buffer as fn(State) -> Collection, idx)
        } else {
            return Err(HIRError::with_msg(format!(
                "cannot create a buffer or tensor from literal {:?}",
                literal
            ))
            .into());
        };
        Ok(collection_f(State::Initialized(idx)))
    }
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
#[cfg(test)]
mod tests {
    use super::*;
    use ast::{ast_root_from_assert_no_err, cast_node_into_type, literal::Literal as ASTLiteral};

    fn into_collection(program: &str) -> Collection {
        let ast_root = ast_root_from_assert_no_err(program);
        let tensor_node = ast_root.get_root().first_child().unwrap();
        let ast_literal = cast_node_into_type::<ASTLiteral>(&tensor_node);
        let mut hir_builder = HIRBuilder::new(ast_root);
        let literal = hir_builder
            .lower_literal(&ast_literal)
            .expect("lowering to literal should have been Ok");
        hir_builder
            .from_literal(literal)
            .expect("should have been a tensor")
    }

    #[test]
    fn uninit_tensor_shorthand_literal_test() {
        let program = "tensor[0.0; dynamic]";
        let tensor = into_collection(program);

        assert!(matches!(
            tensor,
            Collection::Tensor(State::Uninitialized(Uninitialized { .. }))
        ));
    }

    #[test]
    fn uninit_buffer_literal_test() {
        let program = "[[0], [1], [2]]";
        let buffer = into_collection(program);

        assert!(matches!(
            buffer,
            Collection::Buffer(State::Uninitialized(Uninitialized { .. }))
        ));
    }
    #[test]
    fn uninit_buffer_shorthand_literal_test() {
        let program = "buffer[0.0; 100]";
        let buffer = into_collection(program);

        assert!(matches!(
            buffer,
            Collection::Buffer(State::Uninitialized(Uninitialized { .. }))
        ));
    }
}
