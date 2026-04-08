use smol_str::SmolStr;
use std::hash::Hash;
use thin_vec::ThinVec;

use crate::{
    collection::shape::Shape,
    expression::Expr,
    literal::LazyCollection,
    typing::hindley_milner::types::{Maybe, Type},
};

use super::layout::Layout;

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub enum LazyInit {
    FromLiteral(LazyCollection),
    /// buffer/tensor of given shape filled with the given expression [expr, <shape>]
    WithAll(Expr, Shape),
    /// from a file
    FromStream(SmolStr),
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub struct Uninitialized {
    pub shape: Option<ThinVec<usize>>,
    pub data_type: Maybe<Type>,
    pub layout: Option<Layout>,
    pub lazy_init: LazyInit,
}

impl Uninitialized {
    /// unlowered literal do not have any information on the actual Shape, data type of Layout of the collection
    /// since it's lowering and analysis is deferred
    pub fn from_unlowered_literal(lazy_init: LazyInit) -> Self {
        Self {
            shape: None,
            data_type: Maybe::None,
            layout: None,
            lazy_init,
        }
    }
}
