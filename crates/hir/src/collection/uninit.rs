use ast::collection::BufferTree;
use smol_str::SmolStr;
use std::hash::Hash;
use thin_vec::ThinVec;

use crate::{
    HIRResult,
    builder::HIRBuilder,
    collection::{op::Op, shape::Shape},
    expression::Expr,
    literal::LazyCollection,
    typing::hindley_milner::types::{Maybe, Type},
};

use super::layout::Layout;

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub enum LazyInit {
    FromLiteral(LazyCollection),
    WithAll(Expr, Shape),
    /// from a file
    FromStream(SmolStr),
    /// from op
    FromOp(Op),
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub struct Uninitialized {
    pub shape: ThinVec<usize>,
    pub data_type: Maybe<Type>,
    pub layout: Layout,
    pub lazy_init: LazyInit,
}
