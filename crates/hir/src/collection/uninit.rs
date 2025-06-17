use thin_vec::ThinVec;

use crate::typing::hindley_milner::types::{Maybe, Type};

use super::layout::Layout;

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub struct Uninitialized {
    pub shape: ThinVec<usize>,
    pub data_type: Maybe<Type>,
    pub layout: Layout,
}
