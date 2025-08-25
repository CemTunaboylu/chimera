use la_arena::Idx;
use thin_vec::ThinVec;

use crate::{
    literal::Value,
    scope::{ExprIdx, Scoped},
};

pub type StorageIdx = Idx<Storage>;
pub type ScopedStorageIdx = Scoped<StorageIdx>;

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub enum Storage {
    Direct(ThinVec<Value>),
    Indexed(ThinVec<ExprIdx>),
}
