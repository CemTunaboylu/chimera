use la_arena::Idx;
use thin_vec::ThinVec;

use crate::{
    literal::Value,
    scope::{ExprIdx, Scoped},
};

use super::layout::Layout;

pub type StorageIdx = Idx<Storage>;
pub type ScopedStorageIdx = Scoped<StorageIdx>;

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub enum Storage {
    Direct(ThinVec<Value>),
    Indexed(ThinVec<ExprIdx>),
}

impl Storage {
    pub fn switch_storage_memory_layout(&mut self, from: Layout, to: &Layout) {
        unimplemented!()
    }
    /// From any major to
    fn from_major_to_major(&mut self, from: Layout, to: &Layout) {
        unimplemented!()
    }
}
