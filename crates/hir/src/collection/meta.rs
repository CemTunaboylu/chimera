use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
};

use core::hash::Hash;

use thin_vec::ThinVec;

use crate::{
    HIRResult,
    errors::HIRError,
    expression::Expr,
    literal::Value,
    metadata::Common,
    scope::{ExprIdx, ScopeIdx, placeholder_idx},
    typing::hindley_milner::{
        expression::HMExpr,
        types::{Maybe, Type},
    },
};

use super::{Shape, builtin::Op, layout::Layout};

/* TODO:
- max,min while traversing, but the type must impl. Cmp,
    delegate to an object
- cache for ops, have a generation field, each time mutated
    increment, thus ops won't collide with cache?
    have a cache TensorOp -> result
    pub op_log: ThinVec<TensorOp>
*/
#[derive(Clone, Debug, Default, Eq, Hash, PartialEq, PartialOrd)]
pub struct CollectionMeta {
    // pub data_type: Maybe<Type>,
    pub common: Common,
    pub layout: Layout,
    pub shape: Shape,
    // sparsity: if more than X% is same element, tensor is
    // considered sparse
    pub sparse: bool,
    // heap tensors are grouped by shape + type hash to reuse backing memory across allocations  (Allocation Pooling)
    pub group_id: u32,
    pub is_allocated: bool,
}

impl CollectionMeta {
    pub fn with(
        common: Common,
        collection_examination: CollectionExamination,
        shape: Shape,
    ) -> Self {
        Self {
            common: Common::default(),
            sparse: collection_examination.sparsity.is_sparse(),
            // data_type: collection_examination.unchecked_types(),
            shape: shape.clone(),
            layout: Layout::row_major(&shape),
            group_id: 0,        // TODO: fix
            is_allocated: true, // TODO: fix
        }
    }
}

pub struct CollectionExamination {
    scope_idx: ScopeIdx,
    sparsity: Sparsity<ExprIdx>,
    expressions: HashSet<ExprIdx>,
}
impl Default for CollectionExamination {
    fn default() -> Self {
        Self::new(placeholder_idx())
    }
}

impl CollectionExamination {
    pub fn new(scope_idx: ScopeIdx) -> Self {
        Self {
            scope_idx,
            sparsity: Sparsity::new(),
            expressions: HashSet::new(),
        }
    }
    pub fn add(&mut self, _expr: &Expr) -> HIRResult<()> {
        todo!()
    }
    pub fn add_with_id(&mut self, idx: ExprIdx) -> HIRResult<()> {
        self.sparsity.count(&idx);
        self.expressions.insert(idx);
        Ok(())
    }
}

pub struct Sparsity<V: Eq + Hash + Clone>(HashMap<V, usize>, usize);

impl<V: Eq + Hash + Clone> Default for Sparsity<V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<V: Eq + Hash + Clone> Sparsity<V> {
    pub fn new() -> Self {
        Self(HashMap::new(), 0)
    }
    pub fn count(&mut self, value: &V) {
        if self.0.contains_key(value) {
            self.0.entry(value.clone()).and_modify(|e| *e += 1);
        } else {
            self.0.insert(value.clone(), 1);
        }
        self.1 += 1;
    }
    pub fn is_sparse(&self) -> bool {
        let whole = self.1;
        let threshold = (whole as f32 * 0.7) as usize;
        // TODO: record the most frequent
        self.0.iter().any(|entry| *entry.1 >= threshold)
    }
}
