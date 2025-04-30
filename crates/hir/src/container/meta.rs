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
    scope::ExprIdx,
    typing::hindley_milner::{
        expression::HMExpr,
        types::{Maybe, Type},
    },
};

use super::{Shape, layout::Layout, op::TensorOp};

#[derive(Clone, Debug, PartialEq)]
pub struct TenMeta {
    // pub data_type: Maybe<Type>,
    pub layout: Layout,
    pub shape: Shape,
    // can be used for quantization,
    // TODO: get rid of Option
    pub max: Option<Value>,
    pub min: Option<Value>,
    // sparsity: if more than X% is same element, tensor is
    // considered sparse
    pub sparse: bool,    //
    pub num_refs: usize, // How many times is this function called statically?
    // cache only make sense if things stay the same
    // TODO: have a cache TensorOp -> result
    pub op_log: ThinVec<TensorOp>, // How many times is this function used?
    pub group_id: u32, // heap tensors are grouped by shape + type hash to reuse backing memory across allocations  (Allocation Pooling)
    pub is_allocated: bool,
}

impl TenMeta {
    pub fn with(container_examination: ContainerExamination, shape: Shape) -> Self {
        // let MinMax { max, min } = min_max;
        Self {
            // max: max,
            // min: min,
            max: None,
            min: None,
            sparse: container_examination.sparsity.is_sparse(),
            num_refs: 0,
            op_log: ThinVec::new(),
            // data_type: container_examination.unchecked_types(),
            shape: shape.clone(),
            layout: Layout::row_major(&shape),
            group_id: 0,        // TODO: fix
            is_allocated: true, // TODO: fix
        }
    }
}

pub struct ContainerExamination {
    sparsity: Sparsity<ExprIdx>,
    // min_max: MinMax,
    // types: Types,
}
impl Default for ContainerExamination {
    fn default() -> Self {
        Self::new()
    }
}

impl ContainerExamination {
    pub fn new() -> Self {
        Self {
            sparsity: Sparsity::new(),
            // min_max: MinMax::new(),
            // types: Types::new(),
        }
    }
    pub fn add(&mut self, _expr: &Expr, idx: ExprIdx) -> HIRResult<()> {
        self.sparsity.count(&idx);
        // self.min_max.min_max(value);
        // self.types.add(expr)?;
        Ok(())
    }
    // pub fn unchecked_types(self) -> Maybe<Type> {
    //     Maybe::Unchecked(self.types.0.into_iter().collect())
    // }
}

#[derive(Default)]
pub struct MinMax {
    max: Option<Value>,
    min: Option<Value>,
}

impl MinMax {
    pub fn min_max(&mut self, value: &Value) {
        let v = value.clone();
        let mx = self.max.get_or_insert(v.clone());
        if *mx < v {
            *mx = v;
            return;
        }
        let mn = self.min.get_or_insert(v.clone());
        if *mn > v {
            *mn = v;
        }
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

#[derive(Default)]
pub struct Types(HashSet<Type>);

impl Types {
    pub fn add(&mut self, _value: &Expr) -> HIRResult<()> {
        // Expr -> HMExpr -> Type
        // let t: Type = Type::from(&hm);
        // if !self.0.contains(&t) {
        //     self.0.insert(t);
        // }
        Ok(())
    }

    pub fn type_check(&self) -> HIRResult<()> {
        if self.0.len() > 1 {
            let msg = format!("found multiple types within tensor: {:?}", self.0);
            return Err(HIRError::with_msg(msg).into());
        }
        Ok(())
    }
    pub fn get_only_type(self) -> Type {
        let e = self.0.iter().next();
        e.unwrap().clone()
    }
}
