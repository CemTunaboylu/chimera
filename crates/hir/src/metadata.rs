use std::{
    cmp::min,
    collections::{HashMap, HashSet},
    fmt::Debug,
};

use thin_vec::ThinVec;

use crate::{HIRResult, errors::HIRError, literal::Value, tensor::TensorOp, types::Type};

#[derive(Clone, Debug)]
pub struct FnMeta {
    pub inline_hint: bool,  // Should this function be inlined?
    pub is_pure: bool,      // Is this function free of side effects?
    pub is_recursive: bool, // Does this function call itself?
    pub is_cyclic: bool,    // Is there a call cycle? (not sure if this is useful)
    pub num_refs: usize,    // How many times is this function called statically?
    pub num_calls: usize,   // How many times is this function used?
}

#[derive(Clone, Debug, PartialEq)]
pub struct TenMeta {
    pub data_type: Type,
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
}

impl TenMeta {
    pub fn with(sparsity: Sparsity, type_check: TypeCheck, min_max: MinMax) -> Self {
        let MinMax { max, min } = min_max;
        Self {
            data_type: type_check.get_only_type(),
            max: max,
            min: min,
            sparse: sparsity.is_sparse(),
            num_refs: 0,
            op_log: ThinVec::new(),
        }
    }
}

pub struct MinMax {
    max: Option<Value>,
    min: Option<Value>,
}

impl MinMax {
    pub fn new() -> Self {
        Self {
            max: None,
            min: None,
        }
    }
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

pub struct Sparsity(HashMap<Value, usize>, usize);

impl Sparsity {
    pub fn new() -> Self {
        Self(HashMap::new(), 0)
    }
    pub fn count(&mut self, value: &Value) {
        if self.0.contains_key(value) {
            self.0.entry(value.clone()).and_modify(|e| *e += 1);
        } else {
            self.0.insert(value.clone(), 1);
        }
        self.1 += 1;
    }
    pub fn is_sparse(self) -> bool {
        let whole = self.1;
        let threshold = (whole as f32 * 0.7) as usize;
        // TODO: record the most frequent
        self.0.iter().any(|entry| *entry.1 >= threshold)
    }
}

pub struct TypeCheck(HashSet<Type>);

impl TypeCheck {
    pub fn new() -> Self {
        Self(HashSet::new())
    }
    pub fn add(&mut self, value: &Value) {
        let t = Type::from(value);
        if !self.0.contains(&t) {
            self.0.insert(t);
        }
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

/*
   - inline_hint is for now true for every function that is shorter than
   certain lines
   - is_pure = !parameters.contain(mut)
   - is_recursive: if a function call has the same name or not
   - is_cyclic: form a graph, detect cycles
   - num_refs: how many times this function is called in source code i.e. referenced
   - num_calls: increment each time this is called
*/

/*
TODO:
- sparsity
- hashing
- min/max
 */
