use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
};

use thin_vec::ThinVec;

use crate::{
    HIRResult,
    context::{LoweringContext, UsageContext},
    errors::HIRError,
    literal::Value,
    scope::{ScopeIdx, Span},
    typing::hindley_milner::types::Type,
};

pub type Usages = ThinVec<Usage>;
#[derive(Clone, Debug, PartialEq)]
pub struct Usage {
    pub kind: UsageContext,
    pub span: Span,
    pub scope_idx: ScopeIdx,
    pub stmt_idx: usize,
}

impl Usage {
    pub fn from(l_ctx: &LoweringContext, span: Span, stmt_idx: usize) -> Self {
        Self {
            kind: l_ctx.usage,
            span,
            scope_idx: l_ctx.scope_idx,
            stmt_idx,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct BlkMeta {
    pub is_pure: bool,                     // Is this block free of side effects?
    pub return_indices: ThinVec<usize>, // Indices of statements within the block that are returning a value
    pub capturing_indices: ThinVec<usize>, // Indices of statements within the block that capture variables from the parent scope
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarMeta {
    pub def: Usage,
    pub of_type: Type,
    pub usages: Usages,
    pub is_mut: bool,
    pub first_read_idx: Option<usize>,
    pub first_write_idx: Option<usize>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnMeta {
    pub def: Usage,
    pub usages: Usages,
    pub inline_hint: bool,  // Should this function be inlined?
    pub is_pure: bool,      // Is this function free of side effects?
    pub is_recursive: bool, // Does this function call itself?
    pub is_cyclic: bool,    // Is there a call cycle? (not sure if this is useful)
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructMeta {
    pub def: Usage,
    pub usages: Usages,
    pub num_fields: usize,
    // pub is_generic: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct SharedMeta {
    pub def: Usage,
    pub usages: Usages,
    pub espaces: bool,
    // pub is_generic: bool,
}

/*
   ? Escape analysis pass to analyze:
   •	Returned?
   •	Stored into another structure?
   •	Passed to other functions?
*/

/*
   - inline_hint is for now true for every function that is shorter than
   certain lines
   - is_pure = !parameters.contain(mut)
   - is_recursive: if a function call has the same name or not
   - is_cyclic: form a graph, detect cycles
   - num_refs: how many times this function is called in source code i.e. referenced
   - num_calls: increment each time this is called
*/
