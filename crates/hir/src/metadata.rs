use std::fmt::Debug;

use smol_str::SmolStr;
use thin_vec::ThinVec;

use crate::{
    builder::HIRBuilder,
    context::{LoweringContext, UsageContext},
    control_flow::Conditional,
    expression::Expr,
    function::Callable,
    literal::Value,
    purity::Purity,
    scope::{ExprIdx, ScopeIdx, Span, StrIdx},
    statement::Stmt,
    typing::hindley_milner::types::Type,
};

#[derive(Clone, Debug, PartialEq)]
pub struct SharedMeta {
    pub def: Usage,
    pub usages: Usages,
    pub escapes: bool,
    // pub is_generic: bool,
}
#[derive(Clone, Debug, Default, Eq, Hash, PartialEq, PartialOrd)]
pub struct Common {
    pub purity: Purity, // Is free of side effects?
    pub refs_as_stmt_indices: ThinVec<usize>,
}

pub type Usages = ThinVec<Usage>;
#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub struct Usage {
    pub kind: UsageContext,
    pub span: Span,
    pub scope_idx: ScopeIdx,
    // ! statements are in Scope now, it should be a Scoped index
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
    pub common: Common,
    pub return_indices: ThinVec<usize>, // Indices of statements within the block that are returning a value
    pub capturing_indices: ThinVec<usize>, // Indices of statements within the block that capture variables from the parent scope
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
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
    pub common: Common,
    pub def: Usage,
    pub usages: Usages,
    pub inline_hint: bool,  // Should this function be inlined?
    pub is_recursive: bool, // Does this function call itself?
    pub is_cyclic: bool,    // Is there a call cycle? (not sure if this is useful)
}

// * is recursive is painful to handle:
// *    a) if bottom up, I miss the recursion through bindings (if I bind f() to a, calling a() won't be considered)
// *    b) if top to bottom, I end up lots of retrieving a lot and traversing the whole tree again
// *    maybe during lowering, I can have a list of fns and lambdas that are deferred with certain info,
// *        so that after resolution pass, I can check for b. for cases where we bind inside a scope, we can remember
// *        them, (a -> foo), thus can check there.
impl HIRBuilder {
    // ! TODO: add graphs to analyse further for indirect recursions
    // ! e.g. a() calls b(), b() calls a()
    pub fn is_recursive(&self, callable: &Callable, name_idx: &StrIdx) -> bool {
        false // TODO: replace with actual graph traversal
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructMeta {
    pub def: Usage,
    pub usages: Usages,
    pub num_fields: usize,
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
