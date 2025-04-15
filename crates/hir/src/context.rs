use crate::scope::ScopeIdx;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UsageContext {
    Ref, // borrowing
    RefMut,
    Deref,
    FnArg,
    Init,      // let x = 2;
    Moved,     // let y = x; where x is move only
    Mut,       // declared as mut
    Read,      // non-mut read
    ReadWrite, // x *= 2;
    Return,
    StructInit,
    Unknown,
    Write, // x = 2;
}

#[derive(Debug)]
pub struct LoweringContext {
    pub usage: UsageContext,
    pub statement_idx: usize,
    pub scope_idx: ScopeIdx,
}

impl LoweringContext {
    pub fn is_of_usage(&self, u: UsageContext) -> bool {
        self.usage == u
    }
}
