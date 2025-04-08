use crate::scope::ScopeIdx;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UsageContext {
    AsRef, // borrowing
    AsRefMut,
    FunctionArg,
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
