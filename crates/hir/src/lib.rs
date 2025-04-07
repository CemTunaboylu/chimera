pub mod builder;
pub mod container_ref;
pub mod control_flow;
pub mod delimited;
pub mod errors;
pub mod expression;
pub mod function;
pub mod hir;
pub mod impl_block;
pub mod jump;
pub mod literal;
pub mod loops;
pub mod metadata;
pub mod mutable;
pub mod operation;
pub mod parameter;
pub mod resolution;
pub mod return_stmt;
pub mod scope;
pub mod self_ref;
pub mod semi;
pub mod statement;
pub mod structure;
pub mod tensor;
pub mod types;
pub mod variable;

use errors::HIRError;
use thin_vec::ThinVec;

use miette::Report;

pub type HIRResult<T> = Result<T, Report>;

pub fn clone_thin_vec<T: Clone>(thin_vec: &ThinVec<T>) -> ThinVec<T> {
    thin_vec.iter().map(|e| e.clone()).collect::<ThinVec<_>>()
}
pub fn compare_thin_vecs<T: PartialEq>(a: &ThinVec<T>, b: &ThinVec<T>) -> bool {
    a.len() == b.len() && a.starts_with(b.as_ref())
}

pub fn err_if_none<Any>(any: Option<&Any>, err_msg: &str) -> HIRResult<()> {
    if any.is_some() {
        return Ok(());
    }
    return Err(HIRError::with_msg(err_msg).into());
}

pub fn unwrap_or_err<'caller, Any>(
    any: Option<&'caller Any>,
    err_msg: &str,
) -> HIRResult<&'caller Any> {
    if let Some(some) = any {
        return Ok(some);
    }
    return Err(HIRError::with_msg(err_msg).into());
}

#[cfg(test)]
mod tests {
    use super::*;
}
