pub mod builder;
pub mod climbing;
pub mod container;
pub mod container_ref;
pub mod context;
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
pub mod types;
pub mod typing;
pub mod variable;

use builder::HIRBuilder;
use errors::HIRError;
use resolution::Baggage;
use thin_vec::ThinVec;

use miette::Report;
use typing::hindley_milner::inference::TypeKey;

pub type HIRResult<T> = Result<T, Report>;

pub fn err_if_none<Any>(any: Option<&Any>, err_msg: &str) -> HIRResult<()> {
    if any.is_some() {
        return Ok(());
    }
    Err(HIRError::with_msg(err_msg).into())
}

pub fn unwrap_or_err<'caller, Any>(
    any: Option<&'caller Any>,
    err_msg: &str,
) -> HIRResult<&'caller Any> {
    if let Some(some) = any {
        return Ok(some);
    }
    Err(HIRError::with_msg(err_msg).into())
}

pub fn expect_non_baggage(b: &Baggage, type_key: TypeKey) -> HIRResult<()> {
    match b {
        Baggage::None => Ok(()),
        _ => Err(HIRError::with_msg(format!("{:?} does not expect a baggage", type_key)).into()),
    }
}

pub fn clone_from_iter_with_err<Any: Clone, Post>(
    tv: impl Iterator<Item = Any>,
    len: usize,
    payload: fn(a: Any) -> HIRResult<Post>,
) -> HIRResult<ThinVec<Post>> {
    let mut clone: ThinVec<Post> = ThinVec::with_capacity(len);
    for c in tv {
        let put = payload(c)?;
        clone.push(put);
    }
    Ok(clone)
}

pub fn clone_with_err<Any: Clone, Post>(
    tv: &[Any],
    hir: &HIRBuilder,
    payload: fn(a: &Any, &HIRBuilder) -> HIRResult<Post>,
) -> HIRResult<ThinVec<Post>> {
    let mut clone: ThinVec<Post> = ThinVec::with_capacity(tv.len());
    for c in tv {
        let put = payload(c, hir)?;
        clone.push(put);
    }
    Ok(clone)
}

pub fn mut_clone_with_err<Any: Clone, Post>(
    tv: &[Any],
    hir: &mut HIRBuilder,
    payload: fn(a: &Any, &mut HIRBuilder) -> HIRResult<Post>,
) -> HIRResult<ThinVec<Post>> {
    let mut clone: ThinVec<Post> = ThinVec::with_capacity(tv.len());
    for c in tv {
        let put = payload(c, hir)?;
        clone.push(put);
    }
    Ok(clone)
}
