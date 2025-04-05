use ast::mutable::Mut as ASTMut;

use crate::{
    HIRResult,
    hir::{ExprIdx, HIRBuilder},
};

#[derive(Clone, Debug, PartialEq)]
pub struct Mut(ExprIdx);

impl HIRBuilder {
    pub fn lower_mut(&mut self, mutable: &ASTMut) -> HIRResult<Mut> {
        let expr_id = self.try_lowering_expr_as_idx(mutable.expr())?;
        Ok(Mut(expr_id))
    }
}
