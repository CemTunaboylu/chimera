use ast::mutable::Mut as ASTMut;
use hir_macro::with_context;

use crate::{HIRResult, builder::HIRBuilder, context::UsageContext, scope::ExprIdx};

#[derive(Clone, Debug, PartialEq)]
pub struct Mut(pub ExprIdx);

impl HIRBuilder {
    #[with_context(UsageContext::Mut)]
    pub fn lower_mut(&mut self, mutable: &ASTMut) -> HIRResult<Mut> {
        let expr_id = self.try_lowering_expr_as_idx(mutable.expr())?;
        Ok(Mut(expr_id))
    }
}
