use ast::semi::Semi as ASTSemi;

use crate::{HIRResult, builder::HIRBuilder, scope::ExprIdx};

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub struct Semi(ExprIdx);

impl HIRBuilder {
    pub fn lower_semi(&mut self, semi: &ASTSemi) -> HIRResult<Semi> {
        let expr_id = self.try_lowering_expr_as_idx(semi.expr())?;
        Ok(Semi(expr_id))
    }
}
