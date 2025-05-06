use ast::jump::Jump as ASTJump;
use hir_macro::with_context;

use crate::{HIRResult, builder::HIRBuilder, context::UsageContext, scope::ExprIdx};

// possible types are primitives + custom types i.e. structs
#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub enum Jump {
    Continue,
    Break(Option<ExprIdx>),
}

impl HIRBuilder {
    #[with_context(UsageContext::Return)]
    pub fn lower_break(&mut self, jump: &ASTJump) -> HIRResult<Jump> {
        let inner = if let Some(expr) = jump.expr() {
            Some(self.lower_expr_as_idx(&expr)?)
        } else {
            None
        };
        Ok(Jump::Break(inner))
    }
    pub fn lower_jump(&mut self, jump: &ASTJump) -> HIRResult<Jump> {
        let j = match jump {
            ASTJump::Continue => Jump::Continue,
            ASTJump::Break(_) => self.lower_break(jump)?,
        };
        Ok(j)
    }
}

#[cfg(test)]
mod tests {}
