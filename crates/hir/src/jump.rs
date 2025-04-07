use ast::jump::Jump as ASTJump;

use crate::{HIRResult, builder::HIRBuilder, scope::ExprIdx};

// possible types are primitives + custom types i.e. structs
#[derive(Clone, Debug, PartialEq)]
pub enum Jump {
    Continue,
    Break(Option<ExprIdx>),
}

impl HIRBuilder {
    pub fn lower_jump(&mut self, jump: &ASTJump) -> HIRResult<Jump> {
        let j = match jump {
            ASTJump::Continue => Jump::Continue,
            ASTJump::Break(_) => {
                let inner = if let Some(expr) = jump.expr() {
                    Some(self.lower_expr_as_idx(&expr)?)
                } else {
                    None
                };
                Jump::Break(inner)
            }
        };
        Ok(j)
    }
}

#[cfg(test)]
mod tests {}
