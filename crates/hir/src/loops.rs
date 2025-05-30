use hir_macro::with_context;
use smol_str::SmolStr;
use thin_vec::ThinVec;

use crate::{
    HIRResult, builder::HIRBuilder, context::UsageContext, control_flow::Condition,
    delimited::Block, scope::ExprIdx,
};

use ast::loops::{In as ASTIn, Loop as ASTLoop};

// TODO: do I support multiple var defs at once?
#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub struct Identifiers(pub ThinVec<SmolStr>);

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub struct In(ExprIdx);

// TODO: lower loops into a single structure
#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub enum Loop {
    While(Condition, Block),
    For(Identifiers, In, Block),
}

impl Loop {}

impl HIRBuilder {
    // TODO: Is it Read though? Isn't it also ref for the elements within?
    #[with_context(UsageContext::Read)]
    pub fn lower_in(&mut self, in_: &ASTIn) -> HIRResult<In> {
        let expr_id = self.try_lowering_expr_as_idx(in_.expr())?;
        Ok(In(expr_id))
    }
    pub fn lower_loop(&mut self, ast_loop: &ASTLoop) -> HIRResult<Loop> {
        match ast_loop {
            ASTLoop::While(condition, block) => {
                let cond = self.lower_condition(condition)?;
                let block = self.lower_block(block)?;
                Ok(Loop::While(cond, block))
            }
            ASTLoop::For(identifiers, _in, block) => {
                let ident = Identifiers(identifiers.0.clone());
                let in_ = self.lower_in(_in)?;
                let block = self.lower_block(block)?;
                Ok(Loop::For(ident, in_, block))
            }
        }
    }
}

#[cfg(test)]
mod test {}
