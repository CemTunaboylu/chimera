use hir_macro::with_context;
use thin_vec::ThinVec;

use ast::control_flow::{
    Condition as ASTCondition, Conditional as ASTConditional, ControlFlow as ASTControlFlow,
};

use crate::{
    HIRResult, builder::HIRBuilder, context::UsageContext, delimited::Block, scope::ExprIdx,
};

#[derive(Clone, Debug, PartialEq)]
pub struct Condition(ExprIdx);

#[derive(Clone, Debug, PartialEq)]
pub enum Conditional {
    If(Condition, Block),
    Elif(Condition, Block),
    Else(Block),
}

#[derive(Clone, Debug, PartialEq)]
pub struct ControlFlow(ThinVec<Conditional>);

impl HIRBuilder {
    #[with_context(UsageContext::Read)]
    pub fn lower_condition(&mut self, cond: &ASTCondition) -> HIRResult<Condition> {
        let expr_id = self.lower_expr_as_idx(cond.expr())?;
        Ok(Condition(expr_id))
    }
    pub fn lower_conditional(&mut self, cond: &ASTConditional) -> HIRResult<Conditional> {
        match cond {
            ASTConditional::If(condition, block) => {
                let cond = self.lower_condition(condition)?;
                let block = self.lower_block(block)?;
                Ok(Conditional::If(cond, block))
            }
            ASTConditional::Elif(condition, block) => {
                let cond = self.lower_condition(condition)?;
                let block = self.lower_block(block)?;
                Ok(Conditional::Elif(cond, block))
            }
            ASTConditional::Else(block) => {
                let block = self.lower_block(block)?;
                Ok(Conditional::Else(block))
            }
        }
    }
    pub fn lower_control_flow(&mut self, c_flow: &ASTControlFlow) -> HIRResult<ControlFlow> {
        let mut lowered = ThinVec::with_capacity(c_flow.0.len());
        for c in c_flow.0.iter() {
            let low_cond = self.lower_conditional(c)?;
            lowered.push(low_cond);
        }
        Ok(ControlFlow(lowered))
    }
}

#[cfg(test)]
mod test {}
