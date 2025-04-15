use ast::delimited::{Block as ASTBlock, Indexing as ASTIndexing, Paren as ASTParen};
use hir_macro::{scoped, with_context};

use thin_vec::ThinVec;

use crate::{
    HIRResult,
    builder::HIRBuilder,
    context::UsageContext,
    errors::HIRError,
    scope::{ExprIdx, ScopeIdx, ScopeKind},
    statement::Stmt,
};

#[derive(Clone, Debug, PartialEq)]
pub struct Paren(ExprIdx);

impl Paren {
    pub fn expr(&self) -> ExprIdx {
        self.0
    }
}
// TODO: needs metadata as well
#[derive(Clone, Debug, PartialEq)]
pub struct Block {
    scope_idx: ScopeIdx,
    is_returning: bool,
    statements: ThinVec<Stmt>,
}

impl Block {
    pub fn statements(&self) -> &ThinVec<Stmt> {
        &self.statements
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Indexing(pub(crate) ExprIdx);

impl HIRBuilder {
    pub fn lower_paren(&mut self, paren: &ASTParen) -> HIRResult<Paren> {
        let index = self.lower_expr_as_idx(&paren.expr().map_err(HIRError::from_err)?)?;
        Ok(Paren(index))
    }
    #[scoped(ScopeKind::Block)]
    pub fn lower_block(&mut self, block: &ASTBlock) -> HIRResult<Block> {
        let statements = block.statements();
        let mut lowered_statements = ThinVec::with_capacity(statements.len());
        for stmt in statements {
            let low_stmt = self.lower_statement(&stmt)?;
            lowered_statements.push(low_stmt);
        }
        let is_returning = matches!(block, ASTBlock::Returning(_));
        Ok(Block {
            scope_idx: self.current_scope_cursor,
            is_returning,
            statements: lowered_statements,
        })
    }
    #[with_context(UsageContext::Read)]
    pub fn lower_indexing(&mut self, indexing: &ASTIndexing) -> HIRResult<Indexing> {
        let index = self.lower_expr_as_idx(&indexing.index().map_err(HIRError::from_err)?)?;
        Ok(Indexing(index))
    }
}

#[cfg(test)]
mod test {}
