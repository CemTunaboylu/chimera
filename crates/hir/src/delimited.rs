use ast::delimited::{Block as ASTBlock, Indexing as ASTIndexing, Paren as ASTParen};
use hir_macro::{scoped, with_context};

use thin_vec::ThinVec;

use crate::{
    HIRResult,
    builder::HIRBuilder,
    context::UsageContext,
    errors::HIRError,
    expression::Expr,
    scope::{ExprIdx, ScopeIdx, ScopeKind, placeholder_idx},
    statement::Stmt,
};

#[derive(Clone, Debug, PartialEq)]
pub struct Paren(pub ExprIdx);

// TODO: needs metadata as well
#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub struct Block {
    pub scope_idx: ScopeIdx,
    pub returns: ThinVec<usize>,
    pub statements: ThinVec<Stmt>,
}

impl Default for Block {
    fn default() -> Self {
        Self {
            scope_idx: placeholder_idx(),
            returns: Default::default(),
            statements: Default::default(),
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
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
        let mut returns = ThinVec::new();
        for stmt in statements {
            let low_stmt = self.lower_statement(stmt)?;
            match low_stmt {
                Stmt::Return(_) => {
                    returns.push(lowered_statements.len());
                }
                Stmt::Expr(idx) if !matches!(self.get_expr(idx), Expr::Missing) => {
                    returns.push(lowered_statements.len());
                }
                _ => {}
            }
            lowered_statements.push(low_stmt);
        }
        Ok(Block {
            scope_idx: self.current_scope_cursor,
            returns,
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
