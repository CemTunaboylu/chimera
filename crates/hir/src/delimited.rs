use ast::delimited::{Block as ASTBlock, Indexing as ASTIndexing, Paren as ASTParen};
use thin_vec::ThinVec;

use crate::{
    HIRResult, clone_thin_vec, compare_thin_vecs,
    errors::HIRError,
    hir::{ExprIdx, HIRBuilder},
    statement::Stmt,
};

#[derive(Clone, Debug, PartialEq)]
pub struct Paren(ExprIdx);

impl Paren {
    pub fn expr(&self) -> ExprIdx {
        self.0
    }
}
#[derive(Debug)]
pub enum Block {
    Semi(ThinVec<Stmt>),
    Returning(ThinVec<Stmt>),
}

impl Clone for Block {
    fn clone(&self) -> Self {
        match self {
            Block::Semi(thin_vec) => {
                let inside = clone_thin_vec(thin_vec);
                Self::Semi(inside)
            }
            Block::Returning(thin_vec) => {
                let inside = clone_thin_vec(thin_vec);
                Self::Returning(inside)
            }
        }
    }
}

impl PartialEq for Block {
    fn eq(&self, other: &Self) -> bool {
        let (lhs, rhs) = match (self, other) {
            (Block::Semi(thin_vec_self), Block::Semi(thin_vec_other)) => {
                (thin_vec_self, thin_vec_other)
            }
            (Block::Returning(thin_vec_self), Block::Returning(thin_vec_other)) => {
                (thin_vec_self, thin_vec_other)
            }
            _ => return false,
        };
        compare_thin_vecs(rhs, lhs)
    }
}

impl Block {
    pub fn statements(&self) -> &ThinVec<Stmt> {
        match self {
            Self::Returning(stmts) => stmts,
            Self::Semi(stmts) => stmts,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Indexing(pub(crate) ExprIdx);

impl HIRBuilder {
    pub fn lower_paren(&mut self, paren: &ASTParen) -> HIRResult<Paren> {
        let index = self.lower_expr_as_idx(&paren.expr().map_err(HIRError::from_err)?)?;
        Ok(Paren(index))
    }
    pub fn lower_block(&mut self, block: &ASTBlock) -> HIRResult<Block> {
        let statements = block.statements();
        let mut lowered_statements = ThinVec::with_capacity(statements.len());
        for stmt in statements {
            let low_stmt = self.lower_statement(&stmt)?;
            lowered_statements.push(low_stmt);
        }
        let blk = match block {
            ASTBlock::Semi(_) => Block::Semi(lowered_statements),
            ASTBlock::Returning(_) => Block::Returning(lowered_statements),
        };
        Ok(blk)
    }
    pub fn lower_indexing(&mut self, indexing: &ASTIndexing) -> HIRResult<Indexing> {
        let index = self.lower_expr_as_idx(&indexing.index().map_err(HIRError::from_err)?)?;
        Ok(Indexing(index))
    }
}

#[cfg(test)]
mod test {
    use super::*;
}
