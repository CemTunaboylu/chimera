use ast::{
    delimited::{Block as ASTBlock, Indexing as ASTIndexing, Paren as ASTParen, Tuple as ASTTuple},
    expression::Expr as ASTExpr,
};
use hir_macro::{scoped, with_context};

use thin_vec::ThinVec;

use crate::{
    HIRResult,
    builder::HIRBuilder,
    context::UsageContext,
    errors::HIRError,
    expression::Expr,
    mut_clone_with_err,
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

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub struct Tuple(pub(crate) ThinVec<ExprIdx>);

impl HIRBuilder {
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
    pub fn lower_paren(&mut self, paren: &ASTParen) -> HIRResult<Paren> {
        let index = self.lower_expr_as_idx(&paren.expr().map_err(HIRError::from_err)?)?;
        Ok(Paren(index))
    }
    pub fn lower_tuple(&mut self, tuple: &ASTTuple) -> HIRResult<Tuple> {
        let to_expr = |hc: &ASTExpr, hir: &mut HIRBuilder| hir.lower_expr_as_idx(hc);
        let ast_exprs_in_tuple = tuple
            .elements()
            .map_err(|ast_err| HIRError::for_ast(ast_err, "a valid expression within tuple"))?;
        let expr_idx = mut_clone_with_err(ast_exprs_in_tuple.as_ref(), self, to_expr)?;
        Ok(Tuple(expr_idx))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{builder::HIRBuilder, scope::into_idx};
    use ast::{
        ast_root_from, cast_node_into_type, delimited::Tuple as ASTTuple,
        expression::Expr as ASTExpr,
    };
    use parameterized_test::create;

    fn get_hir_builder_and_ast_root(program: &str) -> (ASTTuple, HIRBuilder) {
        let ast_root = ast_root_from(program);
        let node = ast_root.get_root().first_child().unwrap();
        let ast_expr = cast_node_into_type::<ast::expression::Expr>(&node);
        let ast_tuple = if let ASTExpr::Tuple(tuple) = ast_expr {
            tuple
        } else {
            panic!("expected a tuple expression")
        };

        let hir = HIRBuilder::new(ast_root);
        (ast_tuple, hir)
    }

    create! {
        lower_tuple,
        (program, assertions), {
            let (ast_tuple, mut hir) = get_hir_builder_and_ast_root(program);
            let hir_tuple = hir.lower_tuple(&ast_tuple).expect("should have been able to lower to tuple");
            for ass in assertions {
                ass(&hir_tuple, &hir)
            }
        }
    }

    lower_tuple! {
        nested_tuples_with_unit: ("((), (),(), )", &[|ht: &Tuple, _| { assert_eq!( ht.0.len(), 3);}]),
        flat_tuple: ("(1,2,3)", &[|ht: &Tuple, _| { assert_eq!( ht.0.len(), 3);}] ),
        nested_tuple: ("((1,2),3)", &[|ht: &Tuple, _| { assert_eq!( ht.0.len(), 2);}] ),
        tuple_with_exprs: ( "(3+5, 2*4)", &[
        |ht: &Tuple, _| {assert_eq!(ht.0.len(), 2)},
        |ht: &Tuple, hir: &HIRBuilder| {
            for expr_idx in ht.0.iter() {
                let expr = hir.get_expr(*expr_idx);
                assert!(matches!(expr, crate::expression::Expr::Infix(_)));
            }
        }]),
    }

    #[test]
    fn unit_tuple_is_unit_expr() {
        let program = "()";

        let ast_root = ast_root_from(program);
        let node = ast_root.get_root().first_child().unwrap();
        let ast_expr = cast_node_into_type::<ast::expression::Expr>(&node);
        assert!(matches!(ast_expr, ASTExpr::Unit));

        let mut hir = HIRBuilder::new(ast_root);

        let unit = hir
            .lower_expr_as_idx(&ast_expr)
            .expect("should have been able to lower to unit");
        assert_eq!(unit, into_idx(1));

        let scope = hir.get_current_scope();
        let exprs = &scope.exprs;

        let lowered = &exprs[unit];
        assert_eq!(lowered, &Expr::Unit);
    }
}
