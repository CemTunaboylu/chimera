use ast::{
    delimited::{Block as ASTBlock, Paren as ASTParen, Tuple as ASTTuple},
    expression::Expr as ASTExpr,
};
use hir_macro::scoped;

use thin_vec::ThinVec;

use crate::{
    HIRResult,
    builder::HIRBuilder,
    errors::HIRError,
    expression::Expr,
    index_types::{ExprIdx, ScopeIdx, ScopedExprIdx, ScopedStmtIdx, placeholder_idx},
    mut_clone_with_err,
    scope::ScopeKind,
};

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub struct Paren(pub ScopedExprIdx);

// TODO: needs metadata as well
#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub struct Block {
    pub is_pure: bool,
    pub scope_idx: ScopeIdx,
    pub returns: ThinVec<ScopedStmtIdx>,
}

impl Default for Block {
    fn default() -> Self {
        Self {
            is_pure: Default::default(),
            scope_idx: placeholder_idx(),
            returns: Default::default(),
        }
    }
}
#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub struct Tuple(pub(crate) ScopeIdx, pub(crate) ThinVec<ExprIdx>);

impl HIRBuilder {
    #[scoped(ScopeKind::Block)]
    pub fn lower_block(&mut self, block: &ASTBlock) -> HIRResult<Block> {
        let statements = block.statements();
        let mut returns = ThinVec::new();

        let mut is_pure = true;

        for stmt in statements {
            let low_stmt_idx = self.lower_statement_as_idx(stmt)?;
            // * TODO: indexing multiple times, fix this
            is_pure &= self.is_stmt_of_idx_pure(&low_stmt_idx);
            if self.does_statement_of_idx_return(&low_stmt_idx) {
                returns.push(low_stmt_idx);
            }
        }
        Ok(Block {
            is_pure,
            scope_idx: self.current_scope_cursor,
            returns,
        })
    }
    pub fn lower_paren(&mut self, paren: &ASTParen) -> HIRResult<Paren> {
        let index = self.lower_expr_as_idx(&paren.expr().map_err(HIRError::from_err)?)?;
        Ok(Paren(index))
    }
    pub fn lower_tuple(&mut self, tuple: &ASTTuple) -> HIRResult<Tuple> {
        let to_expr =
            |hc: &ASTExpr, hir: &mut HIRBuilder| hir.lower_expr_as_idx(hc).map(|scoped| scoped.elm);
        let ast_exprs_in_tuple = tuple
            .elements()
            .map_err(|ast_err| HIRError::for_ast(ast_err, "a valid expression within tuple"))?;
        let expr_idx = mut_clone_with_err(ast_exprs_in_tuple.as_ref(), self, to_expr)?;
        let scope_idx = self.get_current_scope_idx();
        Ok(Tuple(scope_idx, expr_idx))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{
        builder::HIRBuilder,
        index_types::{Scoped, into_idx},
    };
    use ast::{
        ast_root_from_assert_no_err, cast_node_into_type, delimited::Tuple as ASTTuple,
        expression::Expr as ASTExpr,
    };
    use parameterized_test::create;

    fn get_hir_builder_and_ast_root(program: &str) -> (ASTTuple, HIRBuilder) {
        let ast_root = ast_root_from_assert_no_err(program);
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
        nested_tuples_with_unit: ("((), (),(), )", &[|ht: &Tuple, _| { assert_eq!( ht.1.len(), 3);}]),
        flat_tuple: ("(1,2,3)", &[|ht: &Tuple, _| { assert_eq!( ht.1.len(), 3);}] ),
        nested_tuple: ("((1,2),3)", &[|ht: &Tuple, _| { assert_eq!( ht.1.len(), 2);}] ),
        tuple_with_exprs: ( "(3+5, 2*4)", &[
        |ht: &Tuple, _| {assert_eq!(ht.1.len(), 2)},
        |ht: &Tuple, hir: &HIRBuilder| {
            for expr_idx in ht.1.iter() {
                let scoped = Scoped::new(ht.0,*expr_idx);
                let expr = hir.get_expr(&scoped);
                assert!(matches!(expr, crate::expression::Expr::Infix(_)));
            }
        }]),
    }

    #[test]
    fn unit_tuple_is_unit_expr() {
        let program = "()";

        let ast_root = ast_root_from_assert_no_err(program);
        let node = ast_root.get_root().first_child().unwrap();
        let ast_expr = cast_node_into_type::<ast::expression::Expr>(&node);
        assert!(matches!(ast_expr, ASTExpr::Unit));

        let mut hir = HIRBuilder::new(ast_root);

        let unit = hir
            .lower_expr_as_idx(&ast_expr)
            .expect("should have been able to lower to unit");
        assert_eq!(unit.elm, into_idx(1));
        assert_eq!(unit.scope_idx, into_idx(0));

        let scope = hir.get_current_scope();
        let exprs = &scope.exprs;

        let lowered = &exprs[unit.elm];
        assert_eq!(lowered, &Expr::Unit);
    }
}
