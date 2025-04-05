use ast::expression::Expr as ASTExpr;

use crate::{
    HIRResult,
    container_ref::ContainerRef,
    delimited::{Block, Indexing, Paren},
    function::FnCall,
    hir::{ExprIdx, HIRBuilder, MISSING, UnresolvedFnCallIdx, UnresolvedVarRefIdx, into_idx},
    literal::Literal,
    mutable::Mut,
    operation::{BinaryInfix, Unary},
    self_ref::SelfRef,
    unwrap_or_err,
    variable::VarRef,
};

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    // TensorStruct(ASTTensorStruct),
    Block(Block),
    ContainerRef(ContainerRef),
    FnCall(FnCall),
    Indexing(Indexing),
    Infix(BinaryInfix),
    Literal(Literal),
    Missing, // handles parser errors
    Mut(Mut),
    Paren(Paren),
    SelfRef(SelfRef),
    Unary(Unary),
    // TODO: I may separate this as Reference enum with Resolved, Unresolved variants.
    Unresolved(Unresolved),
    VarRef(VarRef),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Unresolved {
    VarRef(UnresolvedVarRefIdx),
    FnCall(UnresolvedFnCallIdx),
}

impl HIRBuilder {
    pub fn get_expr(&self, idx: ExprIdx) -> &Expr {
        &self.expr_arena[idx]
    }
    pub fn try_lowering_expr_as_idx(&mut self, ast_expr: Option<ASTExpr>) -> HIRResult<ExprIdx> {
        let expr = unwrap_or_err(ast_expr.as_ref(), "expression")?;
        self.lower_expr_as_idx(&expr)
    }

    pub fn try_lower_expr_as_idx_with_default(
        &mut self,
        ast_expr: Option<&ASTExpr>,
    ) -> HIRResult<ExprIdx> {
        ast_expr
            .map(|expr| self.lower_expr_as_idx(expr))
            .unwrap_or(Ok(into_idx(MISSING)))
    }
    // pub fn lower_expr_as_idx(&mut self, from: Option<&AstExpr>) -> HIRResult<ExprIdx> {
    pub fn lower_expr_as_idx(&mut self, from: &ASTExpr) -> HIRResult<ExprIdx> {
        // if from.is_none() {
        //     return Ok(self.missing_idx);
        // }
        // let from = from.unwrap();
        let expr = match from {
            ASTExpr::Block(block) => Expr::Block(self.lower_block(block)?),
            ASTExpr::ContainerRef(container_ref) => {
                Expr::ContainerRef(self.lower_container_ref(container_ref)?)
            }
            ASTExpr::FnCall(fn_call) => {
                let unresolved = Unresolved::FnCall(self.lower_fn_call(fn_call)?);
                Expr::Unresolved(unresolved)
            }
            ASTExpr::Indexing(indexing) => Expr::Indexing(self.lower_indexing(indexing)?),
            ASTExpr::Infix(infix) => Expr::Infix(self.lower_binary_operation(infix)?),
            ASTExpr::Literal(literal) => Expr::Literal(self.lower_literal(literal)?),
            ASTExpr::Mut(mutable) => Expr::Mut(self.lower_mut(mutable)?),
            ASTExpr::Paren(paren) => Expr::Paren(self.lower_paren(paren)?),
            ASTExpr::SelfRef(self_ref) => Expr::SelfRef(self.lower_self_ref(self_ref)?),
            ASTExpr::Unary(unary) => Expr::Unary(self.lower_unary_operation(unary)?),
            ASTExpr::VarRef(var_ref) => {
                let unresolved = Unresolved::VarRef(self.lower_var_ref(var_ref)?);
                Expr::Unresolved(unresolved)
            }
            _ => Expr::Missing,
        };
        Ok(self.as_expr_idx(expr))
    }
    pub fn as_expr_idx(&mut self, exp: Expr) -> ExprIdx {
        self.expr_arena.alloc(exp)
    }
}
