use ast::expression::Expr as ASTExpr;

use crate::{
    HIRResult,
    builder::HIRBuilder,
    climbing::climb,
    delimited::{Block, Paren, Tuple},
    function::{Call, MayNeedResolution},
    indexing::Indexing,
    let_binding::{LetBinding, VarRef},
    literal::Literal,
    mutable::Mut,
    operation::{BinaryInfix, Unary},
    resolution::Reference,
    scope::{ExprIdx, into_idx},
    self_ref::SelfRef,
    structure::StructRef,
    typing::hindley_milner::types::Type,
    unwrap_or_err,
};

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub enum Expr {
    Block(Block),
    FnCall(Reference<Call>),
    LitCall(Call),
    Indexing(Indexing),
    Infix(BinaryInfix),
    Literal(Literal),
    Missing, // handles parser errors
    Mut(Mut),
    Paren(Paren),
    SelfRef(SelfRef),
    StructRef(Reference<StructRef>),
    Tuple(Tuple),
    Unary(Unary),
    Unit,
    VarRef(Reference<LetBinding>),
}

pub const MISSING: u32 = 0;

impl HIRBuilder {
    pub fn get_expr(&self, idx: ExprIdx) -> &Expr {
        let climber = climb(self.current_scope_cursor, &self.scopes);
        let mut expr = &Expr::Missing;
        for (_, scope) in climber {
            if scope.exprs.len() <= idx.into_raw().into_u32() as usize {
                continue;
            }
            expr = &scope.exprs[idx];
            break;
        }
        expr
    }
    pub fn infer_type_from_expr(&self, idx: ExprIdx) -> Type {
        // match self.get_expr(idx) {
        // TODO: if returning, infer the type from them
        // Expr::Block(block) => todo!(),
        // TODO: not resolved yet, thus should be taken care of separately after the resolution pass
        // Expr::ContainerRef(reference) => todo!(),
        // TODO: not resolved yet, thus should be taken care of separately after the resolution pass
        // Expr::FnCall(reference) => todo!(),
        // Expr::Indexing(indexing) => self.infer_type_from_expr(indexing.0),
        // Expr::Infix(binary_infix) => todo!(),
        // Expr::Literal(literal) => todo!(),
        // Expr::Missing => todo!(),
        // Expr::Mut(_) => todo!(), // Expr::Paren(paren) => todo!(),
        // Expr::SelfRef(self_ref) => todo!(),
        // Expr::StructRef(reference) => todo!(),
        // Expr::Unary(unary) => todo!(),
        // Expr::VarRef(reference) => todo!(),
        // }
        todo!()
    }
    pub fn try_lowering_expr_as_idx(&mut self, ast_expr: Option<ASTExpr>) -> HIRResult<ExprIdx> {
        let expr = unwrap_or_err(ast_expr.as_ref(), "expression")?;
        self.lower_expr_as_idx(expr)
    }
    pub fn try_lower_expr_as_idx_with_default(
        &mut self,
        ast_expr: Option<&ASTExpr>,
    ) -> HIRResult<ExprIdx> {
        ast_expr
            .map(|expr| self.lower_expr_as_idx(expr))
            .unwrap_or(Ok(into_idx(MISSING)))
    }
    pub fn lower_expr_as_idx(&mut self, from: &ASTExpr) -> HIRResult<ExprIdx> {
        let expr = match from {
            ASTExpr::Block(block) => Expr::Block(self.lower_block(block)?),
            ASTExpr::Call(fn_call) => match self.lower_call(fn_call)? {
                MayNeedResolution::Yes(unresolved) => {
                    let idx = self.allocate_for_resolution(unresolved);
                    Expr::FnCall(Reference::Unresolved(idx))
                }
                MayNeedResolution::No(call) => Expr::LitCall(call),
            },
            ASTExpr::Indexing(indexing) => Expr::Indexing(self.lower_indexing(indexing)?),
            ASTExpr::Infix(infix) => Expr::Infix(self.lower_binary_operation(infix)?),
            ASTExpr::Literal(literal) => Expr::Literal(self.lower_literal(literal)?),
            ASTExpr::Mut(mutable) => Expr::Mut(self.lower_mut(mutable)?),
            ASTExpr::Paren(paren) => Expr::Paren(self.lower_paren(paren)?),
            ASTExpr::SelfRef(self_ref) => Expr::SelfRef(self.lower_self_ref(self_ref)?),
            ASTExpr::Tuple(tuple) => Expr::Tuple(self.lower_tuple(tuple)?),
            ASTExpr::Unit => Expr::Unit,
            ASTExpr::Unary(unary) => Expr::Unary(self.lower_unary_operation(unary)?),
            ASTExpr::VarRef(var_ref) => {
                let unresolved = self.lower_var_ref(var_ref)?;
                let idx = self.allocate_for_resolution(unresolved);
                Expr::VarRef(Reference::Unresolved(idx))
            }
            _ => todo!(),
        };
        Ok(self.as_expr_idx(expr))
    }

    pub fn as_expr_idx(&mut self, exp: Expr) -> ExprIdx {
        let current_scope = self.get_current_scope_mut();
        current_scope.allocate_expr(exp)
    }
}
