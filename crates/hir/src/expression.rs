use ast::expression::Expr as ASTExpr;

use crate::{
    HIRResult,
    builder::HIRBuilder,
    climbing::climb,
    delimited::{Block, Paren, Tuple},
    function::{Call, FnDef, MayNeedResolution},
    indexing::Indexing,
    let_binding::LetBinding,
    literal::Literal,
    mutable::Mut,
    operation::{BinaryInfix, Unary},
    resolution::Reference,
    scope::{ExprIdx, Scoped, ScopedExprIdx, into_idx},
    self_ref::SelfRef,
    typing::hindley_milner::types::Type,
    unwrap_or_err,
};

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub enum Expr {
    Block(Block),
    Class(Type),
    FnCall(Reference<FnDef>),
    LitCall(Call),
    Indexing(Indexing),
    Infix(BinaryInfix),
    Literal(Literal),
    Missing, // handles parser errors
    Mut(Mut),
    Paren(Paren),
    SelfRef(SelfRef),
    Tuple(Tuple),
    Unary(Unary),
    Unit,
    VarRef(Reference<LetBinding>),
}

impl Default for Expr {
    fn default() -> Self {
        Self::Missing
    }
}

pub const MISSING: u32 = 0;

impl HIRBuilder {
    pub fn get_expr(&self, idx: &ScopedExprIdx) -> &Expr {
        let scope = &self.scopes[idx.scope_idx];
        &scope.exprs[idx.elm]
    }
    pub fn infer_type_from_expr(&self, idx: ScopedExprIdx) -> Type {
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
    pub fn try_lowering_expr_as_idx(
        &mut self,
        ast_expr: Option<ASTExpr>,
    ) -> HIRResult<ScopedExprIdx> {
        let expr = unwrap_or_err(ast_expr.as_ref(), "expression")?;
        self.lower_expr_as_idx(expr)
    }
    pub fn try_lower_expr_as_idx_with_default(
        &mut self,
        ast_expr: Option<&ASTExpr>,
    ) -> HIRResult<ScopedExprIdx> {
        ast_expr
            .map(|expr| self.lower_expr_as_idx(expr))
            .unwrap_or(Ok(Scoped::new(
                self.get_current_scope_idx(),
                into_idx(MISSING),
            )))
    }
    pub fn lower_expr_as_idx(&mut self, from: &ASTExpr) -> HIRResult<ScopedExprIdx> {
        let is_pure: bool;
        let expr = match from {
            ASTExpr::Block(block) => {
                let block = self.lower_block(block)?;
                is_pure = self.is_block_pure(&block);
                Expr::Block(block)
            }
            ASTExpr::Call(fn_call) => match self.lower_call(fn_call)? {
                MayNeedResolution::Yes(unresolved) => {
                    is_pure = false;
                    let idx = self.allocate_for_resolution(unresolved);
                    Expr::FnCall(Reference::Unresolved(idx))
                }
                MayNeedResolution::No(call) => {
                    is_pure = self.is_call_pure(&call);
                    Expr::LitCall(call)
                }
            },
            ASTExpr::Class(ty) => {
                let t = self.lower_type(ty)?;
                is_pure = self.is_type_pure(&t);
                Expr::Class(t)
            }
            ASTExpr::Indexing(indexing) => {
                let lowered_indexing = self.lower_indexing(indexing)?;
                is_pure = self.is_indexing_pure(&lowered_indexing);
                Expr::Indexing(lowered_indexing)
            }
            ASTExpr::Infix(infix) => {
                let lowered_infix = self.lower_binary_operation(infix)?;
                is_pure = self.is_binary_infix_pure(&lowered_infix);
                Expr::Infix(lowered_infix)
            }
            ASTExpr::Literal(literal) => {
                let lowered_literal = self.lower_literal(literal)?;
                is_pure = self.is_value_pure(&lowered_literal.0);
                Expr::Literal(lowered_literal)
            }
            ASTExpr::Mut(mutable) => {
                let lowered_mutabble = self.lower_mut(mutable)?;
                is_pure = false;
                Expr::Mut(lowered_mutabble)
            }
            ASTExpr::Paren(paren) => {
                let lowered_paren = self.lower_paren(paren)?;
                is_pure = self.is_pure(&lowered_paren.0);
                Expr::Paren(lowered_paren)
            }
            ASTExpr::SelfRef(self_ref) => {
                is_pure = true;
                Expr::SelfRef(self.lower_self_ref(self_ref)?)
            }
            ASTExpr::Tuple(tuple) => {
                let lowered_tuple = self.lower_tuple(tuple)?;
                let scope_idx = lowered_tuple.0;
                let exprs_slice = lowered_tuple.1.as_slice();
                is_pure = self.is_expr_slice_pure(scope_idx, exprs_slice);
                Expr::Tuple(lowered_tuple)
            }
            ASTExpr::Unit => {
                is_pure = true;
                Expr::Unit
            }
            ASTExpr::Unary(unary) => {
                let lowered_unary = self.lower_unary_operation(unary)?;
                is_pure = self.is_unary_op_pure(&lowered_unary);
                Expr::Unary(self.lower_unary_operation(unary)?)
            }
            ASTExpr::VarRef(var_ref) => {
                // * Note: since we did't resolve it yet, we let user of the ref to assign
                is_pure = true;
                let unresolved = self.lower_var_ref(var_ref)?;
                let idx = self.allocate_for_resolution(unresolved);
                Expr::VarRef(Reference::Unresolved(idx))
            }
        };
        let idx = self.as_expr_idx(expr);
        if is_pure {
            self.set_as_pure(idx);
        }
        let scoped = Scoped::new(self.get_current_scope_idx(), idx);
        Ok(scoped)
    }

    pub fn as_expr_idx(&mut self, exp: Expr) -> ExprIdx {
        let current_scope = self.get_current_scope_mut();
        current_scope.allocate_expr(exp)
    }
}
