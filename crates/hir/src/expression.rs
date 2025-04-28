use ast::expression::Expr as ASTExpr;

use crate::{
    HIRResult,
    builder::HIRBuilder,
    climbing::climb,
    container::tensor::Tensor,
    container_ref::ContainerRef,
    delimited::{Block, Indexing, Paren},
    function::FnCall,
    literal::Literal,
    mutable::Mut,
    operation::{BinaryInfix, Unary},
    resolution::Reference,
    scope::{ExprIdx, into_idx},
    self_ref::SelfRef,
    structure::{StructInit, StructRef},
    typing::hindley_milner::types::Type,
    unwrap_or_err,
    variable::VarRef,
};

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Block(Block),
    ContainerRef(Reference<ContainerRef>),
    FnCall(Reference<FnCall>),
    Indexing(Indexing),
    Infix(BinaryInfix),
    Literal(Literal),
    Missing, // handles parser errors
    Mut(Mut),
    Paren(Paren),
    SelfRef(SelfRef),
    StructRef(Reference<StructRef>),
    StructInit(StructInit),
    Unary(Unary),
    VarRef(Reference<VarRef>),
}

pub const MISSING: u32 = 0;

impl HIRBuilder {
    pub fn get_expr(&self, idx: ExprIdx) -> &Expr {
        // TODO: try to find by 'climbing' the branch of scopes
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
        match self.get_expr(idx) {
            // TODO: if returning, infer the type from them
            Expr::Block(block) => todo!(),
            // TODO: not resolved yet, thus should be taken care of separately after the resolution pass
            Expr::ContainerRef(reference) => todo!(),
            // TODO: not resolved yet, thus should be taken care of separately after the resolution pass
            Expr::FnCall(reference) => todo!(),
            Expr::Indexing(indexing) => self.infer_type_from_expr(indexing.0),
            Expr::Infix(binary_infix) => todo!(),
            Expr::Literal(literal) => todo!(),
            Expr::Missing => todo!(),
            Expr::Mut(_) => todo!(),
            Expr::Paren(paren) => todo!(),
            Expr::SelfRef(self_ref) => todo!(),
            Expr::StructRef(reference) => todo!(),
            Expr::StructInit(struct_init) => todo!(),
            Expr::Unary(unary) => todo!(),
            Expr::VarRef(reference) => todo!(),
        }
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
    pub fn lower_expr_as_idx(&mut self, from: &ASTExpr) -> HIRResult<ExprIdx> {
        let expr = match from {
            ASTExpr::Block(block) => Expr::Block(self.lower_block(block)?),
            ASTExpr::ContainerRef(container_ref) => {
                let unresolved = self.lower_container_ref(container_ref)?;
                let idx = self.allocate_for_resolution(unresolved);
                Expr::ContainerRef(Reference::Unresolved(idx))
            }
            ASTExpr::FnCall(fn_call) => {
                let unresolved = self.lower_fn_call(fn_call)?;
                let idx = self.allocate_for_resolution(unresolved);
                Expr::FnCall(Reference::Unresolved(idx))
            }
            ASTExpr::Indexing(indexing) => Expr::Indexing(self.lower_indexing(indexing)?),
            ASTExpr::Infix(infix) => Expr::Infix(self.lower_binary_operation(infix)?),
            ASTExpr::Literal(literal) => Expr::Literal(self.lower_literal(literal)?),
            ASTExpr::Mut(mutable) => Expr::Mut(self.lower_mut(mutable)?),
            ASTExpr::Paren(paren) => Expr::Paren(self.lower_paren(paren)?),
            ASTExpr::SelfRef(self_ref) => Expr::SelfRef(self.lower_self_ref(self_ref)?),
            ASTExpr::StructInit(struct_init) => {
                Expr::StructInit(self.lower_struct_init(struct_init)?)
            }
            // TODO: container init
            // ASTExpr::TensorInit(tensor_struct) => {
            //     Expr::TensorInit(self.lower_tensor_init(tensor_struct)?)
            // }
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
