use ast::expression::Expr as ASTExpr;

use crate::{
    HIRResult,
    builder::HIRBuilder,
    container_ref::ContainerRef,
    delimited::{Block, Indexing, Paren},
    function::FnCall,
    literal::Literal,
    mutable::Mut,
    operation::{BinaryInfix, Unary},
    resolution::Reference,
    scope::{ExprIdx, into_idx},
    self_ref::SelfRef,
    structure::StructRef,
    tensor::TensorStruct,
    unwrap_or_err,
    variable::VarRef,
};

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    TensorStruct(TensorStruct),
    Block(Block),
    // ContainerRef(ContainerRef),
    ContainerRef(Reference<ContainerRef>),
    // FnCall(FnCall),
    FnCall(Reference<FnCall>),
    Indexing(Indexing),
    Infix(BinaryInfix),
    Literal(Literal),
    Missing, // handles parser errors
    Mut(Mut),
    Paren(Paren),
    SelfRef(SelfRef),
    StructRef(Reference<StructRef>),
    Unary(Unary),
    VarRef(Reference<VarRef>),
    // VarRef(VarRef),
}

pub const MISSING: u32 = 0;

impl HIRBuilder {
    pub fn get_expr(&self, idx: ExprIdx) -> &Expr {
        // TODO: try to find by 'climbing' the branch of scopes
        let current_scope = self.get_current_scope();
        &current_scope.exprs[idx]
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
            ASTExpr::TensorStruct(tensor_struct) => {
                Expr::TensorStruct(self.lower_tensor_struct(tensor_struct)?)
            }
            ASTExpr::Unary(unary) => Expr::Unary(self.lower_unary_operation(unary)?),
            ASTExpr::VarRef(var_ref) => {
                let unresolved = self.lower_var_ref(var_ref)?;
                let idx = self.allocate_for_resolution(unresolved);
                Expr::VarRef(Reference::Unresolved(idx))
            }
        };
        Ok(self.as_expr_idx(expr))
    }

    pub fn as_expr_idx(&mut self, exp: Expr) -> ExprIdx {
        let current_scope = self.get_current_scope_mut();
        current_scope.allocate_expr(exp)
    }
}
