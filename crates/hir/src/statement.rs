use ast::statement::Stmt as ASTStmt;

use crate::{
    HIRResult,
    builder::HIRBuilder,
    control_flow::ControlFlow,
    impl_block::Impl,
    jump::Jump,
    loops::Loop,
    return_stmt::Return,
    scope::{ExprIdx, FnDefIdx, StructDefIdx, VarDefIdx},
    semi::Semi,
};

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub enum Stmt {
    ControlFlow(ControlFlow),
    Expr(ExprIdx),
    // Expr(Expr),
    FnDef(FnDefIdx),
    // FnDef(FnDef),
    Impl(Impl),
    Jump(Jump),
    Loop(Loop),
    Return(Return),
    Semi(Semi),
    StructDef(StructDefIdx),
    VarDef(VarDefIdx),
}

impl HIRBuilder {
    pub fn lower_statement(&mut self, stmt: &ASTStmt) -> HIRResult<Stmt> {
        let lowered = match stmt {
            ASTStmt::ControlFlow(c_flow) => {
                let low_c_flow = self.lower_control_flow(c_flow)?;
                Stmt::ControlFlow(low_c_flow)
            }
            ASTStmt::Expr(expr) => {
                let expr_id = self.lower_expr_as_idx(expr)?;
                // let low_expr = self.expr_arena[expr_id];
                // Stmt::Expr(low_expr)
                Stmt::Expr(expr_id)
            }
            ASTStmt::FnDef(fn_def) => {
                let low_fn_def = self.lower_fn_def(fn_def)?;
                Stmt::FnDef(low_fn_def)
            }
            ASTStmt::Impl(imp) => {
                let low_impl = self.lower_impl_block(imp)?;
                Stmt::Impl(low_impl)
            }
            ASTStmt::Jump(jump) => {
                let low_jump = self.lower_jump(jump)?;
                Stmt::Jump(low_jump)
            }
            ASTStmt::Loop(looop) => {
                let low_loop = self.lower_loop(looop)?;
                Stmt::Loop(low_loop)
            }
            ASTStmt::Return(ret) => {
                let low_ret = self.lower_return(ret)?;
                Stmt::Return(low_ret)
            }
            ASTStmt::Semi(semi) => {
                let low_semi = self.lower_semi(semi)?;
                Stmt::Semi(low_semi)
            }
            ASTStmt::StructDef(struct_def) => {
                let low_struct_def_idx = self.lower_struct_def(struct_def)?;
                Stmt::StructDef(low_struct_def_idx)
            }
            ASTStmt::VarDef(var_def) => {
                let low_var_def = self.lower_var_def(var_def)?;
                Stmt::VarDef(low_var_def)
            }
        };
        Ok(lowered)
    }
}
