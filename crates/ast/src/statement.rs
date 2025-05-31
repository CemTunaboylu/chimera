use crate::{
    control_flow::ControlFlow as ASTControlFlow, errors::ASTError, expression::Expr,
    function::FnDef, impl_block::Impl, jump::Jump, loops::Loop, return_stmt::Return, semi::Semi,
    structure::StructDef, variable::VarDef,
};
use syntax::{language::SyntaxNode, syntax_kind::SyntaxKind};

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    ControlFlow(ASTControlFlow),
    Return(Return),
    Expr(Expr),
    FnDef(FnDef),
    Impl(Impl),
    Jump(Jump),
    Loop(Loop),
    Semi(Semi),
    StructDef(StructDef),
    VarDef(VarDef),
}

impl TryFrom<&SyntaxNode> for Stmt {
    type Error = ASTError;

    fn try_from(node: &SyntaxNode) -> Result<Self, Self::Error> {
        match node.kind() {
            SyntaxKind::ControlFlow => match ASTControlFlow::try_from(node) {
                Ok(cond) => Ok(Self::ControlFlow(cond)),
                Err(err) => Err(err),
            },
            SyntaxKind::FnDef => match FnDef::try_from(node) {
                Ok(fn_def) => Ok(Self::FnDef(fn_def)),
                Err(err) => Err(err),
            },
            SyntaxKind::ForLoop | SyntaxKind::WhileLoop => match Loop::try_from(node) {
                Ok(loop_) => Ok(Self::Loop(loop_)),
                Err(err) => Err(err),
            },
            SyntaxKind::ImplBlock => match Impl::try_from(node) {
                Ok(impl_block) => Ok(Self::Impl(impl_block)),
                Err(err) => Err(err),
            },
            SyntaxKind::Jump => match Jump::try_from(node) {
                Ok(jump) => Ok(Self::Jump(jump)),
                Err(err) => Err(err),
            },
            SyntaxKind::Return => match Return::try_from(node) {
                Ok(ret) => Ok(Self::Return(ret)),
                Err(err) => Err(err),
            },
            SyntaxKind::Semi => match Semi::try_from(node) {
                Ok(semi) => Ok(Self::Semi(semi)),
                Err(err) => Err(err),
            },
            SyntaxKind::StructDef => match StructDef::try_from(node) {
                Ok(struct_def) => Ok(Self::StructDef(struct_def)),
                Err(err) => Err(err),
            },
            SyntaxKind::LetBinding => match VarDef::try_from(node) {
                Ok(var_def) => Ok(Self::VarDef(var_def)),
                Err(err) => Err(err),
            },
            _ => match Expr::try_from(node) {
                Ok(expr) => Ok(Self::Expr(expr)),
                Err(err) => Err(err),
            },
        }
    }
}

impl TryFrom<SyntaxNode> for Stmt {
    type Error = ASTError;

    fn try_from(node: SyntaxNode) -> Result<Self, Self::Error> {
        Self::try_from(&node)
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::{ast_root_from, cast_node_into_type};
    use parameterized_test::create;

    create! {
        create_stmt_test,
        (program), {
        let ast_root = ast_root_from(program);
        let stmt_node = ast_root.get_root().first_child().unwrap();
        cast_node_into_type::<Stmt>(&stmt_node);
        }
    }
    create_stmt_test! {
        control_flow_just_if: "if is_ok { return self.block() }",
        control_flow_just_if_else: "if is_ok { return self.block(); } else { return self.alternative(); }",
        control_flow_just_if_elif_else: "if is_red { RED } elif is_blue { BLUE } else { GREEN }",
        expr: "weights[0][0][0]",
        fn_def_with_tensor_typing_just_typehint: "fn quantize(&mut self, mode: Mode) -> tensor<i32> { quantize(self) } ",
        fn_def_with_tensor_typing_just_dimhint: "fn quantize(&mut self, mode: Mode) -> tensor<1000, 1000, 1000> { quantize(self) } ",
        fn_def_with_tensor_typing_full: "fn quantize(&mut self, mode: Mode) -> tensor<i32><1000, 1000, 1000> { quantize(self) } ",
        jump: "break weights.raw_tensor();",
        loop_: "while is_ok { server.listen(); }",
        semi: "cannot_return();",
        struct_def: "struct S {field: Field}",
        var_def: "let unit = [[1,0,0],[0,1,0],[0,0,1]];",
        impl_block: "impl Point { fn translate(&mut self, by: Point) { self.x += by.x; self.y += by.y; } fn rotate(&mut self, by: Point) { self.rotate_around(&by);} \n}",
    }

    #[test]
    fn var_def() {
        let program = "let diff_norm = (point_1.locus() - point_2.locus()).normalize();";
        let ast_root = ast_root_from(program);
        let var_def_node = ast_root.get_root().first_child().unwrap();
        cast_node_into_type::<Stmt>(&var_def_node);
    }
}
