use crate::{
    control_flow::ControlFlow as ASTControlFlow, errors::ASTError, expression::Expr,
    function::FnDef, jump::Jump, semi::Semi, variable::VarDef,
};
use syntax::{language::SyntaxNode, syntax_kind::SyntaxKind};

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    ControlFlow(ASTControlFlow),
    Expr(Expr),
    FnDef(FnDef),
    Jump(Jump),
    Semi(Semi),
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
            SyntaxKind::Semi => match Semi::try_from(node) {
                Ok(semi) => Ok(Self::Semi(semi)),
                Err(err) => Err(err),
            },
            SyntaxKind::VarDef => match VarDef::try_from(node) {
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
    use crate::ast::tests::{ast_root_from, cast_into_type};

    #[test]
    fn var_def() {
        let program = "let diff_norm = (point_1.locus() - point_2.locus()).normalize();";
        let ast_root = ast_root_from(program);
        let var_def_node = ast_root.get_root().first_child().unwrap();
        cast_into_type::<Stmt>(&var_def_node);
    }
}
