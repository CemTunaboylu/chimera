use crate::{errors::ASTError, expression::Expr, variable::VarDef};
use syntax::{language::SyntaxNode, syntax_kind::SyntaxKind};

#[derive(Debug)]
pub enum Stmt {
    VarDef(VarDef),
    Expr(Expr),
}

impl TryFrom<SyntaxNode> for Stmt {
    type Error = ASTError;

    fn try_from(node: SyntaxNode) -> Result<Self, Self::Error> {
        match node.kind() {
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
