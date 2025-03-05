use syntax::{language::SyntaxNode, syntax_kind::SyntaxKind};

use crate::{
    delimited::Paren,
    errors::ASTError,
    literal::Literal as ASTLiteral,
    operation::{Infix, Unary},
    variable::VarRef as ASTVarRef,
};

#[derive(Clone, Debug)]
pub enum Expr {
    Infix(Infix),
    Literal(ASTLiteral),
    Paren(Paren),
    Unary(Unary),
    VarRef(ASTVarRef),
}

impl TryFrom<SyntaxNode> for Expr {
    type Error = ASTError;

    fn try_from(node: SyntaxNode) -> Result<Self, Self::Error> {
        use SyntaxKind::*;
        let result = match node.kind() {
            InfixBinOp => {
                let infix = Infix::new(node)?;
                Self::Infix(infix)
            }

            Literal => {
                let literal = ASTLiteral::try_from(node)?;
                Self::Literal(literal)
            }
            ParenExpr => Self::Paren(Paren(node)),
            VarRef => Self::VarRef(ASTVarRef(node)),
            PrefixUnaryOp => {
                let prefix = Unary::prefix(node)?;
                Self::Unary(prefix)
            }
            PostFixUnaryOp => {
                let postfix = Unary::postfix(node)?;
                Self::Unary(postfix)
            }

            kind => {
                return Err(ASTError::new(
                    node.text_range().into(),
                    [
                        SyntaxKind::InfixBinOp,
                        SyntaxKind::Literal,
                        SyntaxKind::ParenExpr,
                        SyntaxKind::PrefixUnaryOp,
                        SyntaxKind::VarRef,
                    ]
                    .as_ref(),
                    kind,
                ));
            }
        };

        Ok(result)
    }
}
