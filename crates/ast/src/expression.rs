use syntax::{
    language::{SyntaxNode, SyntaxToken},
    syntax_kind::SyntaxKind,
};

use crate::{
    container_ref::ContainerRef as ASTContainerRef,
    delimited::{Block as ASTBlock, Indexing as ASTIndexing, Paren},
    errors::ASTError,
    function::FnCall as ASTFnCall,
    literal::Literal as ASTLiteral,
    mutable::Mut as ASTMut,
    operation::{Binary, Unary},
    variable::VarRef as ASTVarRef,
};

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Block(ASTBlock),
    ContainerRef(ASTContainerRef),
    FnCall(ASTFnCall),
    Infix(Binary),
    Indexing(ASTIndexing),
    Literal(ASTLiteral),
    Mut(ASTMut),
    Paren(Paren),
    Unary(Unary),
    VarRef(ASTVarRef),
}

impl TryFrom<&SyntaxNode> for Expr {
    type Error = ASTError;

    fn try_from(node: &SyntaxNode) -> Result<Self, Self::Error> {
        use SyntaxKind::*;
        let result = match node.kind() {
            Block => {
                let block = ASTBlock::try_from(node)?;
                Self::Block(block)
            }
            ContainerRef => {
                let container_ref = ASTContainerRef::try_from(node)?;
                Self::ContainerRef(container_ref)
            }
            FnCall => {
                let fn_call = ASTFnCall::try_from(node)?;
                Self::FnCall(fn_call)
            }
            InfixBinOp => {
                let infix = Binary::new(node)?;
                Self::Infix(infix)
            }
            Indexing => Self::Indexing(ASTIndexing(node.clone())),
            Literal => {
                let literal = ASTLiteral::try_from(node)?;
                Self::Literal(literal)
            }
            Mut => {
                let mut_ = ASTMut::try_from(node)?;
                Self::Mut(mut_)
            }
            ParenExpr => Self::Paren(Paren(node.clone())),
            PostfixUnaryOp => {
                let postfix = Unary::postfix(node)?;
                Self::Unary(postfix)
            }
            PrefixUnaryOp => {
                let prefix = Unary::prefix(node)?;
                Self::Unary(prefix)
            }
            VarRef => {
                let var_ref = ASTVarRef::try_from(node)?;
                Self::VarRef(var_ref)
            }
            kind => {
                use SyntaxKind::*;
                return Err(ASTError::new(
                    node.text_range().into(),
                    [
                        Block,
                        ContainerRef,
                        FnCall,
                        InfixBinOp,
                        Indexing,
                        Literal,
                        Mut,
                        ParenExpr,
                        PostfixUnaryOp,
                        PrefixUnaryOp,
                        VarRef,
                    ]
                    .as_ref(),
                    kind,
                ));
            }
        };

        Ok(result)
    }
}

impl TryFrom<SyntaxNode> for Expr {
    type Error = ASTError;

    fn try_from(node: SyntaxNode) -> Result<Self, Self::Error> {
        Self::try_from(&node)
    }
}

impl TryFrom<&SyntaxToken> for Expr {
    type Error = ASTError;

    fn try_from(token: &SyntaxToken) -> Result<Self, Self::Error> {
        use SyntaxKind::*;
        let result = match token.kind() {
            KwTrue | KwFalse => {
                let literal = ASTLiteral::try_from(token)?;
                Self::Literal(literal)
            }
            kind => {
                return Err(ASTError::new(
                    token.text_range().into(),
                    [SyntaxKind::KwTrue, SyntaxKind::KwFalse].as_ref(),
                    kind,
                ));
            }
        };

        Ok(result)
    }
}
