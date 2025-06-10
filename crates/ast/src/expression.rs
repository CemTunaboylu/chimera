use smol_str::SmolStr;
use syntax::{
    language::{NodeOrToken, SyntaxNode, SyntaxToken},
    syntax_kind::SyntaxKind,
};

use crate::{
    container_ref::ContainerRef as ASTContainerRef,
    delimited::{Block as ASTBlock, Indexing as ASTIndexing, Paren, Tuple as ASTTuple},
    errors::ASTError,
    function::{Call as ASTCall, On},
    let_binding::VarRef as ASTVarRef,
    literal::Literal as ASTLiteral,
    mutable::Mut as ASTMut,
    operation::{Binary, Unary},
    self_ref::SelfRef as ASTSelfRef,
    types::Type,
};

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Block(ASTBlock),
    Class(Type),
    ContainerRef(ASTContainerRef),
    Call(ASTCall),
    Infix(Binary),
    Indexing(ASTIndexing),
    Literal(ASTLiteral),
    Mut(ASTMut),
    Paren(Paren),
    SelfRef(ASTSelfRef),
    Tuple(ASTTuple),
    Unary(Unary),
    Unit,
    VarRef(ASTVarRef),
}

impl Expr {
    pub fn name(&self) -> Option<SmolStr> {
        match self {
            Self::Mut(mut_) => mut_.expr().name(),
            Self::VarRef(var_ref) => Some(var_ref.name().clone()),
            Self::ContainerRef(container_ref) => Some(container_ref.name().clone()),
            Self::Call(call) => match &call.on {
                On::Binding(name) => Some(name.clone()),
                On::Literal(_) => None,
            },
            _ => None,
        }
    }
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
            TyBool | TyBuffer | TyF32 | TyFn | TyI32 | TyChar | TyStr | TyTensor => {
                let class = Type::try_from(node)?;
                Self::Class(class)
            }
            ContainerRef => {
                let container_ref = ASTContainerRef::try_from(node)?;
                Self::ContainerRef(container_ref)
            }
            Call => {
                let call = ASTCall::try_from(node)?;
                Self::Call(call)
            }
            InfixBinOp => {
                let infix = Binary::new(node)?;
                Self::Infix(infix)
            }
            Indexing => Self::Indexing(ASTIndexing(node.clone())),
            Literal | StructLit => {
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
            SelfRef => {
                let self_ref = ASTSelfRef::try_from(node)?;
                Self::SelfRef(self_ref)
            }
            Tuple => Self::Tuple(ASTTuple(node.clone())),
            Unit => Self::Unit,
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
                        Call,
                        Indexing,
                        InfixBinOp,
                        Literal,
                        Mut,
                        ParenExpr,
                        PostfixUnaryOp,
                        PrefixUnaryOp,
                        SelfRef,
                        StructLit,
                        Tuple,
                        Unit,
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

impl TryFrom<&NodeOrToken> for Expr {
    type Error = ASTError;

    fn try_from(node_or_token: &NodeOrToken) -> Result<Self, Self::Error> {
        match node_or_token {
            NodeOrToken::Node(node) => Self::try_from(node),
            NodeOrToken::Token(token) => Self::try_from(token),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ast_root_from, cast_node_into_type};
    use parameterized_test::create;

    create! {
        create_expr_test,
        (program), {
        let ast_root = ast_root_from(program);
        let expr_node = ast_root.get_root().first_child().unwrap();
        cast_node_into_type::<Expr>(&expr_node);
        }
    }

    create_expr_test! {
        block: "{ return self.block() }",
        container_ref: "weights[0][0][0]",
        fn_call: "random_tensor()",
        method_call: "weights.raw_tensor()",
        static_method_call: "NN::raw_tensor()",
        nested_infix: "3 + 14/100",
        tensor_literal: "[[1,0,0],[0,1,0],[0,0,1]]",
        mutable: "new(mut self)",
        paren: "(1+1)",
        self_ref_instance: "self",
        self_ref_struct: "Self",
        tensor_type: "tensor<i32><batch_number(),_,r,g,b,c>",
        tensor_class_method: "tensor<i32><batch_number(),_,r,g,b,c>::new()",
        buffer_type: "buffer<i32><3,3,3>",
        unary: "-[[1,0,0],[0,1,0],[0,0,1]]",
        var_ref: "my_tensor_ref",
        unit: "()",
    }
}
