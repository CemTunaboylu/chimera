use syntax::language::{SyntaxNode, SyntaxToken};
use thin_vec::ThinVec;

use crate::{
    ast::ASTResult,
    errors::ASTError,
    expression::Expr,
    lang_elems::{get_children_as_expr, get_token_with},
};

#[derive(Debug)]
struct PreComputed {
    exprs: ThinVec<Expr>,
    node: SyntaxNode,
    op: Option<SyntaxToken>,
}

impl Clone for PreComputed {
    fn clone(&self) -> Self {
        Self {
            exprs: self.exprs.iter().map(|e| e.clone()).collect::<ThinVec<_>>(),
            node: self.node.clone(),
            op: self.op.clone(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Infix {
    Binary(PreComputed),
}

impl Infix {
    pub fn new(node: SyntaxNode) -> ASTResult<Self> {
        let exprs = get_children_as_expr(&node)?;
        if exprs.len() != 2 {
            return Err(ASTError::new(
                node.text_range().into(),
                "expected 2 expressions for lhs and rhs",
                &node,
            ));
        }
        let op = get_token_with(&node, |token: &SyntaxToken| {
            token.kind().is_binary_operator()
        });
        Ok(Self::Binary(PreComputed { exprs, node, op }))
    }
    fn get_pre_computed(&self) -> &PreComputed {
        match self {
            Self::Binary(pre_computed) => pre_computed,
        }
    }
    fn get_syntax_node(&self) -> &SyntaxNode {
        &self.get_pre_computed().node
    }

    pub fn lhs(&self) -> Option<&Expr> {
        let pre_computed = self.get_pre_computed();
        pre_computed.exprs.iter().nth(0)
    }
    pub fn rhs(&self) -> Option<&Expr> {
        let pre_computed = self.get_pre_computed();
        pre_computed.exprs.iter().nth(1)
    }

    pub fn op(&self) -> Option<SyntaxToken> {
        let pre_computed = self.get_pre_computed();
        pre_computed.op.clone()
    }
}

#[derive(Clone, Debug)]
pub enum Unary {
    Prefix(PreComputed),
    Postfix(PreComputed),
}

impl Unary {
    pub fn prefix(node: SyntaxNode) -> ASTResult<Self> {
        Ok(Self::Prefix(Self::prepare_pre_computed(node)?))
    }
    pub fn postfix(node: SyntaxNode) -> ASTResult<Self> {
        Ok(Self::Postfix(Self::prepare_pre_computed(node)?))
    }
    fn prepare_pre_computed(node: SyntaxNode) -> ASTResult<PreComputed> {
        let exprs = get_children_as_expr(&node)?;
        let op = get_token_with(&node, |token: &SyntaxToken| {
            token.kind().is_unary_operator()
        });
        Ok(PreComputed { exprs, node, op })
    }
    fn get_pre_computed(&self) -> &PreComputed {
        match self {
            Self::Prefix(pre_computed) => pre_computed,
            Self::Postfix(pre_computed) => pre_computed,
        }
    }

    pub fn op(&self) -> Option<SyntaxToken> {
        let pre = self.get_pre_computed();
        pre.op.clone()
    }

    pub fn expr(&self) -> Option<&Expr> {
        let pre = self.get_pre_computed();
        pre.exprs.first()
    }
}

#[cfg(test)]
mod test {

    use super::*;
    use parser::parser::Parser;

    #[test]
    fn happy_path_for_infix() {
        let program = "your_mom + me";
    }
}
