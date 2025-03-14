use syntax::{
    is_a_binary_operator,
    language::{SyntaxNode, SyntaxToken},
};
use thin_vec::ThinVec;

use crate::{
    ast::ASTResult,
    errors::ASTError,
    expression::Expr,
    lang_elems::{get_children_as_expr, get_token_with},
};

#[derive(Debug)]
pub struct PreComputed {
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

impl PreComputed {
    fn get_op(&self) -> Option<&SyntaxToken> {
        self.op.as_ref()
    }

    fn get_nth_expr(&self, n: usize) -> Option<&Expr> {
        self.exprs.get(n)
    }
}

#[derive(Clone, Debug)]
pub enum Binary {
    Infix(PreComputed),
}

impl Binary {
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
            is_a_binary_operator(&token.kind())
        });
        Ok(Self::Infix(PreComputed { exprs, node, op }))
    }
    fn get_pre_computed(&self) -> &PreComputed {
        match self {
            Self::Infix(pre_computed) => pre_computed,
        }
    }

    pub fn lhs(&self) -> Option<&Expr> {
        let pre_computed = self.get_pre_computed();
        pre_computed.get_nth_expr(0)
    }
    pub fn rhs(&self) -> Option<&Expr> {
        let pre_computed = self.get_pre_computed();
        pre_computed.get_nth_expr(1)
    }

    pub fn op(&self) -> Option<&SyntaxToken> {
        let pre_computed = self.get_pre_computed();
        pre_computed.get_op()
    }
}

#[derive(Clone, Debug)]
pub enum Unary {
    Prefix(PreComputed),
    Postfix(PreComputed),
}

fn prepare_pre_computed(node: SyntaxNode, variant: fn(PreComputed) -> Unary) -> ASTResult<Unary> {
    let exprs = get_children_as_expr(&node)?;
    let op = get_token_with(&node, |token: &SyntaxToken| {
        token.kind().is_unary_operator()
    });
    let p = PreComputed { exprs, node, op };
    Ok(variant(p))
}

impl Unary {
    pub fn prefix(node: SyntaxNode) -> ASTResult<Self> {
        prepare_pre_computed(node, Self::Prefix)
    }
    pub fn postfix(node: SyntaxNode) -> ASTResult<Self> {
        prepare_pre_computed(node, Self::Postfix)
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

    pub fn operand(&self) -> Option<&Expr> {
        let pre = self.get_pre_computed();
        pre.exprs.first()
    }
}

#[cfg(test)]
mod test {

    use super::*;

    #[test]
    fn happy_path_for_infix() {}
}
