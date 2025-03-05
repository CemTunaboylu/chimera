use syntax::{
    language::{SyntaxElement, SyntaxNode, SyntaxToken},
    syntax_kind::SyntaxKind,
};
use thin_vec::{ThinVec, thin_vec};

use crate::{
    ast::ASTResult,
    errors::{ASTError, Stringer},
    expression::Expr,
};

const EXPR_CANDIDATES: &[SyntaxKind; 10] = &[
    SyntaxKind::Literal,
    SyntaxKind::Block,
    SyntaxKind::FnCall,
    SyntaxKind::Ident,
    SyntaxKind::InfixBinOp,
    SyntaxKind::KwSelf,
    SyntaxKind::Kwself,
    SyntaxKind::KwFalse,
    SyntaxKind::KwTrue,
    SyntaxKind::ParenExpr,
];

pub fn get_single_children_as_expr(node: &SyntaxNode) -> ASTResult<Expr> {
    node.children()
        .find_map(|node| Expr::try_from(node).ok())
        .map(|opt_exp| Ok(opt_exp))
        .unwrap_or(Err(error_for_node(node, EXPR_CANDIDATES.as_ref())))
}

pub fn get_children_as_expr(node: &SyntaxNode) -> ASTResult<ThinVec<Expr>> {
    let mut errors = thin_vec![];

    let results = node
        .children()
        .filter_map(|node| {
            let result: ASTResult<Expr> = Expr::try_from(node);
            match result {
                Ok(expr) => Some(expr),
                Err(err) => {
                    errors.push(err);
                    None
                }
            }
        })
        .collect::<ThinVec<_>>();
    if results.is_empty() {
        Err(ASTError::new(
            node.text_range().into(),
            EXPR_CANDIDATES.as_ref(),
            node,
        ))
    } else {
        Ok(results)
    }
}

pub fn get_token(node: &SyntaxNode) -> Option<SyntaxToken> {
    node.children_with_tokens()
        .find_map(SyntaxElement::into_token)
}

pub fn get_token_with(
    node: &SyntaxNode,
    predicate: fn(&SyntaxToken) -> bool,
) -> Option<SyntaxToken> {
    node.children_with_tokens()
        .filter_map(SyntaxElement::into_token)
        .find(predicate)
}

pub fn get_first_node(node: &SyntaxNode, predicate: fn(&SyntaxNode) -> bool) -> Option<SyntaxNode> {
    node.children().find(predicate)
}

pub fn error_for_node(node: &SyntaxNode, expected: impl Stringer) -> ASTError {
    ASTError::new(node.text_range().into(), expected, node.kind())
}

pub fn err_with_recovered(node: SyntaxNode, expected: impl Stringer) -> ASTError {
    let recovered = get_first_node(&node, |node: &SyntaxNode| {
        node.kind() == SyntaxKind::Recovered
    });
    let parent = recovered.unwrap().parent().unwrap();
    error_for_node(&parent, expected)
}
