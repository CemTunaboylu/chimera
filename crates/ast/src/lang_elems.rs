use syntax::{
    bitset::SyntaxKindBitSet,
    language::{NodeOrToken, SyntaxElement, SyntaxNode, SyntaxToken},
    syntax_kind::SyntaxKind,
};
use thin_vec::{ThinVec, thin_vec};

use crate::{ast::ASTResult, errors::ASTError, expression::Expr};

use std::{fmt::Debug, ops::Range};

use SyntaxKind::*;

const EXPR_CANDIDATES: &[SyntaxKind; 10] = &[
    Literal, Block, Call, Ident, InfixBinOp, KwSelf, Kwself, KwFalse, KwTrue, ParenExpr,
];

pub fn vector_of_children_as<T>(
    node: &SyntaxNode,
    set: impl Into<SyntaxKindBitSet>,
    f: fn(&NodeOrToken) -> ASTResult<T>,
) -> ASTResult<ThinVec<T>> {
    let mut types: ThinVec<T> = thin_vec![];
    for ch in children_with_tokens_without_unwanted(&node, set) {
        let t = f(&ch).map_err(|e| ASTError::new(ch.text_range().into(), e, ch.kind()))?;
        types.push(t);
    }
    Ok(types)
}

pub fn err_if_empty<T>(a: &ThinVec<T>, span: Range<usize>, exp: &str) -> ASTResult<()> {
    if a.is_empty() {
        return Err(ASTError::with_err_msg(span, exp.to_string()));
    }
    Ok(())
}

pub fn unwrap_first_child_or_err(node: &SyntaxNode) -> ASTResult<SyntaxNode> {
    if let Some(child) = node.first_child() {
        Ok(child)
    } else {
        Err(error_for_node(node, "a children"))
    }
}

pub fn first_token_expect(
    node: &SyntaxNode,
    set: impl Into<SyntaxKindBitSet>,
) -> ASTResult<SyntaxToken> {
    let set: SyntaxKindBitSet = set.into();
    if let Some(first_token) = node.first_token() {
        if set.contains(first_token.kind()) {
            Ok(first_token)
        } else {
            Err(error_for_token(&first_token, set))
        }
    } else {
        Err(ASTError::new(node.text_range().into(), set, node))
    }
}

pub fn ensure_token_kind_is_not(token: &SyntaxToken, kind: SyntaxKind) -> ASTResult<()> {
    if token.kind() == kind {
        return Err(error_for_token(
            token,
            format!("cannot be {:?}", kind).as_str(),
        ));
    }
    Ok(())
}

pub fn ensure_token_kind_is(token: &SyntaxToken, kind: SyntaxKind) -> ASTResult<()> {
    if token.kind() != kind {
        return Err(error_for_token(
            token,
            format!("must be {:?}", kind).as_str(),
        ));
    }
    Ok(())
}

pub fn ensure_node_kind_is(node: &SyntaxNode, kind: SyntaxKind) -> ASTResult<()> {
    if node.kind() != kind {
        return Err(error_for_node(node, format!("must be {:?}", kind).as_str()));
    }
    Ok(())
}

pub fn ensure_node_kind_is_any(
    node: &SyntaxNode,
    set: impl Into<SyntaxKindBitSet>,
) -> ASTResult<()> {
    let set: SyntaxKindBitSet = set.into();
    if !set.contains(node.kind()) {
        return Err(error_for_node(node, set));
    }
    Ok(())
}
pub fn get_single_children_as_expr(node: &SyntaxNode) -> ASTResult<Expr> {
    node.children()
        .find_map(|node| Expr::try_from(&node).ok())
        .map(Ok)
        .unwrap_or(Err(error_for_node(node, EXPR_CANDIDATES.as_ref())))
}

pub fn first_child_of_kind(
    node: &SyntaxNode,
    set: impl Into<SyntaxKindBitSet>,
) -> Option<SyntaxNode> {
    let set: SyntaxKindBitSet = set.into();
    node.children().find(|node| set.contains(node.kind()))
}

pub fn first_child_of_kind_errs(
    node: &SyntaxNode,
    set: impl Into<SyntaxKindBitSet>,
) -> ASTResult<SyntaxNode> {
    let set: SyntaxKindBitSet = set.into();

    if let Some(node) = first_child_of_kind(node, set) {
        Ok(node)
    } else {
        Err(error_for_node(node, set))
    }
}

pub fn filtered_children_with_tokens(
    node: &SyntaxNode,
    set: impl Into<SyntaxKindBitSet>,
) -> ThinVec<NodeOrToken> {
    let set = set.into();
    node.children_with_tokens()
        .filter(|node_or_token| set.contains(node_or_token.kind()))
        .collect::<ThinVec<_>>()
}

pub fn children_with_tokens_without_unwanted(
    node: &SyntaxNode,
    unwanted: impl Into<SyntaxKindBitSet>,
) -> ThinVec<NodeOrToken> {
    let set = unwanted.into();
    node.children_with_tokens()
        .filter(|node_or_token| !set.contains(node_or_token.kind()))
        .collect::<ThinVec<_>>()
}

pub fn get_children_as<T>(node: &SyntaxNode) -> ASTResult<ThinVec<T>>
where
    T: TryFrom<SyntaxNode> + Debug,
    <T as TryFrom<SyntaxNode>>::Error: Debug,
{
    let results = node
        .children()
        .filter_map(|node| T::try_from(node).ok())
        .collect::<ThinVec<_>>();

    // TODO: put a flag
    if results.is_empty() {
        Err(error_for_node(node, "non empty"))
    } else {
        Ok(results)
    }
}

pub fn get_children_in(node: &SyntaxNode, set: impl Into<SyntaxKindBitSet>) -> ThinVec<SyntaxNode> {
    let set: SyntaxKindBitSet = set.into();
    node.children()
        .filter(|node| set.contains(node.kind()))
        .collect::<ThinVec<_>>()
}

pub fn get_children_in_errs(
    node: &SyntaxNode,
    set: impl Into<SyntaxKindBitSet>,
) -> ASTResult<ThinVec<SyntaxNode>> {
    let set: SyntaxKindBitSet = set.into();
    let children = node
        .children()
        .filter(|node| set.contains(node.kind()))
        .collect::<ThinVec<_>>();
    if children.is_empty() {
        Err(error_for_node(node, set))
    } else {
        Ok(children)
    }
}

pub fn get_first_child_in(
    node: &SyntaxNode,
    set: impl Into<SyntaxKindBitSet>,
) -> Option<SyntaxNode> {
    let set: SyntaxKindBitSet = set.into();
    node.children().find(|node| set.contains(node.kind()))
}

pub fn get_token(node: &SyntaxNode) -> Option<SyntaxToken> {
    node.children_with_tokens()
        .find_map(SyntaxElement::into_token)
}

pub fn get_token_of(node: &SyntaxNode, set: impl Into<SyntaxKindBitSet>) -> Option<SyntaxToken> {
    let set: SyntaxKindBitSet = set.into();
    node.children_with_tokens()
        .filter_map(SyntaxElement::into_token)
        .find(|t| set.contains(t.kind()))
}

pub fn get_token_of_errs(
    node: &SyntaxNode,
    set: impl Into<SyntaxKindBitSet>,
) -> ASTResult<SyntaxToken> {
    let set: SyntaxKindBitSet = set.into();
    if let Some(token) = get_token_of(node, set) {
        Ok(token)
    } else {
        Err(error_for_node(node, set))
    }
}

pub fn get_tokens_in(node: &SyntaxNode, set: impl Into<SyntaxKindBitSet>) -> ThinVec<SyntaxToken> {
    let set: SyntaxKindBitSet = set.into();
    node.children_with_tokens()
        .filter_map(SyntaxElement::into_token)
        .filter(|t| set.contains(t.kind()))
        .collect()
}

pub fn get_tokens_in_errs(
    node: &SyntaxNode,
    set: impl Into<SyntaxKindBitSet>,
) -> ASTResult<ThinVec<SyntaxToken>> {
    let set: SyntaxKindBitSet = set.into();
    let tokens = get_tokens_in(node, set);
    if tokens.is_empty() {
        Err(error_for_node(node, set))
    } else {
        Ok(tokens)
    }
}

pub fn get_children_with_tokens_in_f(
    node: &SyntaxNode,
    f: fn(SyntaxKind) -> bool,
) -> ThinVec<NodeOrToken> {
    node.children_with_tokens()
        .filter(|node_or_token| f(node_or_token.kind()))
        .collect::<ThinVec<_>>()
}

pub fn get_token_with(
    node: &SyntaxNode,
    predicate: fn(&SyntaxToken) -> bool,
) -> Option<SyntaxToken> {
    node.children_with_tokens()
        .filter_map(SyntaxElement::into_token)
        .find(predicate)
}

pub fn error_for_node(node: &SyntaxNode, expected: impl Debug) -> ASTError {
    ASTError::new(node.text_range().into(), expected, node.kind())
}

pub fn error_for_token(token: &SyntaxToken, expected: impl Debug) -> ASTError {
    ASTError::new(token.text_range().into(), expected, token.kind())
}
