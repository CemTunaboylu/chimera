use lazy_static::lazy_static;
use num_traits::{FromPrimitive, ToPrimitive};
use smol_str::{SmolStr, ToSmolStr};
use std::{fmt::Debug, ops::Range};
use thin_vec::{ThinVec, thin_vec};

use crate::{ast::ASTResult, errors::ASTError, expression::Expr};
use SyntaxKind::*;
use syntax::{
    bitset::SyntaxKindBitSet,
    language::{NodeOrToken, SyntaxElement, SyntaxNode, SyntaxToken},
    syntax_kind::SyntaxKind,
};

const EXPR_CANDIDATES: &[SyntaxKind; 10] = &[
    Literal, Block, Call, Ident, InfixBinOp, KwSelf, Kwself, KwFalse, KwTrue, ParenExpr,
];

pub fn get_text(not: &NodeOrToken) -> SmolStr {
    match not {
        NodeOrToken::Node(node) => node.text().to_smolstr(),
        NodeOrToken::Token(token) => token.text().to_smolstr(),
    }
}

lazy_static! {
    pub static ref UNNECESSARY: SyntaxKindBitSet = {
        use SyntaxKind::*;
        let ranges = [(KwBreak, Kwself), (LBrace, LParen), (RArrow, RParen)];
        let mut set = SyntaxKindBitSet::empty();
        for (start, end) in ranges {
            for kind in start.to_u16().unwrap()..=end.to_u16().unwrap() {
                let kind: SyntaxKind = SyntaxKind::from_u16(kind).unwrap();
                set += kind.into();
            }
        }
        set += [
            And, Comma, Colon, LBrack, NullTerm, RBrack, RetType, Whitespace,
        ]
        .as_ref()
        .into();
        set
    };
}

pub fn get_kind_on_node_or_token(not: &NodeOrToken) -> SyntaxKind {
    not.kind()
}

pub fn get_kind_on_node(n: &SyntaxNode) -> SyntaxKind {
    n.kind()
}

pub fn filter_irrelevant_out<K>(
    iterable: impl Iterator<Item = K>,
    get_kind: fn(&K) -> SyntaxKind,
) -> ThinVec<K> {
    iterable
        .filter(|elm_with_kind| !UNNECESSARY.contains(get_kind(elm_with_kind)))
        .collect::<ThinVec<_>>()
}

pub fn vector_of_children_and_tokens_as<T>(
    node: &SyntaxNode,
    remove: impl Into<SyntaxKindBitSet>,
    f: fn(&NodeOrToken) -> ASTResult<T>,
) -> ASTResult<ThinVec<T>> {
    let mut types: ThinVec<T> = thin_vec![];
    let set: SyntaxKindBitSet = remove.into();
    for ch in node
        .children_with_tokens()
        .filter(|not| !set.contains(not.kind()))
    {
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

pub const CAN_BE_EMPTY: bool = false;
pub const ERR_IF_EMPTY: bool = true;

pub fn get_children_as<T>(node: &SyntaxNode, err_if_empty: bool) -> ASTResult<ThinVec<T>>
where
    T: TryFrom<SyntaxNode> + Debug,
    <T as TryFrom<SyntaxNode>>::Error: Debug,
{
    let results = node
        .children()
        .filter_map(|node| T::try_from(node).ok())
        .collect::<ThinVec<_>>();

    if err_if_empty && results.is_empty() {
        Err(error_for_node(
            node,
            "children are expected to be non empty",
        ))
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
