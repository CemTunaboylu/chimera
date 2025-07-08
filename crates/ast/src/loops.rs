use smol_str::{SmolStr, ToSmolStr};
use syntax::{language::SyntaxNode, syntax_kind::SyntaxKind};
use thin_vec::{ThinVec, thin_vec};

use crate::{
    control_flow::{Condition, parse_condition_and_block},
    delimited::Block,
    errors::ASTError,
    expression::Expr,
    lang_elems::{
        ensure_node_kind_is, ensure_node_kind_is_any, filtered_children_with_tokens,
        first_child_of_kind_errs, get_children_in_errs, get_first_child_in, get_token_of,
    },
};

// TODO: do I support multiple var defs at once?
#[derive(Clone, Debug, PartialEq)]
pub struct Identifiers(pub ThinVec<SmolStr>);
#[derive(Clone, Debug, PartialEq)]
pub struct In(SyntaxNode);

impl In {
    pub fn expr(&self) -> Option<Expr> {
        Expr::try_from(&self.0).ok()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Loop {
    While(Condition, Block),
    For(Identifiers, In, Block),
}

fn parse_var_ref_as_identifier(node: &SyntaxNode) -> ThinVec<SmolStr> {
    let token = get_token_of(node, SyntaxKind::Ident).unwrap();
    thin_vec![token.text().to_smolstr()]
}
fn parse_tuple_identifiers(node: &SyntaxNode) -> ThinVec<SmolStr> {
    let mut identifiers = thin_vec![];

    for child in node.children() {
        if child.kind() == SyntaxKind::Tuple {
            let identifiers_within = parse_tuple_identifiers(&child);
            identifiers.extend_from_slice(identifiers_within.as_slice());
            continue;
        }
        identifiers.extend_from_slice(parse_var_ref_as_identifier(&child).as_slice());
    }

    identifiers
}

impl TryFrom<&SyntaxNode> for Loop {
    type Error = ASTError;

    fn try_from(node: &SyntaxNode) -> Result<Self, Self::Error> {
        ensure_node_kind_is_any(node, [SyntaxKind::ForLoop, SyntaxKind::WhileLoop].as_ref())?;
        match node.kind() {
            SyntaxKind::ForLoop => {
                let for_identifier_node = first_child_of_kind_errs(node, SyntaxKind::ForIdent)?;
                let sub_tree = filtered_children_with_tokens(
                    &for_identifier_node,
                    [SyntaxKind::Ident, SyntaxKind::Tuple].as_slice(),
                );
                let child = sub_tree.first().unwrap();
                let identifiers = if child.kind() == SyntaxKind::Tuple {
                    parse_tuple_identifiers(child.as_node().unwrap())
                } else {
                    thin_vec![child.as_token().map(|t| t.text().to_smolstr()).unwrap()]
                };
                let in_part = get_children_in_errs(node, SyntaxKind::In)?
                    .first()
                    .unwrap()
                    .clone();
                let block_node = node.last_child().unwrap();
                ensure_node_kind_is(&block_node, SyntaxKind::Block)?;
                let block = Block::try_from(&block_node)?;
                Ok(Self::For(Identifiers(identifiers), In(in_part), block))
            }
            SyntaxKind::WhileLoop => {
                let condition_and_block = get_children_in_errs(
                    node,
                    [SyntaxKind::Condition, SyntaxKind::Block].as_ref(),
                )?;
                if condition_and_block.len() != 2 {
                    Err(ASTError::new(
                        node.text_range().into(),
                        "a condition and a block",
                        condition_and_block.as_ref(),
                    ))
                } else {
                    let (condition, block) = parse_condition_and_block(condition_and_block)?;
                    Ok(Self::While(condition, block))
                }
            }
            _ => unreachable!(),
        }
    }
}

#[cfg(test)]
mod test {

    use parameterized_test::create;

    use super::*;
    use crate::{ast_root_from_assert_no_err, cast_node_into_type};

    create! {
        happy_path_loop_test,
        (program), {
            let ast_root = ast_root_from_assert_no_err(program);
            let loop_node = ast_root.get_root().first_child().unwrap();
            _ = cast_node_into_type::<Loop>(&loop_node);
        }
    }

    happy_path_loop_test! {
        while_true_literal: "while true {something()}",
        while_method_call: "while grandpa.is_alive() {something()}",
        while_expr: "while expectations-reality {something()}",
        while_expr_with_break: "while true {if self.is_full() {break;} eat()}",
        for_range: "for i in 0_9{something()}",
        for_each: "for elm in elements {something()}",
        for_method_call: "for hair in head.get_hairs() {something()}",
        for_enumerated: "for (ix, hair) in head.get_hairs().enumerate() {something()}",
    }
}
