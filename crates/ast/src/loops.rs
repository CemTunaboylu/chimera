use syntax::{language::SyntaxNode, syntax_kind::SyntaxKind};
use thin_vec::ThinVec;

use crate::{
    control_flow::{Condition, parse_condition_and_block},
    delimited::Block,
    errors::ASTError,
    lang_elems::{ensure_node_kind_is, ensure_node_kind_is_any, get_children_in_errs},
};

// TODO: do I support multiple var defs at once?
#[derive(Clone, Debug, PartialEq)]
pub struct Identifiers(ThinVec<SyntaxNode>);
#[derive(Clone, Debug, PartialEq)]
pub struct In(SyntaxNode);

#[derive(Clone, Debug, PartialEq)]
pub enum Loop {
    While(Condition, Block),
    For(Identifiers, In, Block),
}

impl Loop {}

impl TryFrom<&SyntaxNode> for Loop {
    type Error = ASTError;

    fn try_from(node: &SyntaxNode) -> Result<Self, Self::Error> {
        _ = ensure_node_kind_is_any(node, [SyntaxKind::ForLoop, SyntaxKind::WhileLoop].as_ref())?;
        match node.kind() {
            SyntaxKind::ForLoop => {
                let identifiers = get_children_in_errs(node, SyntaxKind::ForIdent)?;
                let in_part = get_children_in_errs(node, SyntaxKind::In)?
                    .first()
                    .unwrap()
                    .clone();
                let block_node = node.last_child().unwrap();
                _ = ensure_node_kind_is(&block_node, SyntaxKind::Block)?;
                let block = Block::try_from(&block_node)?;
                Ok(Self::For(Identifiers(identifiers), In(in_part), block))
            }
            SyntaxKind::WhileLoop => {
                let condition_and_block = get_children_in_errs(
                    node,
                    [SyntaxKind::Condition, SyntaxKind::Block].as_ref(),
                )?;
                if condition_and_block.len() != 2 {
                    return Err(ASTError::new(
                        node.text_range().into(),
                        "a condition and a block",
                        condition_and_block.as_ref(),
                    ));
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
    use crate::ast::tests::{ast_root_from, cast_node_into_type};

    create! {
        happy_path_loop_test,
        (program), {
            let ast_root = ast_root_from(program);
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
        for_enumerated: "for ix, hair in head.get_hairs().enumerate() {something()}",
    }
}
