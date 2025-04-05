use syntax::{language::SyntaxNode, syntax_kind::SyntaxKind};
use thin_vec::ThinVec;

use crate::{
    delimited::Block,
    errors::ASTError,
    expression::Expr,
    lang_elems::{
        ensure_token_kind_is, ensure_token_kind_is_not, error_for_node, first_token_expect,
        get_children_in, get_children_in_errs, get_token_of_errs,
    },
};

#[derive(Clone, Debug, PartialEq)]
pub struct Condition(Expr);

impl Condition {
    pub fn expr(&self) -> &Expr {
        &self.0
    }
}

impl TryFrom<&SyntaxNode> for Condition {
    type Error = ASTError;

    fn try_from(condition_node: &SyntaxNode) -> Result<Self, Self::Error> {
        if let Some(inside_condition) = condition_node.first_child() {
            Ok(Self(Expr::try_from(&inside_condition)?))
        } else if let Some(inside_condition) = condition_node.first_token() {
            Ok(Self(Expr::try_from(&inside_condition)?))
        } else {
            Err(error_for_node(condition_node, "an expression"))
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Conditional {
    If(Condition, Block),
    Elif(Condition, Block),
    Else(Block),
}

pub(crate) fn parse_condition_and_block(
    condition_and_block: ThinVec<SyntaxNode>,
) -> Result<(Condition, Block), ASTError> {
    let condition_node = condition_and_block.first().unwrap();
    let condition = Condition::try_from(condition_node)?;

    let block_node = condition_and_block.last().unwrap();
    let block = Block::try_from(block_node)?;
    Ok((condition, block))
}

impl TryFrom<&SyntaxNode> for Conditional {
    type Error = ASTError;

    fn try_from(conditional_node: &SyntaxNode) -> Result<Self, Self::Error> {
        let kw = first_token_expect(
            &conditional_node,
            [SyntaxKind::KwIf, SyntaxKind::KwElif, SyntaxKind::KwElse].as_ref(),
        )?;
        let condition_and_block = get_children_in(
            conditional_node,
            [SyntaxKind::Condition, SyntaxKind::Block].as_ref(),
        );
        if condition_and_block.len() > 2 {
            return Err(ASTError::new(
                conditional_node.text_range().into(),
                "a condition and a block",
                condition_and_block.as_ref(),
            ));
        } else if condition_and_block.len() == 2 {
            _ = ensure_token_kind_is_not(&kw, SyntaxKind::KwElse);
            let (condition, block) = parse_condition_and_block(condition_and_block)?;

            let c_flow = match kw.kind() {
                SyntaxKind::KwIf => Self::If(condition, block),
                SyntaxKind::KwElif => Self::Elif(condition, block),
                _ => unreachable!(),
            };
            Ok(c_flow)
        } else {
            _ = ensure_token_kind_is(&kw, SyntaxKind::KwElse);
            let block_node = condition_and_block.first().unwrap();
            let block = Block::try_from(block_node)?;
            Ok(Self::Else(block))
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ControlFlow(pub ThinVec<Conditional>);

impl TryFrom<&SyntaxNode> for ControlFlow {
    type Error = ASTError;

    fn try_from(control_flow_node: &SyntaxNode) -> Result<Self, Self::Error> {
        let conditional_nodes =
            get_children_in_errs(control_flow_node, [SyntaxKind::Conditional].as_ref())?;
        let first_conditional_if = conditional_nodes.first().unwrap();
        _ = get_token_of_errs(first_conditional_if, SyntaxKind::KwIf)?;
        let mut conditionals: ThinVec<Conditional> =
            ThinVec::with_capacity(conditional_nodes.len());
        for c in conditional_nodes {
            let conditional = Conditional::try_from(&c)?;
            conditionals.push(conditional);
        }
        Ok(Self(conditionals))
    }
}

#[cfg(test)]
mod test {

    use parameterized_test::create;

    use super::*;
    use crate::{ast_root_from, cast_node_into_type};

    create! {
        happy_path_condition_test,
        (program), {
            let ast_root = ast_root_from(program);
            let control_flow_node = ast_root.get_root().first_child().unwrap();
            let conditional_node = control_flow_node.first_child().unwrap();
            let condition_node = conditional_node.first_child().unwrap();
            _ = cast_node_into_type::<Condition>(&condition_node);
        }
    }

    happy_path_condition_test! {
        true_literal: "if true {}",
        var_ref: "if is_ok {}",
        expr: "if here & now {}",
        fn_call: "if ok() {}",
    }

    create! {
        happy_path_control_flow_test,
        (program), {
            let ast_root = ast_root_from(program);
            let control_flow_node = ast_root.get_root().first_child().unwrap();
            _ = cast_node_into_type::<ControlFlow>(&control_flow_node);
        }
    }

    happy_path_control_flow_test! {
        just_if_true: "if true {return true;}",
        if_elif: "if is_red{return true;} elif is_blue {return false;} ",
        if_elif_else: "if is_red{return RED;} elif is_blue {return BLUE;} else {return GREEN;}",
        if_else: "if is_red{return true;} else {return false;} ",
    }
}
