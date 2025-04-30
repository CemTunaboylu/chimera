use syntax::{language::SyntaxNode, syntax_kind::SyntaxKind};

use crate::{
    errors::ASTError,
    expression::Expr,
    lang_elems::{ensure_token_kind_is, error_for_node},
};

#[derive(Clone, Debug, PartialEq)]
pub struct Return(Expr);

impl Return {
    pub fn expr(&self) -> &Expr {
        &self.0
    }
}

impl TryFrom<&SyntaxNode> for Return {
    type Error = ASTError;

    fn try_from(return_node: &SyntaxNode) -> Result<Self, Self::Error> {
        ensure_token_kind_is(
            return_node.first_token().as_ref().unwrap(),
            SyntaxKind::KwReturn,
        )?;
        if let Some(to_return) = return_node.first_child() {
            Ok(Self(Expr::try_from(&to_return)?))
        } else {
            Err(error_for_node(return_node, "an expression"))
        }
    }
}

#[cfg(test)]
mod test {

    use parameterized_test::create;

    use super::*;
    use crate::{ast_root_from, cast_node_into_type};

    create! {
        happy_path_return_test,
        (program), {
            let ast_root = ast_root_from(program);
            let return_node = ast_root.get_root().first_child().unwrap();
            _ = cast_node_into_type::<Return>(&return_node);
        }
    }

    happy_path_return_test! {
        return_true_literal: "return true;",
        return_binary_infix_op: "return here & now;",
        return_method_call: "return obj.blueprint();",
    }
}
