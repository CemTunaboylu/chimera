use syntax::{bitset::SyntaxKindBitSet, language::SyntaxNode};
use syntax::{language::NodeOrToken, syntax_kind::SyntaxKind::*};

use crate::{
    errors::ASTError,
    expression::Expr,
    lang_elems::{filtered_children_with_tokens, get_tokens_in_errs},
};

// possible types are primitives + custom types i.e. structs
#[derive(Clone, Debug, PartialEq)]
pub enum Jump {
    Continue,
    Break(Option<NodeOrToken>),
}

impl Jump {
    pub fn expr(&self) -> Option<Expr> {
        match self {
            Jump::Continue => None,
            Jump::Break(node_or_token) => {
                if let Some(not) = node_or_token {
                    Expr::try_from(not).ok()
                } else {
                    None
                }
            }
        }
    }
}

impl TryFrom<&SyntaxNode> for Jump {
    type Error = ASTError;

    fn try_from(node: &SyntaxNode) -> Result<Self, Self::Error> {
        let tokens = get_tokens_in_errs(&node, [KwContinue, KwBreak].as_ref())?;
        let kw = tokens.first().unwrap();

        let j = match kw {
            cont if matches!(cont.kind(), KwContinue) => Self::Continue,
            brk if matches!(brk.kind(), KwBreak) => {
                let ignore: SyntaxKindBitSet = [KwBreak, Whitespace, Semi].as_ref().into();
                if let Some(returning) = filtered_children_with_tokens(&node, !ignore).first() {
                    Self::Break(Some(returning.clone()))
                } else {
                    Self::Break(None)
                }
            }
            unwanted => {
                return Err(ASTError::new(
                    unwanted.text_range().into(),
                    [KwContinue, KwBreak].as_ref(),
                    unwanted,
                ));
            }
        };

        Ok(j)
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::{ast_root_from, cast_node_into_type};
    use parameterized_test::create;

    create! {
        create_jump_test,
        (program), {
        let ast_root = ast_root_from(program);
        let jump_node = ast_root.get_root().first_child().unwrap();
        cast_node_into_type::<Jump>(&jump_node);
        }
    }

    create_jump_test! {
        valid_continue: "continue;",
        valid_break_no_return: "break;",
    }

    #[test]
    fn valid_returning_break() {
        let program = "break do_something();";
        let ast_root = ast_root_from(program);
        cast_node_into_type::<Jump>(ast_root.get_root().first_child().as_ref().unwrap());
    }
}
