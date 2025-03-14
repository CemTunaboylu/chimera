use syntax::{bitset::SyntaxKindBitSet, language::SyntaxNode};
use syntax::{
    language::{NodeOrToken, SyntaxToken},
    syntax_kind::SyntaxKind::*,
};

use crate::{
    errors::ASTError,
    lang_elems::{error_for_node, filtered_children_with_tokens, get_children_in},
};

// possible types are primitives + custom types i.e. structs
#[derive(Clone, Debug)]
pub enum Jump {
    Continue(SyntaxToken),
    Break(SyntaxToken, Option<NodeOrToken>),
}

impl TryFrom<&SyntaxNode> for Jump {
    type Error = ASTError;

    fn try_from(parent_of_jump: &SyntaxNode) -> Result<Self, Self::Error> {
        let types = [Jump].as_ref();

        let jump = get_children_in(&parent_of_jump, types);
        let jump = if let Some(node) = jump.first() {
            node
        } else {
            return Err(error_for_node(parent_of_jump, "a jumping statement"));
        };
        let kw = if let Some(cwt) = jump.children_with_tokens().next() {
            cwt
        } else {
            return Err(error_for_node(jump, [KwContinue, KwBreak].as_ref()));
        };

        let j = match kw {
            cont if matches!(cont.kind(), KwContinue) => Self::Continue(cont.into_token().unwrap()),
            brk if matches!(brk.kind(), KwBreak) => {
                let brk = brk.into_token().unwrap();
                let ignore: SyntaxKindBitSet = [KwBreak, Whitespace, Semi].as_ref().into();
                if let Some(returning) = filtered_children_with_tokens(jump, !ignore).first() {
                    Self::Break(brk, Some(returning.clone()))
                } else {
                    Self::Break(brk, None)
                }
            }
            unwanted => {
                return Err(ASTError::new(
                    unwanted.text_range().into(),
                    [KwContinue, KwBreak].as_ref(),
                    &unwanted,
                ));
            }
        };

        Ok(j)
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::ast::tests::{ast_root_from, cast_into_type};
    use parameterized_test::create;

    create! {
        create_type_test,
        (program), {
        let ast_root = ast_root_from(program);
        cast_into_type::<Jump>(ast_root.get_root());
        }
    }

    create_type_test! {
        valid_continue: "continue;",
        valid_break_no_return: "break;",
    }

    #[test]
    fn valid_returning_break() {
        let program = "break do_something();";
        let ast_root = ast_root_from(program);

        cast_into_type::<Jump>(ast_root.get_root());
    }
}
