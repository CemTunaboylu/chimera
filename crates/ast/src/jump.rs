use syntax::{bitset::SyntaxKindBitSet, language::SyntaxNode};
use syntax::{
    language::{NodeOrToken, SyntaxToken},
    syntax_kind::SyntaxKind::*,
};

use crate::{
    errors::ASTError,
    lang_elems::{filtered_children_with_tokens, get_children_in_errs, get_tokens_in_errs},
};

// possible types are primitives + custom types i.e. structs
#[derive(Clone, Debug, PartialEq)]
pub enum Jump {
    Continue(SyntaxToken),
    Break(SyntaxToken, Option<NodeOrToken>),
}

impl TryFrom<&SyntaxNode> for Jump {
    type Error = ASTError;

    fn try_from(parent_of_jump: &SyntaxNode) -> Result<Self, Self::Error> {
        let jump = get_children_in_errs(&parent_of_jump, Jump)?;
        let node = jump.first().unwrap();
        let tokens = get_tokens_in_errs(node, [KwContinue, KwBreak].as_ref())?;
        let kw = tokens.first().unwrap();

        let j = match kw {
            cont if matches!(cont.kind(), KwContinue) => Self::Continue(cont.clone()),
            brk if matches!(brk.kind(), KwBreak) => {
                let ignore: SyntaxKindBitSet = [KwBreak, Whitespace, Semi].as_ref().into();
                let brk = brk.clone();
                if let Some(returning) = filtered_children_with_tokens(node, !ignore).first() {
                    Self::Break(brk, Some(returning.clone()))
                } else {
                    Self::Break(brk, None)
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
    use crate::ast::tests::{ast_root_from, cast_into_type};
    use parameterized_test::create;

    create! {
        create_jump_test,
        (program), {
        let ast_root = ast_root_from(program);
        cast_into_type::<Jump>(ast_root.get_root());
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

        cast_into_type::<Jump>(ast_root.get_root());
    }
}
