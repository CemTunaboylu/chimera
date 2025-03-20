use syntax::{language::SyntaxNode, syntax_kind::SyntaxKind};

use crate::{
    errors::ASTError,
    lang_elems::{ensure_node_kind_is, get_token},
};

#[derive(Clone, Debug, PartialEq)]
pub enum SelfRef {
    Instance,
    Struct,
}

impl TryFrom<&SyntaxNode> for SelfRef {
    type Error = ASTError;

    fn try_from(self_ref_node: &SyntaxNode) -> Result<Self, Self::Error> {
        _ = ensure_node_kind_is(self_ref_node, SyntaxKind::SelfRef)?;
        let token = get_token(self_ref_node).unwrap();
        let txt = token.text();
        let s = if txt == "self" {
            Self::Instance
        } else if txt == "Self" {
            Self::Struct
        } else {
            return Err(ASTError::new(
                token.text_range().into(),
                [SyntaxKind::Kwself, SyntaxKind::KwSelf].as_ref(),
                txt,
            ));
        };
        Ok(s)
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::ast::tests::{ast_root_from, cast_node_into_type};

    use parameterized_test::create;

    create! {
        happy_path_self_ref_test,
        (program, exp), {
            let ast_root = ast_root_from(program);
            let self_ref =
                cast_node_into_type::<SelfRef>(ast_root.get_root().first_child().as_ref().unwrap());
            assert_eq!(exp, self_ref);
        }
    }
    happy_path_self_ref_test! {
        valid_instance: ("self", SelfRef::Instance),
        valid_struct: ("Self", SelfRef::Struct),
    }
}
