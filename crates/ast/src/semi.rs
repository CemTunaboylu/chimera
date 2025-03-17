use syntax::{language::SyntaxNode, syntax_kind::SyntaxKind};

use crate::{errors::ASTError, lang_elems::error_for_node};

#[derive(Clone, Debug, PartialEq)]
pub struct Semi(SyntaxNode);

impl TryFrom<&SyntaxNode> for Semi {
    type Error = ASTError;

    fn try_from(semi_node: &SyntaxNode) -> Result<Self, Self::Error> {
        if semi_node.kind() != SyntaxKind::Semi {
            return Err(error_for_node(semi_node, SyntaxKind::Semi));
        }
        Ok(Self(semi_node.first_child().unwrap()))
    }
}
