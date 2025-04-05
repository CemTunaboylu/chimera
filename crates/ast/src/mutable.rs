use syntax::{language::SyntaxNode, syntax_kind::SyntaxKind};
use thin_vec::ThinVec;

use crate::{
    errors::ASTError,
    expression::Expr,
    lang_elems::{error_for_node, get_children_in},
};

#[derive(Clone, Debug, PartialEq)]
pub struct Mut(SyntaxNode);

impl Mut {
    pub fn get_mut_nodes_from(node: &SyntaxNode) -> ThinVec<SyntaxNode> {
        get_children_in(node, SyntaxKind::Mut)
    }
    pub fn expr(&self) -> Option<Expr> {
        Expr::try_from(&self.0).ok()
    }
}

impl TryFrom<&SyntaxNode> for Mut {
    type Error = ASTError;

    fn try_from(mut_node: &SyntaxNode) -> Result<Self, Self::Error> {
        if let Some(child) = mut_node.first_child() {
            Ok(Self(child))
        } else {
            return Err(error_for_node(mut_node, "to have a child"));
        }
    }
}
