use syntax::{language::SyntaxNode, syntax_kind::SyntaxKind};

use crate::{
    errors::ASTError,
    expression::Expr,
    lang_elems::{children_with_tokens_without_unwanted, error_for_node},
};

// In Rust‐style syntax, it is only ever seen “&mut T” or “&T” in type hints—there’s no syntax for “mut T” at the type‐level
// isolation. Instead, “mut” attaches to a pattern or variable binding, not directly to a “bare” type
#[derive(Clone, Debug, PartialEq)]
pub struct Mut(pub Box<Expr>);

impl Mut {
    pub fn expr(&self) -> &Expr {
        &self.0
    }
}

impl TryFrom<&SyntaxNode> for Mut {
    type Error = ASTError;

    fn try_from(mut_node: &SyntaxNode) -> Result<Self, Self::Error> {
        use SyntaxKind::{KwMut, Whitespace};
        if let Some(child) =
            children_with_tokens_without_unwanted(mut_node, [KwMut, Whitespace].as_ref()).first()
        {
            let expr = Expr::try_from(child)?;
            Ok(Self(Box::new(expr)))
        } else {
            Err(error_for_node(mut_node, "to have a child"))
        }
    }
}
