use syntax::language::SyntaxNode;

use crate::{ast::ASTResult, expression::Expr, lang_elems::get_single_children_as_expr};

#[derive(Clone, Debug)]
pub struct Paren(pub SyntaxNode);

impl Paren {
    pub fn expr(&self) -> ASTResult<Expr> {
        get_single_children_as_expr(&self.0)
    }
}
