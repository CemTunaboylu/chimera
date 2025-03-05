use smol_str::{SmolStr, ToSmolStr};
use syntax::{language::SyntaxNode, syntax_kind::SyntaxKind};

use crate::{
    errors::ASTError,
    expression::Expr,
    lang_elems::{err_with_recovered, error_for_node, get_children_as_expr, get_token},
};

#[derive(Debug)]
pub struct VarDef(VarRef, Option<Expr>);

impl VarDef {
    pub fn name(&self) -> SmolStr {
        self.0.name()
    }

    pub fn value(&self) -> Option<&Expr> {
        self.1.as_ref()
    }
}

impl TryFrom<SyntaxNode> for VarDef {
    type Error = ASTError;

    fn try_from(node: SyntaxNode) -> Result<Self, Self::Error> {
        let exprs = get_children_as_expr(&node)?;

        let infix = exprs.get(0);
        let infix = if let Some(Expr::Infix(inner_infix)) = infix {
            inner_infix
        } else {
            return Err(err_with_recovered(node, "a valid assignment"));
        };

        let var_ref = if let Some(Expr::VarRef(var_ref)) = infix.lhs() {
            var_ref.clone()
        } else {
            return Err(error_for_node(&node, SyntaxKind::VarRef));
        };
        let assignment = if let Some(expr) = infix.rhs() {
            expr.clone()
        } else {
            return Ok(Self(var_ref, None));
        };

        Ok(Self(var_ref, Some(assignment)))
    }
}

#[derive(Clone, Debug)]
pub struct VarRef(pub(crate) SyntaxNode);

impl VarRef {
    pub fn name(&self) -> SmolStr {
        get_token(&self.0).unwrap().text().to_smolstr()
    }
}
