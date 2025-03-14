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
    pub fn name(&self) -> &SmolStr {
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

        let infix = if let Some(Expr::Infix(inner_infix)) = exprs.get(0) {
            inner_infix
        } else {
            return Err(err_with_recovered(node, "a valid assignment"));
        };

        let var_ref = if let Some(Expr::VarRef(var_ref)) = infix.lhs() {
            var_ref.clone()
        } else {
            return Err(error_for_node(&node, SyntaxKind::VarRef));
        };
        let assignment = infix.rhs().map(|e| e.clone());
        Ok(Self(var_ref, assignment))
    }
}

#[derive(Clone, Debug)]
pub struct VarRef(pub(crate) SmolStr);

impl TryFrom<&SyntaxNode> for VarRef {
    type Error = ASTError;

    fn try_from(var_ref_node: &SyntaxNode) -> Result<Self, Self::Error> {
        if var_ref_node.kind() != SyntaxKind::VarRef {
            return Err(error_for_node(var_ref_node, SyntaxKind::VarRef));
        }
        let name = get_token(var_ref_node).unwrap().text().to_smolstr();
        Ok(Self(name))
    }
}

impl VarRef {
    pub fn name(&self) -> &SmolStr {
        &self.0
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::ast::tests::ast_root_from;
    use parameterized_test::create;

    create! {
        create_var_ref_test,
        (program), {
        let ast_root = ast_root_from(program);
        let var_ref = VarRef::try_from(ast_root.get_root().first_child().as_ref().unwrap()).expect("should have been ok");
        assert_eq!(program, var_ref.name().as_str());
        }
    }

    create_var_ref_test! {
        valid_var_ref: "var",
    }
}
