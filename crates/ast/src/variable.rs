use std::ops::Range;

use smol_str::{SmolStr, ToSmolStr};
use syntax::{language::SyntaxNode, syntax_kind::SyntaxKind};

use crate::{
    errors::ASTError,
    expression::Expr,
    lang_elems::{ensure_node_kind_is, error_for_node, get_children_in_errs, get_token},
    operation::Binary,
};

#[derive(Clone, Debug, PartialEq)]
pub struct VarDef {
    name: VarRef,
    expr: Option<Expr>,
    span: Range<usize>,
}

impl VarDef {
    pub fn name(&self) -> &SmolStr {
        self.name.name()
    }

    pub fn value(&self) -> Option<&Expr> {
        self.expr.as_ref()
    }
    pub fn span(&self) -> Range<usize> {
        self.span.clone()
    }
}

impl TryFrom<&SyntaxNode> for VarDef {
    type Error = ASTError;

    fn try_from(var_def_node: &SyntaxNode) -> Result<Self, Self::Error> {
        let children = get_children_in_errs(&var_def_node, SyntaxKind::InfixBinOp)?;
        let infix_node = children.first().unwrap();
        let bin = Binary::new(&infix_node)?;

        let var_ref = if let Some(Expr::VarRef(var_ref)) = bin.lhs() {
            var_ref.clone()
        } else {
            return Err(error_for_node(
                &bin.get_pre_computed().get_node(),
                SyntaxKind::VarRef,
            ));
        };
        let assignment = bin.rhs().map(|e| e.clone());
        let span = var_def_node.text_range().into();
        Ok(Self {
            name: var_ref,
            expr: assignment,
            span,
        })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarRef {
    name: SmolStr,
    span: Range<usize>,
}

impl TryFrom<&SyntaxNode> for VarRef {
    type Error = ASTError;

    fn try_from(var_ref_node: &SyntaxNode) -> Result<Self, Self::Error> {
        _ = ensure_node_kind_is(var_ref_node, SyntaxKind::VarRef)?;
        let name = get_token(var_ref_node).unwrap().text().to_smolstr();
        let span = var_ref_node.text_range().into();
        Ok(Self { name, span })
    }
}

impl VarRef {
    pub fn name(&self) -> &SmolStr {
        &self.name
    }
    pub fn span(&self) -> Range<usize> {
        self.span.clone()
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::{ast_root_from, cast_node_into_type};

    #[test]
    fn valid_var_ref() {
        let program = "var";
        let ast_root = ast_root_from(program);
        let var_ref =
            cast_node_into_type::<VarRef>(ast_root.get_root().first_child().as_ref().unwrap());
        assert_eq!(program, var_ref.name().as_str());
    }

    #[test]
    fn var_def() {
        let program = "let diff_norm = (point_1.locus() - point_2.locus()).normalize();";
        let ast_root = ast_root_from(program);
        let var_def_node = ast_root.get_root().first_child().unwrap();
        let var_def = cast_node_into_type::<VarDef>(&var_def_node);
        assert_eq!("diff_norm", var_def.name().as_str());
    }
}
