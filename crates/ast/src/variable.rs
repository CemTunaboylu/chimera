use std::ops::Range;

use smol_str::{SmolStr, ToSmolStr};
use syntax::{language::SyntaxNode, syntax_kind::SyntaxKind};

use crate::{
    errors::ASTError,
    expression::Expr,
    lang_elems::{
        ensure_node_kind_is, error_for_node, first_child_of_kind_errs, get_first_child_in,
        get_token,
    },
    operation::Binary,
    types::Hint,
};

#[derive(Clone, Debug, PartialEq)]
pub struct VarDef {
    var_ref: VarRef,
    is_mut: bool,
    expr: Option<Expr>,
    span: Range<usize>,
}

impl VarDef {
    pub fn var_ref(&self) -> &VarRef {
        &self.var_ref
    }
    pub fn name(&self) -> &SmolStr {
        self.var_ref.name()
    }
    pub fn is_mut(&self) -> bool {
        self.is_mut
    }
    pub fn value(&self) -> Option<&Expr> {
        self.expr.as_ref()
    }
    pub fn span(&self) -> Range<usize> {
        self.span.clone()
    }
    pub fn type_hint(&self) -> Option<&Hint> {
        self.var_ref.type_hint()
    }
}

impl TryFrom<&SyntaxNode> for VarDef {
    type Error = ASTError;

    fn try_from(var_def_node: &SyntaxNode) -> Result<Self, Self::Error> {
        let mut child = first_child_of_kind_errs(
            &var_def_node,
            [SyntaxKind::InfixBinOp, SyntaxKind::Mut].as_ref(),
        )?;
        let is_mut = child.kind() == SyntaxKind::Mut;
        if is_mut {
            child = first_child_of_kind_errs(&child, SyntaxKind::InfixBinOp)?;
        }
        let bin = Binary::new(&child)?;

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
            var_ref,
            is_mut,
            expr: assignment,
            span,
        })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarRef {
    name: SmolStr,
    span: Range<usize>,
    type_hint: Option<Hint>,
}

fn get_type_hint(var_ref_node: &SyntaxNode) -> Option<Hint> {
    let type_hint_node = get_first_child_in(var_ref_node, SyntaxKind::TypeHint)?;
    // note: silenlty ignoring the failed type hint
    Hint::type_hint(&type_hint_node).ok()
}

impl TryFrom<&SyntaxNode> for VarRef {
    type Error = ASTError;

    fn try_from(var_ref_node: &SyntaxNode) -> Result<Self, Self::Error> {
        _ = ensure_node_kind_is(var_ref_node, SyntaxKind::VarRef)?;
        let name = get_token(var_ref_node).unwrap().text().to_smolstr();
        let type_hint = get_type_hint(var_ref_node);
        let span = var_ref_node.text_range().into();
        Ok(Self {
            name,
            span,
            type_hint,
        })
    }
}

impl VarRef {
    pub fn name(&self) -> &SmolStr {
        &self.name
    }
    pub fn span(&self) -> Range<usize> {
        self.span.clone()
    }
    pub fn type_hint(&self) -> Option<&Hint> {
        self.type_hint.as_ref()
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::types::Type;
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
        let program = "let diff_norm : Point = (point_1.locus() - point_2.locus()).normalize();";
        let ast_root = ast_root_from(program);
        let var_def_node = ast_root.get_root().first_child().unwrap();
        let var_def = cast_node_into_type::<VarDef>(&var_def_node);
        assert_eq!("diff_norm", var_def.name().as_str());
        assert_eq!(
            &Hint::Type(Type::Struct(SmolStr::from("Point"))),
            var_def.type_hint().expect("should have a type hint")
        );
        assert!(!var_def.is_mut);
    }

    #[test]
    fn mut_var_def() {
        let program = "let mut diff_norm = (point_1.locus() - point_2.locus()).normalize();";
        let ast_root = ast_root_from(program);
        let var_def_node = ast_root.get_root().first_child().unwrap();
        let var_def = cast_node_into_type::<VarDef>(&var_def_node);
        assert_eq!("diff_norm", var_def.name().as_str());
        assert!(var_def.is_mut);
    }
}
