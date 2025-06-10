use std::ops::Range;

use smol_str::SmolStr;
use syntax::{
    bitset::SyntaxKindBitSet,
    language::{NodeOrToken, SyntaxNode},
    syntax_kind::SyntaxKind,
};
use thin_vec::ThinVec;

use crate::{
    ast::ASTResult,
    errors::ASTError,
    expression::Expr,
    lang_elems::{
        error_for_node, filtered_children_with_tokens, get_children_in, get_first_child_in,
        get_name, get_token,
    },
    self_ref::SelfRef as ASTSelfRef,
    types::{Type, parse_type_hinted},
};
use SyntaxKind::*;

#[derive(Clone, Debug, PartialEq)]
pub struct Param {
    pub by_ref: bool,
    pub is_mut: bool,
    pub name: SmolStr,
    pub param_type: Option<Type>,
    pub span: Range<usize>,
}

impl Param {
    pub fn new_self_ref(by_ref: bool, is_mut: bool, span: Range<usize>) -> Self {
        Self {
            by_ref,
            is_mut,
            name: SmolStr::from(""),
            param_type: Some(Type::SelfRef(ASTSelfRef::Instance)),
            span,
        }
    }
    pub fn new_named(
        is_mut: bool,
        name: SmolStr,
        param_type: Option<Type>,
        span: Range<usize>,
    ) -> Self {
        Self {
            by_ref: false,
            is_mut,
            name,
            param_type,
            span,
        }
    }
    pub fn new_generic(name: SmolStr, is_mut: bool, span: Range<usize>) -> Self {
        Self {
            by_ref: false,
            is_mut,
            name,
            param_type: None,
            span,
        }
    }
    pub fn get_params_nodes_from(fn_def_node: &SyntaxNode) -> ThinVec<SyntaxNode> {
        get_children_in(fn_def_node, ParamDecl)
    }
    fn get_parameter_related_nodes(
        param_decl_node: &SyntaxNode,
    ) -> ASTResult<ThinVec<NodeOrToken>> {
        use SyntaxKind::*;
        let allowed_nodes: SyntaxKindBitSet =
            [Mut, PrefixUnaryOp, TypeHint, VarRef].as_ref().into();
        let can_be_param: SyntaxKindBitSet = SyntaxKind::can_be_parameter().as_ref().into();
        let nodes = filtered_children_with_tokens(param_decl_node, allowed_nodes + can_be_param);
        if nodes.is_empty() {
            return Err(error_for_node(
                param_decl_node,
                allowed_nodes + can_be_param,
            ));
        }
        Ok(nodes)
    }
    fn try_parsing_self_ref(child_node: &SyntaxNode) -> Option<Param> {
        // * Can be &mut self, &self, mut self or self.
        let mut expected: SyntaxKindBitSet = [PrefixUnaryOp, Mut, SelfRef].as_ref().into();
        use SyntaxKind::*;
        if !expected.contains(child_node.kind()) {
            return None;
        }
        let mut is_mut = false;
        let mut by_ref = false;

        let mut first_child = if child_node.kind() == PrefixUnaryOp {
            let op = get_token(child_node).unwrap();
            if op.text() != "&" {
                return None;
            }
            by_ref = true;
            expected -= PrefixUnaryOp.into();
            if let Some(c) = get_first_child_in(child_node, expected) {
                c
            } else {
                return None;
            }
        } else {
            child_node.clone()
        };
        loop {
            match first_child.kind() {
                Mut => {
                    is_mut = true;
                    expected -= Mut.into();
                    expected -= PrefixUnaryOp.into();
                    first_child = get_first_child_in(&first_child, expected)?;
                }
                SelfRef => {
                    return Some(Param::new_self_ref(
                        by_ref,
                        is_mut,
                        child_node.text_range().into(),
                    ));
                }
                _ => break,
            }
        }

        return None;
    }
    fn parse_type_hinted(type_hint_node: &SyntaxNode) -> ASTResult<Param> {
        let (type_hinted, ty) = parse_type_hinted(type_hint_node)?;
        let mut is_mut = false;
        let span: Range<usize> = type_hint_node.text_range().into();
        let name = match type_hinted {
            Expr::Mut(mut_expr) => {
                is_mut = true;
                mut_expr.expr().name().unwrap()
            }
            Expr::VarRef(var_ref) => var_ref.name().clone(),
            _ => {
                return Err(ASTError::with_err_msg(
                    span,
                    format!(
                        "expected a valid identifier to type hint but got {:?}",
                        type_hinted
                    ),
                ));
            }
        };
        Ok(Self::new_named(is_mut, name, Some(ty), span))
    }
}

impl TryFrom<&SyntaxNode> for Param {
    type Error = ASTError;
    fn try_from(param_decl_node: &SyntaxNode) -> Result<Self, Self::Error> {
        let mut children = Self::get_parameter_related_nodes(param_decl_node)?;
        let mut is_mut = false;
        let kind = children.first().unwrap().kind();
        if kind == TypeHint {
            let type_hint_node = children.first().unwrap().as_node().unwrap();
            return Self::parse_type_hinted(&type_hint_node);
        }
        if kind == Mut {
            is_mut = true;
            children =
                Self::get_parameter_related_nodes(&children.first().unwrap().as_node().unwrap())?;
        }
        let child = children.first().unwrap();
        let span: Range<usize> = child.text_range().into();
        // Only attempt to form SelfRef if there is only one child
        if children.len() == 1 && child.as_node().is_some() {
            let node = child.as_node().unwrap();
            if let Some(mut param) = Self::try_parsing_self_ref(&node) {
                if is_mut {
                    param.is_mut = true;
                }
                return Ok(param);
            }
        }
        let name = get_name(child);
        Ok(Self::new_generic(name, is_mut, span))
    }
}

#[cfg(test)]
mod tests {
    use thin_vec::ThinVec;

    use super::*;
    use crate::{ast::Root, ast_root_from, cast_node_into_type};

    use parameterized_test::create;

    create! {
        happy_path_self_ref_parameter_test,
        (program, exp_by_ref, exp_is_mut), {
        let ast_root = ast_root_from(program);
        let params_nodes = get_params_nodes_from(ast_root);
        let p = cast_node_into_type::<Param>(params_nodes.first().unwrap());
        let Param{ by_ref, is_mut, param_type, name, .. } = p;
        assert_eq!(exp_by_ref, by_ref);
        assert_eq!(exp_is_mut, is_mut);
        assert_eq!(name, SmolStr::from(""));
        assert_eq!(param_type, Some(Type::SelfRef(ASTSelfRef::Instance)));
        }
    }
    const BY_REF: bool = true;
    const IS_MUT: bool = true;
    happy_path_self_ref_parameter_test! {
    param_ref_mut_self: ("fn f(&mut self) {}", BY_REF, IS_MUT),
    param_ref_self: ("fn f(&self) {}", BY_REF, !IS_MUT),
    param_mut_self: ("fn f(mut self) {}", !BY_REF, IS_MUT),
    param_self: ("fn f(self) {}", !BY_REF, !IS_MUT),
    }

    create! {
        happy_path_named_parameter_test,
        (program, exp_is_mut, exp_name, exp_type), {
        let ast_root = ast_root_from(program);
        let params_nodes = get_params_nodes_from(ast_root);
        let p = cast_node_into_type::<Param>(params_nodes.first().unwrap());

        let Param{ by_ref, is_mut, param_type, name, .. } = p;
        assert_eq!(exp_is_mut, is_mut);
        assert_eq!(name, SmolStr::from(exp_name));
        assert_eq!(param_type, Some(exp_type));

        }
    }

    fn get_params_nodes_from(ast_root: Root) -> ThinVec<SyntaxNode> {
        let fn_def_node = ast_root.get_root().first_child().unwrap();
        Param::get_params_nodes_from(&fn_def_node)
    }

    happy_path_named_parameter_test! {
    param_ref_mut_struct: ("fn f(s: &mut Structure) {}", !IS_MUT, "s", Type::Pointer{is_mut: IS_MUT, ty: Box::new(Type::Struct(SmolStr::from("Structure")))}),
    param_ref_struct: ("fn f(s: &Structure) {}", !IS_MUT, "s", Type::Pointer{is_mut: !IS_MUT, ty: Box::new(Type::Struct(SmolStr::from("Structure")))}),
    param_struct: ("fn f(s: Structure) {}", !IS_MUT, "s", Type::Struct(SmolStr::from("Structure"))),
    mut_param_ref_mut_struct: ("fn f(mut s: &mut Structure) {}", IS_MUT, "s", Type::Pointer{is_mut: true, ty: Box::new(Type::Struct(SmolStr::from("Structure")))}),
    mut_param_ref_struct: ("fn f(mut s: &Structure) {}", IS_MUT, "s", Type::Pointer{is_mut: !IS_MUT, ty: Box::new(Type::Struct(SmolStr::from("Structure")))}),
    mut_param_struct: ("fn f(mut s: Structure) {}", IS_MUT, "s", Type::Struct(SmolStr::from("Structure"))),
    param_ref_mut_int: ("fn f(i: &mut i32) {}", !IS_MUT, "i", Type::Pointer{is_mut: IS_MUT, ty: Box::new(Type::Integer32)}),
    param_ref_int: ("fn f(i: &i32) {}", !IS_MUT, "i", Type::Pointer{is_mut: !IS_MUT, ty: Box::new(Type::Integer32)}),
    param_int: ("fn f(i: i32) {}", !IS_MUT, "i", Type::Integer32),
    }

    #[test]
    fn multiple_params() {
        let program = "fn f(&mut self, s: &Structure, c:i32) {}";
        let ast_root = ast_root_from(program);
        let params_nodes = get_params_nodes_from(ast_root);
        let assert_param_types = [
            |p: Param| assert_eq!(p, Param::new_self_ref(BY_REF, IS_MUT, 5..14)),
            |p: Param| {
                let Param {
                    by_ref,
                    is_mut,
                    param_type,
                    name,
                    ..
                } = p;
                assert_eq!(name, "s");
                assert_eq!(is_mut, !IS_MUT);
                assert_eq!(by_ref, !BY_REF);
                assert_eq!(
                    param_type,
                    Some(Type::Pointer {
                        is_mut: !IS_MUT,
                        ty: Box::new(Type::Struct(SmolStr::from("Structure")))
                    })
                );
            },
            |p: Param| {
                let Param {
                    by_ref,
                    is_mut,
                    param_type,
                    name,
                    ..
                } = p;
                assert_eq!(name, "c");
                assert_eq!(is_mut, !IS_MUT);
                assert_eq!(by_ref, !BY_REF);
                assert_eq!(param_type, Some(Type::Integer32));
            },
        ];
        for (ix, param_node) in params_nodes.iter().enumerate() {
            let p = cast_node_into_type::<Param>(param_node);
            let assertion = assert_param_types[ix];
            assertion(p);
        }
    }

    #[test]
    fn generic_lambda_params() {
        let program = "|g, en, eric| {g+en+eric}";
        let ast_root = ast_root_from(program);

        let literal_node = ast_root.get_root().first_child().unwrap();
        let lambda_node = literal_node.first_child().unwrap();
        let params_nodes = Param::get_params_nodes_from(&lambda_node);

        let assert_param_names = ["g", "en", "eric"];
        for (p, n) in params_nodes.iter().zip(assert_param_names.iter()) {
            let param = cast_node_into_type::<Param>(&p);
            let Param {
                by_ref,
                is_mut,
                param_type,
                name,
                ..
            } = param;
            assert_eq!(name, SmolStr::from(*n));
            assert_eq!(by_ref, false);
            assert_eq!(is_mut, false);
            assert_eq!(param_type, None);
        }
    }
}
