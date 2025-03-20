use smol_str::{SmolStr, ToSmolStr};
use syntax::{
    can_be_a_parameter, can_be_a_parameter_with_mut, is_a_type,
    language::{NodeOrToken, SyntaxNode},
    syntax_kind::SyntaxKind,
};
use thin_vec::ThinVec;

use crate::{
    ast::ASTResult,
    errors::ASTError,
    lang_elems::{
        error_for_node, get_children_in, get_children_with_tokens_in_f, get_token, get_tokens_in,
        get_tokens_in_errs,
    },
};
use SyntaxKind::*;

#[derive(Clone, Debug, PartialEq)]
pub enum ParameterType {
    ByRef(NodeOrToken),
    ByRefMut(NodeOrToken),
    ByValueMut(NodeOrToken),
    ByValue(NodeOrToken),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Param {
    name: Option<SmolStr>,
    of_type: ParameterType,
}

impl Param {
    pub fn get_params_nodes_from(fn_def_node: &SyntaxNode) -> ThinVec<SyntaxNode> {
        get_children_in(&fn_def_node, ParamDecl)
    }
    fn validate_op(node: &SyntaxNode) -> ASTResult<()> {
        // note: in the future, raw pointers will be introduced thus we don't assume
        // it is only &, left as an arr now so that it would be easier to add it.
        let op = [And].as_ref();
        _ = get_tokens_in_errs(node, op)?;
        Ok(())
    }
    fn error_for_can_be_a_param_type(got: &SyntaxNode) -> ASTError {
        let child = got.first_child_or_token();
        let mut exp = SyntaxKind::can_be_parameter();
        exp.push(Mut);
        return ASTError::new(got.text_range().into(), child.as_ref(), exp);
    }
    fn extract_from_ref(ref_node: &SyntaxNode) -> ASTResult<ParameterType> {
        let node = get_children_with_tokens_in_f(ref_node, can_be_a_parameter_with_mut)
            .first()
            .map(Clone::clone);
        let param_type = if let Some(node) = node {
            if node.kind() == Mut {
                ParameterType::ByRefMut(node)
            } else {
                ParameterType::ByRef(node)
            }
        } else {
            return Err(Self::error_for_can_be_a_param_type(ref_node));
        };
        Ok(param_type)
    }
    fn extract_from_mut(mut_node: &SyntaxNode) -> ASTResult<ParameterType> {
        let node = get_children_with_tokens_in_f(mut_node, can_be_a_parameter)
            .first()
            .map(Clone::clone);

        if let Some(node) = node {
            Ok(ParameterType::ByValueMut(node))
        } else {
            return Err(Self::error_for_can_be_a_param_type(mut_node));
        }
    }
    fn get_parameter_type_from_node(node: &NodeOrToken) -> ASTResult<ParameterType> {
        let param_type = match node.kind() {
            PrefixUnaryOp => {
                let node = node.clone().into_node().unwrap();
                _ = Self::validate_op(&node)?;
                Param::extract_from_ref(&node)?
            }
            Mut => {
                let node = node.clone().into_node().unwrap();
                Param::extract_from_mut(&node)?
            }
            StructAsType => ParameterType::ByValue(node.clone()),
            SelfRef => ParameterType::ByValue(node.clone()),
            type_ if is_a_type(&type_) => ParameterType::ByValue(node.clone()),
            no => {
                let mut types = SyntaxKind::types();
                types.extend_from_slice(&[PrefixUnaryOp, Mut, SelfRef]);
                return Err(ASTError::new(node.text_range().into(), types, no));
            }
        };
        Ok(param_type)
    }
}

impl TryFrom<&SyntaxNode> for Param {
    type Error = ASTError;
    /*
    ParamDecl@10..17
        Ident@10..13 "one"
        Colon@13..14 ":"
        TyI32@14..17 "i32"
    ParamDecl@13..22
        PrefixUnaryOp@13..22
            And@13..14 "&"
            Mut@14..22
                KwMut@14..17 "mut"
                Whitespace@17..18 " "
                SelfRef@18..22
                    Kwself@18..22 "self"
    ParamDecl@23..37
        Ident@23..25 "by"
        Colon@25..26 ":"
        Whitespace@26..27 " "
        PrefixUnaryOp@27..37
            And@27..28 "&"
            Mut@28..37
                KwMut@28..31 "mut"
                Whitespace@31..32 " "
                StructAsType@32..37
                    Ident@32..37 "Point"
    */

    fn try_from(param_decl_node: &SyntaxNode) -> Result<Self, Self::Error> {
        let name = get_token(param_decl_node).map(|t| t.text().to_smolstr());
        let child_as_param_type = param_decl_node.last_child_or_token();
        let of_type = if let Some(node_or_token) = child_as_param_type {
            Self::get_parameter_type_from_node(&node_or_token)?
        } else {
            let mut types = SyntaxKind::types();
            types.extend_from_slice(&[PrefixUnaryOp, Mut, SelfRef]);
            return Err(error_for_node(param_decl_node, types));
        };
        Ok(Self { name, of_type })
    }
}

#[cfg(test)]
mod tests {
    use thin_vec::ThinVec;

    use super::*;
    use crate::ast::{
        Root,
        tests::{ast_root_from, cast_node_into_type},
    };

    use parameterized_test::create;

    create! {
        happy_path_parameter_test,
        (program, assertion, text), {
        let ast_root = ast_root_from(program);
        let params_nodes = get_params_nodes_from(ast_root);
        let p = cast_node_into_type::<Param>(params_nodes.first().unwrap());
        assertion(p.of_type, text);
        }
    }

    fn get_params_nodes_from(ast_root: Root) -> ThinVec<SyntaxNode> {
        let fn_def_node = ast_root.get_root().first_child().unwrap();
        Param::get_params_nodes_from(&fn_def_node)
    }

    fn get_text_from(node_or_token: NodeOrToken) -> String {
        match node_or_token {
            NodeOrToken::Node(node) => node.text().to_string(),
            NodeOrToken::Token(token) => token.text().to_string(),
        }
    }

    fn assert_by_ref_mut(of_type: ParameterType, text: &str) {
        match of_type {
            ParameterType::ByRefMut(node_or_token) => {
                assert_eq!(text, get_text_from(node_or_token))
            }
            _ => unreachable!(),
        }
    }

    fn assert_by_ref(of_type: ParameterType, text: &str) {
        match of_type {
            ParameterType::ByRef(node_or_token) => {
                assert_eq!(text, get_text_from(node_or_token))
            }
            _ => unreachable!(),
        }
    }

    fn assert_by_value(of_type: ParameterType, text: &str) {
        match of_type {
            ParameterType::ByValue(node_or_token) => {
                assert_eq!(text, get_text_from(node_or_token))
            }
            _ => unreachable!(),
        }
    }

    fn assert_by_value_mut(of_type: ParameterType, text: &str) {
        match of_type {
            ParameterType::ByValueMut(node_or_token) => {
                assert_eq!(text, get_text_from(node_or_token))
            }
            _ => unreachable!(),
        }
    }

    happy_path_parameter_test! {
    param_ref_mut_self: ("fn f(&mut self) {}", assert_by_ref_mut, "mut self"),
    param_ref_self: ("fn f(&self) {}", assert_by_ref, "self"),
    param_mut_self: ("fn f(mut self) {}", assert_by_value_mut, "self"),
    param_self: ("fn f(self) {}", assert_by_value, "self"),
    param_ref_mut_struct: ("fn f(s: &mut Structure) {}", assert_by_ref_mut, "mut Structure"),
    param_ref_struct: ("fn f(s: &Structure) {}", assert_by_ref, "Structure"),
    param_mut_struct: ("fn f(s: mut Structure) {}", assert_by_value_mut, "Structure"),
    param_struct: ("fn f(s: Structure) {}", assert_by_value, "Structure"),
    param_ref_mut_int: ("fn f(i: &mut i32) {}", assert_by_ref_mut, "mut i32"),
    param_ref_int: ("fn f(i: &i32) {}", assert_by_ref, "i32"),
    param_mut_int: ("fn f(i: mut i32) {}", assert_by_value_mut, "i32"),
    param_int: ("fn f(i: i32) {}", assert_by_value, "i32"),
    }

    #[test]
    fn multiple_params() {
        let program = "fn f(&mut self, s: &Structure, c:i32) {}";
        let ast_root = ast_root_from(program);
        let params_nodes = get_params_nodes_from(ast_root);
        let names = [None, Some("s"), Some("c")];
        let assert_param_types = [
            |p: Param| assert_by_ref_mut(p.of_type, "mut self"),
            |p: Param| assert_by_ref(p.of_type, "Structure"),
            |p: Param| assert_by_value(p.of_type, "i32"),
        ];
        for (ix, param_node) in params_nodes.iter().enumerate() {
            let p = cast_node_into_type::<Param>(param_node);
            let exp_name = names[ix];
            assert_eq!(exp_name, p.name.as_ref().map(|smol| smol.as_str()));
            let assertion = assert_param_types[ix];
            assertion(p);
        }
    }
}
