use smol_str::{SmolStr, ToSmolStr};
use syntax::{
    can_be_a_parameter_with_mut, is_a_type,
    language::{NodeOrToken, SyntaxNode},
    syntax_kind::SyntaxKind,
};
use thin_vec::ThinVec;

use crate::{
    ast::ASTResult,
    errors::ASTError,
    lang_elems::{
        error_for_node, get_children_in, get_children_with_tokens_in_f, get_token,
        get_tokens_in_errs,
    },
    types::Type,
};
use SyntaxKind::*;

#[derive(Clone, Debug, PartialEq)]
pub enum By {
    Ref,
    RefMut,
    Value,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ParamType(pub By, pub Type);

#[derive(Clone, Debug, PartialEq)]
pub enum Param {
    /// Represents a lambda parameter without an explicit type annotation.
    Generic(SmolStr),
    Named(SmolStr, ParamType),
    SelfRef(By),
}

impl Param {
    pub fn get_params_nodes_from(fn_def_node: &SyntaxNode) -> ThinVec<SyntaxNode> {
        get_children_in(fn_def_node, ParamDecl)
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
        let exp = SyntaxKind::can_be_parameter();
        ASTError::new(got.text_range().into(), child.as_ref(), exp)
    }
    fn extract_from_ref(ref_node: &SyntaxNode) -> ASTResult<ParamType> {
        let node = get_children_with_tokens_in_f(ref_node, can_be_a_parameter_with_mut)
            .first()
            .cloned();
        if node.is_none() {
            return Err(Self::error_for_can_be_a_param_type(ref_node));
        }
        let node = node.unwrap();
        let param_type = match node {
            NodeOrToken::Node(node) => {
                if node.kind() == Mut {
                    // TODO: have a method like in HIR to make this easier
                    let last_token = node.children_with_tokens().last();
                    let typ = if matches!(&last_token, Some(node_or_token) if node_or_token.kind() != KwMut)
                    {
                        Type::try_from(last_token.as_ref().unwrap())?
                    } else {
                        let inside = node.first_child().unwrap();
                        Type::try_from(&inside)?
                    };
                    ParamType(By::RefMut, typ)
                } else {
                    let t = Type::try_from(&node)?;
                    ParamType(By::Ref, t)
                }
            }
            NodeOrToken::Token(token) => {
                let t = Type::try_from(&token)?;
                ParamType(By::Ref, t)
            }
        };
        Ok(param_type)
    }
    fn get_parameter_type_from_node(node: &NodeOrToken) -> ASTResult<ParamType> {
        let param_type = match node.kind() {
            PrefixUnaryOp => {
                let node = node.clone().into_node().unwrap();
                Self::validate_op(&node)?;
                Param::extract_from_ref(&node)?
            }
            SelfRef => ParamType(By::Value, Type::try_from(node)?),
            StructAsType => ParamType(By::Value, Type::try_from(node)?),
            type_ if is_a_type(type_) => ParamType(By::Value, Type::try_from(node)?),
            no => {
                let mut types = SyntaxKind::types();
                types.extend_from_slice(&[PrefixUnaryOp, SelfRef]);
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
        let child_as_param_type = param_decl_node
            .children_with_tokens()
            .filter(|not| !matches!(not.kind(), SyntaxKind::Whitespace | SyntaxKind::Comma))
            .last();
        if child_as_param_type.is_none() {
            let mut types = SyntaxKind::types();
            types.extend_from_slice(&[PrefixUnaryOp, SelfRef]);
            return Err(error_for_node(param_decl_node, types));
        }
        let node_or_token = child_as_param_type.unwrap();
        // untyped variable ('generic') for lambda
        if node_or_token.kind() == SyntaxKind::Ident {
            return Ok(Self::Generic(node_or_token.to_smolstr()));
        }
        let param_type = Self::get_parameter_type_from_node(&node_or_token)?;
        let param = if let Some(name) = get_token(param_decl_node).map(|t| t.text().to_smolstr()) {
            Self::Named(name, param_type)
        } else {
            Self::SelfRef(param_type.0)
        };

        Ok(param)
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
        (program, exp_by), {
        let ast_root = ast_root_from(program);
        let params_nodes = get_params_nodes_from(ast_root);
        let p = cast_node_into_type::<Param>(params_nodes.first().unwrap());
        match p {
            Param::SelfRef(by) => {
                assert_eq!(exp_by, by);
            },
            _ => {
                unreachable!()
            },
        }
        }
    }
    happy_path_self_ref_parameter_test! {
    param_ref_mut_self: ("fn f(&mut self) {}", By::RefMut ),
    param_ref_self: ("fn f(&self) {}", By::Ref),
    // param_mut_self: ("fn f(mut self) {}",By::Value),
    param_self: ("fn f(self) {}", By::Value),
    }

    create! {
        happy_path_named_parameter_test,
        (program, exp_by, exp_type), {
        let ast_root = ast_root_from(program);
        let params_nodes = get_params_nodes_from(ast_root);
        let p = cast_node_into_type::<Param>(params_nodes.first().unwrap());
        match p {
            Param::Named(name, param_type) => {
                assert_eq!(exp_by, param_type.0);
                assert_eq!(exp_type, param_type.1);
            },
            Param::SelfRef(_) => {
                unreachable!();
            },
            _ => unreachable!(),
        }
        }
    }

    fn get_params_nodes_from(ast_root: Root) -> ThinVec<SyntaxNode> {
        let fn_def_node = ast_root.get_root().first_child().unwrap();
        Param::get_params_nodes_from(&fn_def_node)
    }

    happy_path_named_parameter_test! {
    param_ref_mut_struct: ("fn f(s: &mut Structure) {}", By::RefMut, Type::Struct(SmolStr::from("Structure"))),
    param_ref_struct: ("fn f(s: &Structure) {}", By::Ref, Type::Struct(SmolStr::from("Structure"))),
    param_struct: ("fn f(s: Structure) {}", By::Value, Type::Struct(SmolStr::from("Structure"))),
    param_ref_mut_int: ("fn f(i: &mut i32) {}", By::RefMut, Type::Integer32),
    param_ref_int: ("fn f(i: &i32) {}", By::Ref, Type::Integer32),
    param_int: ("fn f(i: i32) {}", By::Value, Type::Integer32),
    }

    #[test]
    fn multiple_params() {
        let program = "fn f(&mut self, s: &Structure, c:i32) {}";
        let ast_root = ast_root_from(program);
        let params_nodes = get_params_nodes_from(ast_root);
        let assert_param_types = [
            |p: Param| assert!(matches!(p, Param::SelfRef(By::RefMut))),
            |p: Param| {
                if let Param::Named(name, param_type) = p {
                    assert_eq!(name, "s");
                    assert_eq!(param_type.0, By::Ref);
                    assert!(matches!(param_type.1, Type::Struct(_)));
                }
            },
            |p: Param| {
                if let Param::Named(name, param_type) = p {
                    assert_eq!(name, "c");
                    assert_eq!(param_type.0, By::Value);
                    assert!(matches!(param_type.1, Type::Integer32));
                }
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
            assert!(matches!(param, Param::Generic(name) if name == *n));
        }
    }
}
