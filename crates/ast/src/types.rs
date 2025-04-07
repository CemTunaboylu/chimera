use smol_str::{SmolStr, ToSmolStr};
use syntax::{language::SyntaxToken, syntax_kind::SyntaxKind::*};
use syntax::{
    language::{NodeOrToken, SyntaxNode},
    syntax_kind::SyntaxKind,
};
use thin_vec::{ThinVec, thin_vec};

use crate::{
    errors::ASTError,
    lang_elems::{ensure_node_kind_is_any, error_for_token, get_first_child_in},
    self_ref::SelfRef,
    tensor::Hint,
};

// possible types are primitives + custom types i.e. structs
#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Bool,
    Byte,
    Char,
    Float32,
    Integer32,
    String,
    Struct(SmolStr),
    SelfRef(SelfRef),
    Tensor(ThinVec<Hint>),
}

impl TryFrom<&SyntaxNode> for Type {
    type Error = ASTError;

    fn try_from(parent_node: &SyntaxNode) -> Result<Self, Self::Error> {
        let t = match parent_node.kind() {
            SelfRef => Self::SelfRef(SelfRef::try_from(parent_node)?),
            TensorType => {
                let mut hints = thin_vec![];
                if let Some(type_hint) = get_first_child_in(parent_node, SyntaxKind::TypeHint) {
                    let hint = Hint::type_hint(&type_hint)?;
                    hints.push(hint);
                }
                if let Some(dim_hints) = get_first_child_in(parent_node, SyntaxKind::DimHints) {
                    hints.extend(Hint::dim_hints(&dim_hints)?);
                }
                Self::Tensor(hints)
            }
            _ => {
                _ = ensure_node_kind_is_any(parent_node, [StructAsType, TensorType].as_ref())?;
                unreachable!()
            }
        };
        Ok(t)
    }
}

impl TryFrom<&SyntaxToken> for Type {
    type Error = ASTError;

    fn try_from(token: &SyntaxToken) -> Result<Self, Self::Error> {
        let t = match token.kind() {
            TyBool => Self::Bool,
            TyChar => Self::Char,
            TyF32 => Self::Float32,
            TyI32 => Self::Integer32,
            // str slice is a ref str
            TyStr | TyStrSlc => Self::String,
            StructAsType => Self::Struct(token.text().to_smolstr()),
            _ => return Err(error_for_token(token, SyntaxKind::types())),
        };

        Ok(t)
    }
}

impl TryFrom<&NodeOrToken> for Type {
    type Error = ASTError;

    fn try_from(not: &NodeOrToken) -> Result<Self, Self::Error> {
        match not {
            NodeOrToken::Node(node) => Self::try_from(node),
            NodeOrToken::Token(token) => Self::try_from(token),
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::{ast::ASTResult, ast_root_from, cast_node_into_type, cast_token_into_type};
    use parameterized_test::create;

    create! {
        create_type_test,
        (program), {
        let ast_root = ast_root_from(program);
        let token = ast_root.get_root().first_token().unwrap();
        cast_token_into_type::<Type>(&token);
        }
    }

    create_type_test! {
        valid_bool: "bool",
        // valid_byte: "byte",
        valid_char: "char",
        valid_f32: "f32",
        valid_i32: "i32",
        valid_string: "String",
        valid_str_slice: "str",
    }

    #[test]
    fn valid_struct_identifier() {
        let program = "fn def(arg:Structure)";
        let ast_root = ast_root_from(program);

        let fn_def_node = ast_root.get_root().first_child().unwrap();
        let param_node = fn_def_node
            .children()
            .find(|node| node.kind() == ParamDecl)
            .unwrap();
        let struct_as_type = param_node.last_token().unwrap();
        cast_token_into_type::<Type>(&struct_as_type);
    }

    #[test]
    fn valid_tensor_identifier() {
        let program = "tensor<i32><10,10,10>";
        let ast_root = ast_root_from(program);

        let node = ast_root.get_root().first_child().unwrap();
        cast_node_into_type::<Type>(&node);
    }
    #[test]
    fn invalid_type() {
        let program = "structure";
        let ast_root = ast_root_from(program);
        let child = ast_root.get_root().first_child().unwrap();

        let result: ASTResult<Type> = (&child).try_into();
        let err = result.expect_err("should have been errored");
        assert_eq!(
            "ASTError { err_span: 0..9, expected_but_found: \"expected TensorType or StructAsType, but got VarRef\" }",
            format!("{:?}", err)
        );
    }
}
