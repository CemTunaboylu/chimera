use smol_str::{SmolStr, ToSmolStr};
use syntax::{is_a_type, language::SyntaxToken, syntax_kind::SyntaxKind::*};
use syntax::{
    language::{NodeOrToken, SyntaxNode},
    syntax_kind::SyntaxKind,
};
use thin_vec::{ThinVec, thin_vec};

use crate::{
    ast::ASTResult,
    errors::ASTError,
    lang_elems::{
        ensure_node_kind_is_any, error_for_token, get_children_in, get_children_with_tokens_in_f,
        get_first_child_in,
    },
    literal::{Literal, Value},
    self_ref::SelfRef,
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

#[derive(Clone, Debug, PartialEq)]
pub enum Hint {
    Dim(usize),
    Type(Type),
}

impl Hint {
    pub fn dim_hints(dim_hints_node: &SyntaxNode) -> ASTResult<ThinVec<Self>> {
        let mut dim_hints = ThinVec::new();
        let hint_nodes = get_children_in(dim_hints_node, SyntaxKind::DimHint);
        let hint_nodes = hint_nodes
            .iter()
            .filter_map(|node| node.first_child().map(|child| Literal::try_from(&child)));
        for dim_hint in hint_nodes {
            let dim = match dim_hint?.0 {
                Value::Int(dim) if dim > 0 => dim,
                _ => {
                    return Err(ASTError::with_err_msg(
                        dim_hints_node.text_range().into(),
                        "dimension must be a positive integer".to_string(),
                    ));
                }
            };
            dim_hints.push(Hint::Dim(dim as usize));
        }
        Ok(dim_hints)
    }

    pub fn type_hint(typehint_node: &SyntaxNode) -> ASTResult<Self> {
        let type_node = get_children_with_tokens_in_f(typehint_node, is_a_type);
        let type_node = type_node.first().unwrap();
        let type_ = match type_node {
            NodeOrToken::Node(node) => Type::try_from(node)?,
            NodeOrToken::Token(token) => Type::try_from(token)?,
        };
        Ok(Self::Type(type_))
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
