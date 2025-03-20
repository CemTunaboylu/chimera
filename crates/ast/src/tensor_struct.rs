use syntax::{
    is_a_type,
    language::{NodeOrToken, SyntaxNode},
    syntax_kind::SyntaxKind,
};
use thin_vec::{ThinVec, thin_vec};

use crate::{
    ast::ASTResult,
    errors::ASTError,
    lang_elems::{
        ensure_token_kind_is, error_for_node, get_children_in, get_children_with_tokens_in_f,
    },
    literal::{Literal, ParsedValueIndex},
    types::Type,
};

#[derive(Clone, Debug, PartialEq)]
pub enum Hint {
    Dim(Literal),
    Type(Type),
}

impl Hint {
    fn dim_hints(dim_hints_node: &SyntaxNode) -> ASTResult<ThinVec<Self>> {
        let mut dim_hints = ThinVec::new();
        let hint_nodes = get_children_in(dim_hints_node, SyntaxKind::DimHint);
        let hint_nodes = hint_nodes
            .iter()
            .map(|node| Literal::try_from(node.first_child().as_ref().unwrap()));
        for dim_hint in hint_nodes {
            dim_hints.push(Hint::Dim(dim_hint?));
        }
        Ok(dim_hints)
    }

    fn type_hint(typehint_node: &SyntaxNode) -> ASTResult<Self> {
        let type_node = get_children_with_tokens_in_f(typehint_node, is_a_type);
        let type_node = type_node.first().unwrap();
        let type_ = match type_node {
            NodeOrToken::Node(node) => Type::try_from(node)?,
            NodeOrToken::Token(token) => Type::try_from(token)?,
        };
        Ok(Self::Type(type_))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TensorInit {
    default_value: SyntaxNode,
    dim: Option<ParsedValueIndex>,
}

impl TryFrom<&SyntaxNode> for TensorInit {
    type Error = ASTError;

    fn try_from(init_node: &SyntaxNode) -> Result<Self, Self::Error> {
        let nodes = init_node.children().collect::<ThinVec<_>>();
        if nodes.is_empty() {
            return Err(ASTError::new(
                init_node.text_range().into(),
                "have nodes for default value and optional dimension",
                nodes.as_ref(),
            ));
        }
        let default = nodes.first();
        let dim = if nodes.len() > 1 {
            let d = nodes.last().map(|node| Literal::try_from(node));
            if let Some(r) = d {
                let d = r?;
                match d.value() {
                    crate::literal::Value::Int(range) => Some(range),
                    _ => {
                        return Err(ASTError::new(
                            init_node.text_range().into(),
                            SyntaxKind::Int,
                            nodes.last(),
                        ));
                    }
                }
            } else {
                None
            }
        } else {
            None
        };
        Ok(Self {
            default_value: default.unwrap().clone(),
            dim,
        })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TensorStruct {
    dims: ThinVec<Hint>,
    type_hint: Option<Hint>,
    init: Option<TensorInit>,
}

impl TensorStruct {}

impl TryFrom<&SyntaxNode> for TensorStruct {
    type Error = ASTError;

    fn try_from(tensor_struct_node: &SyntaxNode) -> Result<Self, Self::Error> {
        let kw_token = tensor_struct_node.first_token().unwrap();
        _ = ensure_token_kind_is(&kw_token, SyntaxKind::KwTensor)?;

        let mut dim_hints = thin_vec![];
        let mut type_hint: Option<Hint> = None;
        let mut init: Option<TensorInit> = None;

        let expected = [
            SyntaxKind::DimHints,
            SyntaxKind::TypeHint,
            SyntaxKind::TensorInit,
        ];
        for node in get_children_in(tensor_struct_node, expected.as_ref()) {
            match node.kind() {
                SyntaxKind::DimHints => {
                    dim_hints = Hint::dim_hints(&node)?;
                }
                SyntaxKind::TypeHint => type_hint = Hint::type_hint(&node).ok(),
                SyntaxKind::TensorInit => {
                    init = Some(TensorInit::try_from(&node)?);
                }
                _ => return Err(error_for_node(&node, expected.as_ref())),
            }
        }

        Ok(Self {
            dims: dim_hints,
            type_hint,
            init,
        })
    }
}
#[cfg(test)]
mod tests {

    use super::*;
    use crate::ast::tests::{ast_root_from, cast_node_into_type};
    use parameterized_test::create;

    create! {
        create_tensor_struct_test,
        (program), {
        let ast_root = ast_root_from(program);
        let tensor_node = ast_root.get_root().first_child().unwrap();
        cast_node_into_type::<TensorStruct>(&tensor_node);
        }
    }

    create_tensor_struct_test! {
        tensor_struct: "Tensor<i32><3,3,3>",
        tensor_struct_typed: "Tensor<i32>",
        tensor_struct_dim_hinted: "Tensor<3,3,3>",
        tensor_struct_dim_hinted_init: "Tensor<3,3,3>[0.0]",
        tensor_struct_type_hinted_full_init: "Tensor<f32>[1.0;1000]",
    }

    #[test]
    #[should_panic]
    fn recovered_second_dims() {
        let program = "Tensor<3,3,3><3,3,3>";
        // will result with only Root@0..0, the overflow will raise a LexError
        // and won't be parsed by the parser but rather recorded as an error
        let ast_root = ast_root_from(program);
        let tensor_struct_node = ast_root.get_root().first_child().unwrap();
        cast_node_into_type::<TensorStruct>(&tensor_struct_node);
    }
    #[test]
    #[should_panic]
    fn recovered_second_types() {
        let program = "Tensor<f32><i32>";
        // will result with only Root@0..0, the overflow will raise a LexError
        // and won't be parsed by the parser but rather recorded as an error
        let ast_root = ast_root_from(program);
        let tensor_struct_node = ast_root.get_root().first_child().unwrap();
        cast_node_into_type::<TensorStruct>(&tensor_struct_node);
    }
}
