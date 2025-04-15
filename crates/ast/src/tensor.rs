use syntax::{language::SyntaxNode, syntax_kind::SyntaxKind};
use thin_vec::{ThinVec, thin_vec};

use crate::{
    errors::ASTError,
    expression::Expr,
    lang_elems::{ensure_token_kind_is, error_for_node, get_children_in},
    literal::Literal,
    types::Hint,
};

#[derive(Clone, Debug, PartialEq)]
pub struct TensorInit {
    default_value: SyntaxNode,
    dim: Option<usize>,
}

impl TensorInit {
    pub fn get_default_value(&self) -> Option<Expr> {
        Expr::try_from(&self.default_value).ok()
    }
    pub fn get_dim(&self) -> Option<usize> {
        self.dim
    }
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
                    crate::literal::Value::Int(i) if i > 0 => Some(i as usize),
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
    pub dims: ThinVec<Hint>,
    pub type_hint: Option<Hint>,
    pub init: Option<TensorInit>,
}

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
    use crate::{ast_root_from, cast_node_into_type};
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
