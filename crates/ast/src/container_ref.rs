use smol_str::{SmolStr, ToSmolStr};
use syntax::{
    language::{SyntaxNode, SyntaxToken},
    syntax_kind::SyntaxKind,
};
use thin_vec::ThinVec;

use crate::{delimited::Indexing, errors::ASTError};
use SyntaxKind::*;

#[derive(Clone, Debug, PartialEq)]
pub struct ContainerRef {
    name: SyntaxToken, // should be token
    indices: ThinVec<Indexing>,
}

impl ContainerRef {
    pub fn name(&self) -> SmolStr {
        self.name.text().to_smolstr()
    }

    pub fn indices(&self) -> &ThinVec<Indexing> {
        &self.indices
    }
}

impl TryFrom<&SyntaxNode> for ContainerRef {
    type Error = ASTError;

    fn try_from(container_ref_node: &SyntaxNode) -> Result<Self, Self::Error> {
        let name = if let Some(name) = container_ref_node.first_token() {
            name
        } else {
            return Err(ASTError::new(
                container_ref_node.text_range().into(),
                Ident,
                container_ref_node.first_token().as_ref(),
            ));
        };
        let indices = Indexing::get_indexing_nodes_from(container_ref_node)
            .iter()
            .map(|node| Indexing(node.clone()))
            .collect::<ThinVec<_>>();

        Ok(Self { name, indices })
    }
}

#[cfg(test)]
mod tests {

    use crate::{
        ast::tests::{ast_root_from, cast_into_type},
        container_ref::ContainerRef,
        expression::Expr,
        literal::{Literal, Value},
        operation::Binary,
    };
    use parameterized_test::create;

    create! {
        happy_path_container_ref_test,
        (program, exp_name, indices_count), {
            let ast_root = ast_root_from(program);
            let container_ref_node = ast_root.get_root().first_child().unwrap();
            let container_ref= cast_into_type::<ContainerRef>(&container_ref_node);
            assert_eq!(exp_name, container_ref.name());
            assert!(indices_count== container_ref.indices.len());
        }
    }

    happy_path_container_ref_test! {
        double_infixed_index: (
         "arr[me.z - she.z]", "arr", 1,
        ),
        nested_index: (
         "tensor[tensor[0].len() -1]", "tensor", 1,
        ),
        nested_and_two_dim_index: (
         "tensor[tensor[0].len() -1][0]", "tensor", 2,
        ),
    }

    #[test]
    fn detailed_testing_of_complex_call() {
        let program = "tensor[tensor[0].len() -1][0]";
        let ast_root = ast_root_from(program);
        let container_ref_node = ast_root.get_root().first_child().unwrap();
        let container_ref: ContainerRef = cast_into_type::<ContainerRef>(&container_ref_node);
        let indices = container_ref.indices();
        let first_index_expr = indices[0].index().expect("should have been ok");
        assert!(matches!(first_index_expr, Expr::Infix(Binary::Infix(_))));
        if let Expr::Infix(binary) = &first_index_expr {
            assert!(matches!(
                binary.lhs().unwrap(),
                Expr::Infix(Binary::Infix(_))
            ));
            assert!(matches!(
                binary.rhs().unwrap(),
                Expr::Literal(Literal(Value::Int(_)))
            ));
            assert_eq!("-", binary.op().unwrap().text());
        }
    }
}
