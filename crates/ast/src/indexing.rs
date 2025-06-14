use std::ops::Range;

use syntax::language::SyntaxNode;

use crate::{
    ast::ASTResult,
    errors::ASTError,
    expression::Expr,
    lang_elems::{filter_irrelevant_out, get_kind_on_node},
};

#[derive(Clone, Debug, PartialEq)]
pub struct Indexing {
    pub indexed: SyntaxNode,
    pub index: SyntaxNode,
    pub span: Range<usize>,
}

impl Indexing {
    pub fn indexed(&self) -> ASTResult<Expr> {
        Expr::try_from(&self.indexed)
    }
    pub fn index(&self) -> ASTResult<Expr> {
        Expr::try_from(&self.index)
    }
    pub fn span(&self) -> Range<usize> {
        self.span.clone()
    }
}

impl TryFrom<&SyntaxNode> for Indexing {
    type Error = ASTError;

    fn try_from(node: &SyntaxNode) -> Result<Self, Self::Error> {
        let mut nodes = filter_irrelevant_out(node.children(), get_kind_on_node);
        let span = node.text_range().into();

        if nodes.len() != 2 {
            return Err(ASTError::new(
                node.text_range().into(),
                "two nodes: expression to be indexed and the indexing expression",
                node,
            ));
        }
        let index = nodes.pop().unwrap();
        let indexed = nodes.pop().unwrap();
        Ok(Self {
            indexed,
            index,
            span,
        })
    }
}

#[cfg(test)]
mod tests {

    use super::Indexing;
    use crate::{
        ast_root_from, cast_node_into_type,
        expression::Expr,
        literal::{Literal, Value},
        operation::Binary,
    };
    use parameterized_test::create;

    create! {
        happy_path_indexing_test,
        (program, exp_indexed_text, exp_index_text), {
            let ast_root = ast_root_from(program);
            let indexing_node = ast_root.get_root().first_child().unwrap();
            let indexing = cast_node_into_type::<Indexing>(&indexing_node);
            assert_eq!(exp_indexed_text, indexing.indexed.text());
            assert_eq!(exp_index_text, indexing.index.text());
        }
    }

    happy_path_indexing_test! {
        double_infixed_index: (
         "arr[me.z - she.z]", "arr", "me.z - she.z",
        ),
        nested_index: (
         "matrix[matrix[0].len() -1]", "matrix", "matrix[0].len() -1",
        ),
        nested_and_two_dim_index: (
         "matrix[matrix[0].len() -1][0]", "matrix[matrix[0].len() -1]", "0",
        ),
        index_buffer_literal: (
         "[[1,0,0],[0,1,0],[0,0,1]][0][0][0]", "[[1,0,0],[0,1,0],[0,0,1]][0][0]", "0",
        ),
    }

    #[test]
    fn detailed_testing_of_complex_call() {
        let program = "matrix[matrix[0].len() -1][0]";
        let ast_root = ast_root_from(program);
        let indexing_node = ast_root.get_root().first_child().unwrap();
        let indexing: Indexing = cast_node_into_type::<Indexing>(&indexing_node);
        assert!(matches!(
            indexing.index().unwrap(),
            Expr::Literal(Literal(Value::Int(0)))
        ));
        if let Expr::Indexing(nested_indexing) = indexing.indexed().expect("indexed to be valid") {
            assert!(matches!(
                nested_indexing
                    .index()
                    .expect("nested index to have a valid index"),
                Expr::Infix(Binary::Infix(_))
            ));
            assert!(matches!(
                nested_indexing
                    .indexed()
                    .expect("nested index to have a valid indexed"),
                Expr::VarRef(var_ref) if var_ref.name == "matrix"
            ));
        } else {
            unreachable!()
        }
    }
    #[test]
    fn happy_path_for_indexing() {
        let program = "random3D()[0][0][0]";
        let ast_root = ast_root_from(program);
        let node = ast_root.get_root().first_child().unwrap();
        let indexing = cast_node_into_type::<Indexing>(&node);

        assert!(matches!(
            indexing.index().expect("expects index"),
            Expr::Literal(Literal(Value::Int(0)))
        ));
        assert!(
            matches!(indexing.indexed().unwrap(), Expr::Indexing(indexing) 
                if indexing.index().expect("expects second index") == Expr::Literal(Literal(Value::Int(0))) 
                    && matches!(indexing.indexed().expect("expects second indexed"), Expr::Indexing(_)))
        );
    }
}
