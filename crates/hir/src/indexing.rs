use std::ops::Range;

use hir_macro::with_context;
use syntax::language::SyntaxNode;
use thin_vec::{ThinVec, thin_vec};

use crate::{
    HIRResult, SpannedIdx,
    builder::HIRBuilder,
    context::UsageContext,
    errors::HIRError,
    expression::Expr,
    scope::{ScopeIdx, Scoped, ScopedExprIdx, Span},
};

use ast::{expression::Expr as ASTExpr, indexing::Indexing as ASTIndexing};

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub struct Indexing {
    pub scope_idx: ScopeIdx,
    pub reference: SpannedIdx<Expr>,
    pub indices: ThinVec<SpannedIdx<Expr>>,
    pub span: Span,
}
impl Indexing {
    pub fn scoped_reference(&self) -> ScopedExprIdx {
        Scoped::new(self.scope_idx, self.reference.index)
    }
}

impl HIRBuilder {
    #[with_context(UsageContext::Read)]
    pub fn lower_indexing(&mut self, ast_indexing: &ASTIndexing) -> HIRResult<Indexing> // HIRResult<Unresolved>
    {
        let span: Span = ast_indexing.span().into();

        let get_span = |node: &SyntaxNode| -> Span {
            let range: Range<usize> = node.text_range().into();
            range.into()
        };

        let mut span_of_indexed = get_span(&ast_indexing.indexed);

        let mut ast_expr_being_indexed = ast_indexing.indexed().map_err(HIRError::from_err)?;
        let mut ast_expr_as_index = ast_indexing.index().map_err(HIRError::from_err)?;

        let mut expr_as_index_idx = self.try_lowering_expr_as_idx(Some(ast_expr_as_index))?;
        let mut indices = thin_vec![SpannedIdx::new(
            // by deduction, we know that they belong to the same scope
            expr_as_index_idx.elm,
            get_span(&ast_indexing.index)
        )];

        loop {
            if let ASTExpr::Indexing(indexing) = ast_expr_being_indexed {
                ast_expr_being_indexed = indexing.indexed().map_err(HIRError::from_err)?;
                span_of_indexed = get_span(&indexing.indexed);

                ast_expr_as_index = indexing.index().map_err(HIRError::from_err)?;
                expr_as_index_idx = self.lower_expr_as_idx(&ast_expr_as_index)?;
                indices.push(SpannedIdx::new(
                    // by deduction, we know that they belong to the same scope
                    expr_as_index_idx.elm,
                    get_span(&indexing.index),
                ));
                continue;
            }
            break;
        }

        // indices.push(Spanned::new(expr_as_index_idx, span_of_indexed));
        indices.reverse();

        // ! reference is not resolved yet
        let reference_idx = self.lower_expr_as_idx(&ast_expr_being_indexed)?;
        let reference = SpannedIdx::new(reference_idx.elm, span_of_indexed);
        // Ok(Unresolved::baggaged(
        //     name,
        //     Baggage::Index(lowered_indices),
        //     ResolutionType::Container(span),
        // ))
        let scope_idx = self.get_current_scope_idx();
        Ok(Indexing {
            scope_idx,
            reference,
            indices,
            span,
        })
    }
}

#[cfg(test)]
mod tests {
    use parameterized_test::create;
    use smol_str::ToSmolStr;

    use super::*;
    use crate::{
        collection::{shape::Shape, storage::Storage},
        expression::Expr,
        literal::{LazyCollection, Literal, Value},
        resolution::{Reference, Unresolved},
        scope::into_idx,
        typing::hindley_milner::types::{Maybe, Type},
    };

    use ast::{
        ast_root_from_assert_no_err, cast_node_into_type, indexing::Indexing as ASTIndexing,
    };

    fn unresolved_for_var_ref(name: &str) -> Unresolved {
        let span: Span = (0..name.len()).into();
        Unresolved::new(
            name.to_smolstr(),
            crate::resolution::ResolutionType::Var(span),
        )
    }

    create! {
        happy_path_var_ref_indexing_test,
        (program, unresolved_for_reference, exp_indice_len), {
            let ast_root = ast_root_from_assert_no_err(program);
            let indexing_node = ast_root.get_root().first_child().unwrap();
            let ast_indexing = cast_node_into_type::<ASTIndexing>(&indexing_node);
            let mut hir = HIRBuilder::new(ast_root);

            let indexing = hir.lower_indexing(&ast_indexing).expect("should have been lowered");

            let expr_idx = indexing.reference.index;
            let expr = hir.get_expr(&crate::scope::Scoped::new(indexing.scope_idx, expr_idx));

            match expr {
                Expr::VarRef(Reference::Unresolved(unresolved_idx)) => {
                    let current_scope = hir.get_current_scope();
                    let unresolved = &current_scope.to_resolve[unresolved_idx.clone()];
                    assert_eq!(&unresolved_for_reference, unresolved);
                }
                _ => unreachable!(),
            }

            assert_eq!(exp_indice_len, indexing.indices.len());
        }
    }

    happy_path_var_ref_indexing_test! {
        double_infixed_index: (
         "arr[me.z - she.z]", unresolved_for_var_ref("arr"), 1,
        ),
        nested_index: (
         "matrix[matrix[0].len() -1]", unresolved_for_var_ref("matrix"), 1,
        ),
        nested_and_two_dim_index: (
         "matrix[matrix[0].len() -1][0]", unresolved_for_var_ref("matrix"), 2,
        ),
    }

    #[test]
    fn detailed_testing_of_buffer_literal_indexing() {
        let program = "[ [0],[0],[1] ][0][0]";
        let ast_root = ast_root_from_assert_no_err(program);
        let indexing_node = ast_root.get_root().first_child().unwrap();
        let ast_indexing = cast_node_into_type::<ASTIndexing>(&indexing_node);
        let mut hir = HIRBuilder::new(ast_root);

        let indexing = hir
            .lower_indexing(&ast_indexing)
            .expect("should have been lowered");

        let expr_idx = indexing.reference.index;
        let literal = hir.get_expr(&crate::scope::Scoped::new(indexing.scope_idx, expr_idx));

        assert!(matches!(
            literal,
            Expr::Literal(Literal(Value::LazyInit(LazyCollection::Buffer(_))))
        ));
        assert_eq!(indexing.indices.len(), 2);
        for ix in indexing.indices {
            let idx = ix.index;
            let expr = hir.get_expr(&crate::scope::Scoped::new(indexing.scope_idx, idx));
            assert_eq!(expr, &Expr::Literal(Literal(Value::Int(0))));
        }
    }
}
