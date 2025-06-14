use ast::{
    expression::Expr as ASTExpr,
    let_binding::{
        Identifier as ASTIdentifier, LetBinding as ASTLetBinding, Pattern as ASTPattern,
        VarRef as ASTVarRef,
    },
};

use hir_macro::with_context;
use la_arena::Idx;
use smol_str::{SmolStr, ToSmolStr};
use thin_vec::{ThinVec, thin_vec};

use crate::{
    HIRResult, Spanned,
    builder::HIRBuilder,
    climbing::climb,
    context::UsageContext,
    expression::Expr,
    indexing::Indexing,
    literal::{Literal, Value},
    metadata::{Usage, Usages, VarMeta},
    resolution::{Baggage, Reference, ResolutionType, Unresolved, resolve},
    scope::{ExprIdx, LetBindingIdx, NameIndexed, Span, StrIdx, VarSelector},
    typing::hindley_milner::types::Type,
};

#[derive(Clone, Debug, Default, Eq, Hash, PartialEq, PartialOrd)]
pub struct Identifier {
    pub is_mut: bool,
    pub type_hint: Option<Type>,
    pub spanned_name_idx: Spanned<StrIdx>,
}

impl Identifier {
    fn new(is_mut: bool, type_hint: Option<Type>, spanned_name_idx: Spanned<StrIdx>) -> Self {
        Self {
            is_mut,
            type_hint,
            spanned_name_idx,
        }
    }
}

impl NameIndexed for Identifier {
    fn set_name_index(&mut self, ix: StrIdx) {
        self.spanned_name_idx.index = ix;
    }
    fn get_name_index(&self) -> StrIdx {
        self.spanned_name_idx.index
    }
}

#[derive(Clone, Debug, Default, Eq, Hash, PartialEq, PartialOrd)]
pub struct LetBinding {
    pub identifier: Identifier,
    pub spanned_expr_idx: Spanned<ExprIdx>,
    pub span: Span,
}

impl NameIndexed for LetBinding {
    fn set_name_index(&mut self, ix: StrIdx) {
        self.identifier.set_name_index(ix);
    }
    fn get_name_index(&self) -> StrIdx {
        self.identifier.get_name_index()
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub struct VarRef(pub(crate) LetBindingIdx);

#[derive(Clone, Debug)]
struct NamedIdentifier {
    name: SmolStr,
    identifier: Identifier,
}
struct BindingLoweringPhase {
    ix: isize,
    patterns: ThinVec<ASTPattern>,
    span: Span,
    tmps_expr_idx: ExprIdx,
}
impl BindingLoweringPhase {
    fn with(with: ExprIdx, tuple_pattern: &ASTPattern) -> Self {
        assert!(matches!(tuple_pattern, ASTPattern::Tuple(_)));
        let patterns = tuple_pattern.as_reversed_tuple_pattern().unwrap();
        Self {
            ix: 0,
            patterns,
            span: tuple_pattern.span().into(),
            tmps_expr_idx: with,
        }
    }
    fn wrap_current_tmp_expr_idx_with_span_of(&self, ast_pattern: &ASTPattern) -> Spanned<ExprIdx> {
        let new_span: Span = ast_pattern.span().into();
        Spanned::new(self.tmps_expr_idx, new_span)
    }
}

impl HIRBuilder {
    fn lower_into_named_identifier(
        &mut self,
        ast_identifier: &ASTIdentifier,
    ) -> HIRResult<NamedIdentifier> {
        let span: Span = ast_identifier.span().into();
        let is_mut = ast_identifier.is_mut();
        let mut type_hint = None;
        if let Some(ast_type) = ast_identifier.type_hint() {
            let lowered_type = self.lower_type(ast_type)?;
            type_hint = Some(lowered_type);
        }
        let spanned_name_idx: Spanned<Idx<SmolStr>> = Spanned::spanned(span);
        let identifier = Identifier::new(is_mut, type_hint, spanned_name_idx);
        let name = ast_identifier.name();
        Ok(NamedIdentifier {
            name: name.clone(),
            identifier,
        })
    }

    // ! FIXME: depending on expr type, context should change i.e. if & it is not move.
    #[with_context(UsageContext::Moved)]
    pub fn lower_let_binding_rhs(&mut self, expr: &ASTExpr) -> HIRResult<ExprIdx> {
        self.lower_expr_as_idx(expr)
    }
    fn store_metadata_for_let_binding(
        &mut self,
        _let_binding: &LetBinding,
        _idx: Idx<LetBinding>,
    ) -> HIRResult<()> {
        // let span = ast_let_binding.span();
        // self.allocate_span(ast_let_binding.name(), span.clone().into());
        // let usage_for_def = {
        //     let l_ctx = self.get_context().expect("a context");
        //     Usage::from(l_ctx, span.into(), self.get_current_stmt_idx())
        // };
        // let of_type = if let Some(ASTHint::Type(type_hint)) = ast_let_binding.type_hint() {
        //     self.lower_type(type_hint)?;
        // } else {
        // };
        // let scope = self.get_current_scope_mut();
        // scope.metadata.vars.insert(
        //     idx,
        //     VarMeta {
        //         def: usage_for_def,
        //         usages: Usages::new(),
        //         is_mut: ast_let_binding.is_mut(),
        //         first_read_idx: None,
        //         first_write_idx: None,
        //         of_type: todo!(), // should be able to infer this?
        //     },
        // );
        todo!()
    }

    fn temporary_binding_name(span: impl Into<Span>) -> SmolStr {
        let span: Span = span.into();
        format!("__tmp_{:?}", span).to_smolstr()
    }

    fn int_literal_of(i: i32) -> Expr {
        Expr::Literal(Literal(Value::Int(i)))
    }

    fn bind_a_tmp_with(&mut self, spanned_expr_idx: Spanned<ExprIdx>) -> HIRResult<ExprIdx> {
        let span = spanned_expr_idx.span;
        let tmp_name = Self::temporary_binding_name(span);
        let identifier = Identifier::new(false, None, Spanned::spanned(span));
        let tmp_binding = LetBinding {
            identifier,
            spanned_expr_idx,
            span,
        };
        let tmp_idx = self.allocate::<LetBinding, VarSelector>(tmp_name, tmp_binding)?;
        let tmp_binding_name_idx =
            self.get_current_scope().variable_allocator.definitions[tmp_idx].get_name_index();
        let tmp_expr = Expr::VarRef(Reference::Resolved {
            at: self.current_scope_cursor,
            baggage: Baggage::None,
            name_idx: tmp_binding_name_idx,
            obj_idx: tmp_idx,
        });
        Ok(self.as_expr_idx(tmp_expr))
    }

    fn form_indexing_for(
        &mut self,
        current: &mut BindingLoweringPhase,
        rhs_span: impl Into<Span>,
        pattern_span: impl Into<Span>,
    ) -> ExprIdx {
        let int_literal = Self::int_literal_of(current.ix as i32);
        let int_literal_idx = self.as_expr_idx(int_literal);
        current.ix += 1;
        let indexing = Indexing {
            // * Note: the referenced expression actually does not change, thus we use the rhs_span
            reference: Spanned::new(current.tmps_expr_idx, rhs_span),
            indices: thin_vec![Spanned::new(
                int_literal_idx,
                // * Note: since this actually does not exist, to make debugging easier and things practical,
                // * we use the span of the current identifier for the index that is corresponding to it
                pattern_span
            )],
            span: current.span,
        };
        let ith_indexing_expr = Expr::Indexing(indexing);
        self.as_expr_idx(ith_indexing_expr)
    }

    #[with_context(UsageContext::Init)]
    pub fn lower_let_binding(
        &mut self,
        ast_let_binding: &ASTLetBinding,
    ) -> HIRResult<ThinVec<LetBindingIdx>> {
        let span: Span = ast_let_binding.span().into();

        let rhs = ast_let_binding.value().unwrap();
        let rhs_span: Span = rhs.span.clone().into();
        let expr_index = self.lower_let_binding_rhs(&rhs.element)?;

        // * No tuples, thus we don't need to destructure anything
        if let ASTPattern::Ident(ast_identifier) = ast_let_binding.pattern() {
            // * Note: if we don't have a tuple pattern, expr to bind with will have its own span
            let spanned_expr_idx = Spanned::new(expr_index, rhs_span);
            let NamedIdentifier { name, identifier } =
                self.lower_into_named_identifier(ast_identifier)?;
            let let_binding = LetBinding {
                identifier,
                spanned_expr_idx,
                span,
            };
            let idx = self.allocate::<LetBinding, VarSelector>(name.clone(), let_binding)?;
            return Ok(thin_vec![idx]);
        }
        // * Note: if we are binding multiple identifiers (say n), we destructure the binding into n+1 separate bindings
        // * where the rhs expression is bound to temporary variable, and all bindings are bound to corresponding Indexing
        // * with that temporary variable. If nested, in each 'recursion', a new temporary variable is created with corres-
        // * ponding Indexing again, and then the pattern identifiers form new Indexings accordingly.
        /*
            Destructuring:
                let ((r,g,b), alpha) = pixel.get_colors();
            as 4 different bindings :
                let __tmp_<span of outer tuple> = pixel.get_colors(); // where we assume the result is indexable
                let __tmp_<span of inner tuple> = __tmp_<span of outer tuple>[0]; // where we assume the result is indexable
                let r = __tmp_<span of inner tuple>[0];
                let g = __tmp_<span of inner tuple>[1];
                let b = __tmp_<span of inner tuple>[2];
                let aplha = __tmp_<span of outer tuple>[1];
        */

        let mut binding_indices = thin_vec![];
        // * Note: since we have a tuple pattern, expr to bind with will have the span of the pattern instead
        let spanned_expr_idx = Spanned::new(expr_index, ast_let_binding.pattern_span());

        let mut current = BindingLoweringPhase::with(
            self.bind_a_tmp_with(spanned_expr_idx)?,
            ast_let_binding.pattern(),
        );

        let mut stack: ThinVec<BindingLoweringPhase> = thin_vec![];

        loop {
            if current.patterns.is_empty() {
                if stack.is_empty() {
                    break;
                }
                current = stack.pop().unwrap();
                // we moved to the next
                current.ix += 1;
                continue;
            }
            let pattern = current.patterns.pop().unwrap();
            // * Note: if it is a tuple pattern, we push the current state into the stack,
            // * with a new tmp that indexes the old tmp i.e. tmp[ix] where ix = current.ix
            if matches!(pattern, ASTPattern::Tuple(_)) {
                let pattern_spanned = current.wrap_current_tmp_expr_idx_with_span_of(&pattern);
                let new_tmp = self.bind_a_tmp_with(pattern_spanned)?;
                stack.push(current);
                current = BindingLoweringPhase::with(new_tmp, &pattern);
                continue;
            }
            // at this point we are sure that pattern is Pattern:Ident
            let pattern_as_ident = pattern.as_ident().unwrap();
            let is_mut = pattern_as_ident.is_mut();
            let type_hint = if let Some(ast_type_hint) = pattern_as_ident.type_hint() {
                Some(self.lower_type(ast_type_hint)?)
            } else {
                None
            };

            let ith_indexing_expr_idx =
                self.form_indexing_for(&mut current, rhs_span, pattern_as_ident.span());

            // TODO: create a NamedIdentifier with the method
            let name_of_the_binding = pattern_as_ident.name();

            let identifier =
                Identifier::new(is_mut, type_hint, Spanned::spanned(pattern_as_ident.span()));
            let binding = LetBinding {
                identifier,
                spanned_expr_idx: Spanned::new(ith_indexing_expr_idx, current.span),
                span: current.span,
            };
            let binding_idx =
                self.allocate::<LetBinding, VarSelector>(name_of_the_binding.clone(), binding)?;

            binding_indices.push(binding_idx);
        }
        Ok(binding_indices)
    }
    pub fn lower_var_ref(&mut self, var_ref: &ASTVarRef) -> HIRResult<Unresolved> {
        let name = var_ref.name().clone();
        let span: Span = var_ref.span().into();
        let low_var_ref = Unresolved::new(name, ResolutionType::Var(span));
        Ok(low_var_ref)
    }
    pub fn resolve_var_ref(
        &mut self,
        unresolved: &Reference<LetBinding>,
    ) -> HIRResult<Reference<LetBinding>> {
        let scope_climbing_iter = climb(self.current_scope_cursor, &self.scopes);
        let resolved_reference =
            resolve::<LetBinding, VarSelector>(scope_climbing_iter, unresolved)?;

        // let scope = self.get_current_scope_mut();
        // let usage = todo!();
        // if let Some(meta) = scope
        //     .metadata
        //     .vars
        //     .get_mut(&resolved_reference.get_obj_index()?)
        // {
        //     meta.usages.push(usage);
        //     if meta.first_read_idx.is_none() {
        //         meta.first_read_idx = Some(self.get_current_stmt_idx());
        //     }
        // }

        Ok(resolved_reference)
    }
}

#[cfg(test)]
mod tests {

    use parameterized_test::create;

    use super::*;
    use crate::{expression::Expr, scope::into_idx};

    use ast::{ast_root_from, cast_node_into_type};

    create! {
        happy_path_let_binding_with_tuple_pattern_test,
        (program), {
            let ast_root = ast_root_from(program);
            let binding_node = ast_root.get_root().first_child().unwrap();
            let ast_let_binding= cast_node_into_type::<ASTLetBinding>(&binding_node);
            let mut hir = HIRBuilder::new(ast_root);

            let let_binding_idx = hir.lower_let_binding(&ast_let_binding).expect("should have been lowered");
            let current_scope = hir.get_current_scope();
            for idx in let_binding_idx {
                let let_binding = &current_scope.variable_allocator.definitions[idx];
                assert!(matches!(&let_binding.identifier, ident));
                let expr_idx = let_binding.spanned_expr_idx.index;
                let expr = hir.get_expr(expr_idx);
                assert!(matches!(expr, Expr::Indexing(Indexing{indices, ..}) if indices.len() == 1));
            }
        }
    }

    happy_path_let_binding_with_tuple_pattern_test! {
        tuple_to_tuple: "let (a,b,c) = (1,2,3);",
        tuple_to_fn_call: "let (r,g,b) = pixel.colors();",
        // single_destruct
    }

    /*
        Destructuring:
            let ((r,g,b), alpha) = pixel.get_colors();
        as 6 different bindings :
            let __tmp_<span of outer tuple> = pixel.get_colors(); // where we assume the result is indexable
            let __tmp_<span of inner tuple> = __tmp_<span of outer tuple>[0]; // where we assume the result is indexable
            let r = __tmp_<span of inner tuple>[0];
            let g = __tmp_<span of inner tuple>[1];
            let b = __tmp_<span of inner tuple>[2];
            let aplha = __tmp_<span of outer tuple>[1];
    */
    #[test]
    fn detailed_testing_of_buffer_literal_indexing() {
        let program = "let ((r,g,b), mut alpha) = pixel.get_colors();";
        let ast_root = ast_root_from(program);
        let indexing_node = ast_root.get_root().first_child().unwrap();
        let ast_let_binding = cast_node_into_type::<ASTLetBinding>(&indexing_node);
        let mut hir = HIRBuilder::new(ast_root);

        let let_binding_idx = hir
            .lower_let_binding(&ast_let_binding)
            .expect("should have been lowered");
        let current_scope = hir.get_current_scope();

        let immutable = false;
        let mutable = true;

        let expected_identifiers = &[(immutable, 2), (immutable, 3), (immutable, 4), (mutable, 5)];
        // * Note: since the destructuring will introduce additional bindings, we check if reference also is a binding
        // * (which should be the case) and if the reference binding is as desired e.g. nested tuples __tmp binds to outer __tmp
        let expected_reference_binding =
            &["__tmp_5..12", "__tmp_5..12", "__tmp_5..12", "__tmp_4..24"];
        let expected_binding_assertions =
            &[("r", 6..7), ("g", 8..9), ("b", 10..11), ("alpha", 18..23)];

        for (i, idx) in let_binding_idx.iter().enumerate() {
            let let_binding = &current_scope.variable_allocator.definitions[*idx];
            let (is_mutable, name_idx) = expected_identifiers[i];
            assert!(
                matches!(
                let_binding.identifier,
                Identifier{ is_mut , type_hint: None, spanned_name_idx: Spanned { index, .. } }
                if index == into_idx(name_idx) && is_mut == is_mutable
                ),
                "identifier did not match {:?} with is_mut: {:?}, index: {:?}",
                let_binding.identifier,
                is_mutable,
                into_idx::<SmolStr>(name_idx)
            );

            let Spanned { index, span } = let_binding.identifier.spanned_name_idx;

            let (exp_name, exp_span) = &expected_binding_assertions[i];
            let retrieved_name = &current_scope.variable_allocator.names[index];
            assert_eq!(exp_name, retrieved_name);
            assert_eq!(Into::<Span>::into(exp_span.clone()), span);

            let expr_idx = let_binding.spanned_expr_idx.index;
            let expr = hir.get_expr(expr_idx);
            assert!(matches!(expr, Expr::Indexing(Indexing{indices, ..}) if indices.len() == 1));

            if let Expr::Indexing(indexing) = expr {
                let reference_idx = indexing.reference.index;
                let reference = hir.get_expr(reference_idx);
                if let Expr::VarRef(Reference::Resolved {
                    at,
                    baggage,
                    name_idx,
                    ..
                }) = reference
                {
                    assert_eq!(hir.current_scope_cursor, *at);
                    assert_eq!(Baggage::None, *baggage);

                    let name = &current_scope.variable_allocator.names[*name_idx];
                    assert!(
                        name.starts_with(expected_reference_binding[i]),
                        "name '{:?}' does not start with '{:?}'",
                        name,
                        expected_reference_binding[i]
                    );
                }
            } else {
                unreachable!()
            }
        }
    }
}
