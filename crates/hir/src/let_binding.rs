use ast::{
    expression::Expr as ASTExpr,
    let_binding::{
        Identifier as ASTIdentifier, LetBinding as ASTLetBinding, Pattern as ASTPattern,
        VarRef as ASTVarRef,
    },
    types::Hint as ASTHint,
};

use hir_macro::with_context;
use la_arena::Idx;
use smol_str::{SmolStr, ToSmolStr};
use thin_vec::{ThinVec, thin_vec};

use crate::{
    HIRResult,
    builder::HIRBuilder,
    climbing::climb,
    context::UsageContext,
    delimited::Indexing,
    errors::HIRError,
    expression::Expr,
    metadata::{Usage, Usages, VarMeta},
    mut_clone_with_err,
    resolution::{Reference, ResolutionType, Unresolved, resolve},
    scope::{ExprIdx, LetBindingIdx, NameIndexed, Span, StrIdx, VarSelector, placeholder_idx},
    typing::hindley_milner::types::Type,
};

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd)]
pub struct Spanned<I> {
    pub index: I,
    pub span: Span,
}

impl<I> Default for Spanned<Idx<I>> {
    fn default() -> Self {
        Self {
            index: placeholder_idx(),
            span: Span { start: 0, end: 0 },
        }
    }
}

impl<I> Spanned<Idx<I>> {
    fn new(index: Idx<I>, span: Span) -> Self {
        Self { span, index }
    }
    fn spanned(span: Span) -> Self {
        Self {
            span,
            ..Default::default()
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd)]
pub struct Identifier {
    pub is_mut: bool,
    pub type_hint: Option<Type>,
    pub spanned_name_idx: Spanned<StrIdx>,
}
impl Default for Identifier {
    fn default() -> Self {
        Self {
            is_mut: false,
            type_hint: None,
            spanned_name_idx: Default::default(),
        }
    }
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

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd)]
pub enum Pattern {
    Ident(Identifier),
    Tuple(ThinVec<Pattern>),
}

impl Pattern {
    pub fn as_ident(&self) -> Option<&Identifier> {
        match self {
            Pattern::Ident(identifier) => Some(identifier),
            Pattern::Tuple(_) => None,
        }
    }
    fn get_identifier_name(&self) -> HIRResult<StrIdx> {
        if let Self::Ident(identifier) = self {
            return Ok(identifier.get_name_index());
        }
        Err(HIRError::with_msg("cannot set identifier name of a tuple pattern").into())
    }
    fn set_identifier_name(&mut self, to: StrIdx) -> HIRResult<()> {
        if let Self::Ident(identifier) = self {
            identifier.set_name_index(to);
            return Ok(());
        }
        Err(HIRError::with_msg("cannot set identifier name of a tuple pattern").into())
    }
}

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd)]
pub struct LetBinding {
    pub pattern: Pattern,
    pub spanned_expr_idx: Spanned<ExprIdx>,
    pub span: Span,
}

impl Default for LetBinding {
    fn default() -> Self {
        Self {
            pattern: Pattern::Ident(Default::default()),
            spanned_expr_idx: Default::default(),
            span: Span { start: 0, end: 0 },
        }
    }
}

fn panic_if_err<T>(result: HIRResult<T>) -> T {
    match result {
        Ok(inner) => inner,
        Err(e) => panic!("{:?}", e),
    }
}

impl NameIndexed for LetBinding {
    fn set_name_index(&mut self, ix: StrIdx) {
        panic_if_err(self.pattern.set_identifier_name(ix));
    }
    fn get_name_index(&self) -> StrIdx {
        panic_if_err(self.pattern.get_identifier_name())
    }
}

#[derive(Clone, Debug)]
struct NamedIdentifier {
    name: SmolStr,
    identifier: Identifier,
}

impl HIRBuilder {
    fn lower_into_identifiers(
        &mut self,
        pattern: &ASTPattern,
    ) -> HIRResult<ThinVec<NamedIdentifier>> {
        if let ASTPattern::Tuple(identifiers) = pattern {
            let to_identifiers =
                |p: &ASTPattern, hir: &mut HIRBuilder| hir.lower_into_identifiers(p);

            let lowered_identifiers = mut_clone_with_err(identifiers, self, to_identifiers)?;
            return Ok(lowered_identifiers
                .iter()
                .cloned()
                .flatten()
                .collect::<ThinVec<_>>());
        }
        let pattern_identifier = if let Some(pat) = pattern.as_ident() {
            pat
        } else {
            panic!("An AST Pattern that is not a tuple pattern must be a identifier pattern");
        };
        let span: Span = pattern_identifier.span().into();
        let is_mut = pattern_identifier.is_mut();
        let mut type_hint = None;
        if let Some(ast_type) = pattern.type_hint() {
            let lowered_type = self.lower_type(ast_type)?;
            type_hint = Some(lowered_type);
        }
        let spanned_name_idx: Spanned<Idx<SmolStr>> = Spanned::spanned(span);
        let identifier = Identifier::new(is_mut, type_hint, spanned_name_idx);
        let name = pattern_identifier.name();
        Ok(thin_vec![NamedIdentifier {
            name: name.clone(),
            identifier
        }])
    }

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

    fn temporary_binding_name(span: Span) -> SmolStr {
        format!("__tmp_{:?}", span).to_smolstr()
    }

    fn bind(
        &mut self,
        flat_identifiers: &[NamedIdentifier],
        flat_spanned_expr_idx: &[Spanned<ExprIdx>],
        span: Span,
    ) -> HIRResult<ThinVec<LetBindingIdx>> {
        let mut indices = ThinVec::with_capacity(flat_identifiers.len());
        if flat_identifiers.len() != flat_spanned_expr_idx.len() {
            return Err(HIRError::with_msg(format!(
                "identifiers to bind to must have the same length as expressions, got: identifiers of length {}, expressions of length {}",
                flat_identifiers.len(),
                flat_spanned_expr_idx.len(),
            )).into());
        }
        for (i, (named_identifier, spanned_expr_idx)) in flat_identifiers
            .iter()
            .zip(flat_spanned_expr_idx.iter())
            .enumerate()
        {
            let NamedIdentifier { name, identifier } = named_identifier;
            let pattern = Pattern::Ident(identifier.clone());
            let let_binding = LetBinding {
                pattern,
                spanned_expr_idx: spanned_expr_idx.clone(),
                span: span.clone(),
            };
            let idx = self.allocate::<LetBinding, VarSelector>(name.clone(), let_binding)?;
            // ! handle metadata within alloc
            // self.store_metadata_for_let_binding(&let_binding, idx)?;
            indices[i] = idx;
        }
        Ok(indices)
    }

    #[with_context(UsageContext::Init)]
    pub fn lower_let_binding(
        &mut self,
        ast_let_binding: &ASTLetBinding,
    ) -> HIRResult<ThinVec<LetBindingIdx>> {
        let flat_named_identifiers = self.lower_into_identifiers(ast_let_binding.pattern())?;
        let span: Span = ast_let_binding.span().into();

        let rhs = ast_let_binding.value().unwrap();
        let rhs_span: Span = rhs.span.clone().into();
        let expr_index = self.lower_let_binding_rhs(&rhs.element)?;
        let spanned_expr_idx = Spanned::new(expr_index, rhs_span);
        let mut flat_spanned_expr_indices = thin_vec![];
        // * note: if we are binding multiple identifiers (say n), we destructure it into n+1 separate bindings
        // * where the expression is bound temporarily, and all bindings are bound to its corresponding index
        if flat_named_identifiers.len() > 1 {
            let tmp_name = Self::temporary_binding_name(span);
            let pattern = Pattern::Ident(Identifier::new(false, None, Spanned::spanned(rhs_span)));
            let tmp_binding = LetBinding {
                pattern,
                spanned_expr_idx,
                span,
            };
            let tmp_idx = self.allocate::<LetBinding, VarSelector>(tmp_name, tmp_binding)?;
            for i in 0..flat_named_identifiers.len() {
                // ! FIXME: Indexing must be an op
                // ! as follows: tmp[i] -> InfixBinOp lhs:tmp op:[ rhs:i  ignored:]
                let ith_indexing_expr = Expr::Indexing(Indexing(expr_index));
                flat_spanned_expr_indices
                    .push(Spanned::new(self.as_expr_idx(ith_indexing_expr), rhs_span));
            }
        } else {
            flat_spanned_expr_indices.push(spanned_expr_idx);
        }
        self.bind(
            flat_named_identifiers.as_slice(),
            flat_spanned_expr_indices.as_slice(),
            span,
        )
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

#[derive(Clone, Debug, PartialEq)]
pub struct VarRef(pub(crate) LetBindingIdx);

#[cfg(test)]
mod tests {}
