use ast::{
    expression::Expr as ASTExpr,
    types::Hint as ASTHint,
    variable::{VarDef as ASTVarDef, VarRef as ASTVarRef},
};

use hir_macro::with_context;
use la_arena::Idx;
use smol_str::SmolStr;

use crate::{
    HIRResult,
    builder::HIRBuilder,
    climbing::climb,
    context::UsageContext,
    metadata::{Usage, Usages, VarMeta},
    resolution::{Reference, ResolutionType, Unresolved, resolve},
    scope::{ExprIdx, NameIndexed, Span, StrIdx, VarDefIdx, VarSelector, placeholder_idx},
};
#[derive(Clone, Debug, PartialEq)]
pub struct VarDef {
    pub name_index: StrIdx,
    pub expr_index: ExprIdx,
}
impl Default for VarDef {
    fn default() -> Self {
        Self {
            name_index: placeholder_idx(),
            expr_index: placeholder_idx(),
        }
    }
}

impl NameIndexed for VarDef {
    fn set_name_index(&mut self, ix: StrIdx) {
        self.name_index = ix;
    }
    fn get_name_index(&self) -> StrIdx {
        self.name_index
    }
}

impl HIRBuilder {
    #[with_context(UsageContext::Moved)]
    pub fn lower_var_def_rhs(&mut self, expr: Option<&ASTExpr>) -> HIRResult<ExprIdx> {
        self.try_lower_expr_as_idx_with_default(expr)
    }
    fn store_metadata_for_var_def(
        &mut self,
        ast_var_def: &ASTVarDef,
        idx: Idx<VarDef>,
        expr_idx: ExprIdx,
    ) -> HIRResult<()> {
        let span = ast_var_def.span();
        self.allocate_span(ast_var_def.name(), span.clone());
        let usage_for_def = {
            let l_ctx = self.get_context().expect("a context");
            Usage::from(l_ctx, span, self.get_current_stmt_idx())
        };
        let of_type = if let Some(ASTHint::Type(type_hint)) = ast_var_def.type_hint() {
            self.lower_type(type_hint)?;
        } else {
            todo!()
        };
        let scope = self.get_current_scope_mut();
        scope.metadata.vars.insert(
            idx,
            VarMeta {
                def: usage_for_def,
                usages: Usages::new(),
                is_mut: ast_var_def.is_mut(),
                first_read_idx: None,
                first_write_idx: None,
                of_type: todo!(), // should be able to infer this?
            },
        );
    }
    #[with_context(UsageContext::Init)]
    pub fn lower_var_def(&mut self, ast_var_def: &ASTVarDef) -> HIRResult<VarDefIdx> {
        let name = ast_var_def.name();
        let expr_index = self.lower_var_def_rhs(ast_var_def.value())?;

        let var_def = VarDef {
            name_index: placeholder_idx::<SmolStr>(),
            expr_index,
        };

        let idx = self.allocate::<VarDef, VarSelector>(name.clone(), var_def)?;
        self.store_metadata_for_var_def(ast_var_def, idx, expr_index)?;
        Ok(idx)
    }
    pub fn lower_var_ref(&mut self, var_ref: &ASTVarRef) -> HIRResult<Unresolved> {
        let name = var_ref.name().clone();
        let span: Span = var_ref.span();
        let low_var_ref = Unresolved::new(name, ResolutionType::Var(span));
        Ok(low_var_ref)
    }
    pub fn resolve_var_ref(
        &mut self,
        unresolved: &Reference<VarDef>,
    ) -> HIRResult<Reference<VarDef>> {
        let scope_climbing_iter = climb(self.current_scope_cursor, &self.scopes);
        let resolved_reference = resolve::<VarDef, VarSelector>(scope_climbing_iter, unresolved)?;

        let scope = self.get_current_scope_mut();
        let usage = todo!();
        if let Some(meta) = scope
            .metadata
            .vars
            .get_mut(&resolved_reference.get_obj_index()?)
        {
            meta.usages.push(usage);
            if meta.first_read_idx.is_none() {
                meta.first_read_idx = Some(self.get_current_stmt_idx());
            }
        }

        Ok(resolved_reference)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarRef(pub(crate) VarDefIdx);

#[cfg(test)]
mod tests {}
