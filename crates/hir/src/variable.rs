use hir_macro::with_context;
use la_arena::Idx;
use smol_str::SmolStr;

use crate::{
    HIRResult,
    builder::HIRBuilder,
    climbing::climb,
    context::UsageContext,
    resolution::{Reference, ResolutionType, Unresolved, resolve},
    scope::{ExprIdx, NameIndexed, Span, StrIdx, VarDefIdx, VarSelector, placeholder_idx},
};
#[derive(Clone, Debug, PartialEq)]
pub struct VarUse {
    pub kind: UsageContext,
    pub span: Span,
    pub statement_idx: Option<usize>, // or ExprIdx
}
#[derive(Clone, Debug, PartialEq)]
pub struct VarDef {
    pub name_index: StrIdx,
    pub expr_index: ExprIdx,
}

impl NameIndexed for VarDef {
    fn set_name_index(&mut self, ix: Idx<SmolStr>) {
        self.name_index = ix;
    }
}

impl HIRBuilder {
    #[with_context(UsageContext::Moved)]
    pub fn lower_var_def_rhs(&mut self, expr: Option<&ASTExpr>) -> HIRResult<ExprIdx> {
        self.try_lower_expr_as_idx_with_default(expr)
    }
    #[with_context(UsageContext::Init)]
    pub fn lower_var_def(&mut self, ast_var_def: &ASTVarDef) -> HIRResult<VarDefIdx> {
        let name = ast_var_def.name();
        let expr_index = self.lower_var_def_rhs(ast_var_def.value())?;

        let var_def = VarDef {
            name_index: placeholder_idx::<SmolStr>(),
            expr_index,
        };

        self.allocate::<VarDef, VarSelector>(name.clone(), var_def)
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
        let (at, idx) = resolve::<VarDef, VarSelector>(scope_climbing_iter, unresolved)?;
        Ok(Reference::Resolved { at, idx })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarRef(VarDefIdx);

#[cfg(test)]
mod tests {}
