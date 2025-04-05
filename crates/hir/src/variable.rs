use smol_str::SmolStr;

use ast::variable::{VarDef as ASTVarDef, VarRef as ASTVarRef};

use crate::{
    HIRResult,
    hir::{ExprIdx, HIRBuilder, StrIdx, UnresolvedVarRefIdx, VarDefIdx},
    resolution::ResolutionResult,
};

#[derive(Clone, Debug, PartialEq)]
pub struct VarDef {
    pub name_index: StrIdx,
    pub expr_index: ExprIdx,
}

impl HIRBuilder {
    pub fn lower_var_def(&mut self, var_def: &ASTVarDef) -> HIRResult<VarDefIdx> {
        let name = var_def.name();
        let name_index = self.var_names.alloc(name.clone());

        let expr_index = self.try_lower_expr_as_idx_with_default(var_def.value())?;

        let var_def = VarDef {
            name_index,
            expr_index,
        };
        let idx = self.allocate_var_def_with_name(name, var_def);
        Ok(idx)
    }
    pub fn lower_var_ref(&mut self, var_ref: &ASTVarRef) -> HIRResult<UnresolvedVarRefIdx> {
        let name = var_ref.name().clone();
        let low_var_ref = UnresolvedVarRef(name);
        let idx = self.vars_to_resolve.alloc(low_var_ref);
        Ok(idx)
    }
    pub fn resolve_var_ref(&mut self, var_ref: UnresolvedVarRef) -> ResolutionResult<VarRef> {
        let name = var_ref.0;
        let idx = Self::resolve_with_err(&self.var_name_to_idx_trie, &name)?;
        Ok(VarRef(idx))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct UnresolvedVarRef(SmolStr);

#[derive(Clone, Debug, PartialEq)]
pub struct VarRef(VarDefIdx);

#[cfg(test)]
mod tests {}
