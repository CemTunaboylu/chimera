use ast::variable::{VarDef as ASTVarDef, VarRef as ASTVarRef};

use crate::{
    HIRResult,
    builder::HIRBuilder,
    resolution::{ResolutionType, Unresolved},
    scope::{ExprIdx, StrIdx, VarDefIdx},
};

#[derive(Clone, Debug, PartialEq)]
pub struct VarDef {
    pub name_index: StrIdx,
    pub expr_index: ExprIdx,
}

impl HIRBuilder {
    pub fn lower_var_def(&mut self, var_def: &ASTVarDef) -> HIRResult<VarDefIdx> {
        let name = var_def.name();
        let current_scope = self.get_current_scope_mut();
        let name_index = current_scope.var_names.alloc(name.clone());

        let expr_index = self.try_lower_expr_as_idx_with_default(var_def.value())?;

        let var_def = VarDef {
            name_index,
            expr_index,
        };
        let idx = self.allocate_var_def_with_name(name, var_def);
        Ok(idx)
    }
    pub fn lower_var_ref(&mut self, var_ref: &ASTVarRef) -> HIRResult<Unresolved> {
        let name = var_ref.name().clone();
        let low_var_ref = Unresolved::new(name, ResolutionType::Var);
        Ok(low_var_ref)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarRef(VarDefIdx);

#[cfg(test)]
mod tests {}
