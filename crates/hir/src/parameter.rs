use smol_str::SmolStr;

use crate::{
    HIRResult, builder::HIRBuilder, context::UsageContext, typing::hindley_milner::types::Type,
};
use ast::parameter::{By as ASTBy, Param as ASTParam, ParamType as ASTParamType};

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub enum By {
    Ref,
    RefMut,
    ValueMut,
    Value,
}

impl From<&ASTBy> for By {
    fn from(value: &ASTBy) -> Self {
        match value {
            ASTBy::Ref => By::Ref,
            ASTBy::RefMut => By::RefMut,
            ASTBy::ValueMut => By::ValueMut,
            ASTBy::Value => By::Value,
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub enum Param {
    Generic(SmolStr), // note: as of now, we don't allow ref or mut, thus only the variable ref.
    Named(SmolStr, By, Type),
    SelfRef(By),
}

impl HIRBuilder {
    pub fn lower_type_with_ctx(
        &mut self,
        param_type: &ASTParamType,
        ctx: UsageContext,
    ) -> HIRResult<Type> {
        self.push_usage_context(ctx);
        let t = self.lower_type(&param_type.1);
        self.pop_usage_context();
        t
    }
    pub fn lower_parameter(&mut self, param: &ASTParam) -> HIRResult<Param> {
        let p = match param {
            ASTParam::Named(smol_str, param_type) => {
                let by = By::from(&param_type.0);

                let ctx = match by {
                    By::Ref => UsageContext::Ref,
                    By::RefMut => UsageContext::RefMut,
                    // note: still a move but declared as mutable
                    By::ValueMut => UsageContext::Mut,
                    By::Value => UsageContext::Moved,
                };

                Param::Named(
                    smol_str.clone(),
                    by,
                    self.lower_type_with_ctx(param_type, ctx)?,
                )
            }
            ASTParam::SelfRef(by) => {
                let by = By::from(by);
                Param::SelfRef(by)
            }
            ASTParam::Generic(smol_str) => {
                // note: since we don't allow ref or mut for now, we have to move the value in
                // let ctx = UsageContext::Moved;
                Param::Generic(smol_str.clone())
            }
        };
        Ok(p)
    }
}

#[cfg(test)]
mod tests {}
