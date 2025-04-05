use smol_str::SmolStr;

use crate::{HIRResult, hir::HIRBuilder, types::Type};
use ast::parameter::{By as ASTBy, Param as ASTParam};

#[derive(Clone, Debug, PartialEq)]
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

#[derive(Clone, Debug, PartialEq)]
pub enum Param {
    Named(SmolStr, By, Type),
    SelfRef(By),
}

impl HIRBuilder {
    pub fn lower_parameter(&mut self, param: &ASTParam) -> HIRResult<Param> {
        match param {
            ASTParam::Named(smol_str, param_type) => {
                let lowered_type = self.lower_type(&param_type.1)?;
                let by = By::from(&param_type.0);
                Ok(Param::Named(smol_str.clone(), by, lowered_type))
            }
            ASTParam::SelfRef(by) => {
                let by = By::from(by);
                Ok(Param::SelfRef(by))
            }
        }
    }
}

#[cfg(test)]
mod tests {}
