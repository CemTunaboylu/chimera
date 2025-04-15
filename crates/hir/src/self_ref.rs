use crate::{HIRResult, builder::HIRBuilder, context::UsageContext};

use ast::self_ref::SelfRef as ASTSelfRef;
use hir_macro::with_context;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum SelfRef {
    Instance,
    Struct,
}

impl HIRBuilder {
    #[with_context(UsageContext::Mut)]
    pub fn lower_self_ref(&mut self, self_ref: &ASTSelfRef) -> HIRResult<SelfRef> {
        let sr = match self_ref {
            ASTSelfRef::Instance => SelfRef::Instance,
            ASTSelfRef::Struct => SelfRef::Struct,
        };
        Ok(sr)
    }
}

#[cfg(test)]
mod tests {}
