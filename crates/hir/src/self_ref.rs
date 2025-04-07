use crate::{HIRResult, builder::HIRBuilder};

use ast::self_ref::SelfRef as ASTSelfRef;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum SelfRef {
    Instance,
    Struct,
}

impl HIRBuilder {
    pub fn lower_self_ref(&self, self_ref: &ASTSelfRef) -> HIRResult<SelfRef> {
        let sr = match self_ref {
            ASTSelfRef::Instance => SelfRef::Instance,
            ASTSelfRef::Struct => SelfRef::Struct,
        };
        Ok(sr)
    }
}

#[cfg(test)]
mod tests {}
