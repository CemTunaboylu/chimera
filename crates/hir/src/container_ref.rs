use thin_vec::ThinVec;

use ast::container_ref::ContainerRef as ASTContainerRef;

use crate::{
    HIRResult,
    builder::HIRBuilder,
    delimited::Indexing,
    resolution::{Baggage, ResolutionType, Unresolved},
    scope::VarDefIdx,
};

#[derive(Clone, Debug, PartialEq)]
pub struct ContainerRef {
    pub name: VarDefIdx,
    pub indices: ThinVec<Indexing>,
}

impl HIRBuilder {
    pub fn lower_container_ref(
        &mut self,
        container_ref: &ASTContainerRef,
    ) -> HIRResult<Unresolved> {
        let name = container_ref.name();
        let indices = container_ref.indices();

        let mut lowered_indices = ThinVec::with_capacity(indices.len());
        for idx in indices {
            let low_indexing = self.lower_indexing(idx)?;
            lowered_indices.push(low_indexing);
        }

        Ok(Unresolved::baggaged(
            name,
            Baggage::Index(lowered_indices),
            ResolutionType::Container,
        ))
    }
}

#[cfg(test)]
mod tests {}
