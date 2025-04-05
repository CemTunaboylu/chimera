use smol_str::SmolStr;
use thin_vec::ThinVec;

use ast::container_ref::ContainerRef as ASTContainerRef;

use crate::{HIRResult, delimited::Indexing, hir::HIRBuilder};

#[derive(Clone, Debug, PartialEq)]
pub struct ContainerRef {
    pub name: SmolStr,
    pub indices: ThinVec<Indexing>,
}

impl HIRBuilder {
    pub fn lower_container_ref(
        &mut self,
        container_ref: &ASTContainerRef,
    ) -> HIRResult<ContainerRef> {
        let name = container_ref.name();
        let indices = container_ref.indices();

        let mut lowered_indices = ThinVec::with_capacity(indices.len());
        for idx in indices {
            let low_indexing = self.lower_indexing(idx)?;
            lowered_indices.push(low_indexing);
        }

        Ok(ContainerRef {
            name,
            indices: lowered_indices,
        })
    }
}

#[cfg(test)]
mod tests {}
