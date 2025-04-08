use thin_vec::ThinVec;

use ast::container_ref::ContainerRef as ASTContainerRef;

use crate::{
    HIRResult,
    builder::HIRBuilder,
    delimited::Indexing,
    resolution::{Baggage, Reference, ResolutionType, Unresolved, resolve},
    scope::{Span, VarDefIdx, VarSelector},
    variable::VarDef,
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
        let span: Span = container_ref.span();

        Ok(Unresolved::baggaged(
            name,
            Baggage::Index(lowered_indices),
            ResolutionType::Container(span),
        ))
    }
    pub fn resolve_container_ref(
        &self,
        unresolved: &Reference<VarDef>,
    ) -> HIRResult<Reference<VarDef>> {
        let current_scope_idx = self.current_scope_cursor;
        let (at, idx) =
            resolve::<VarDef, VarSelector>(current_scope_idx, &self.scopes, unresolved)?;
        Ok(Reference::Resolved { at, idx })
    }
}

#[cfg(test)]
mod tests {}
