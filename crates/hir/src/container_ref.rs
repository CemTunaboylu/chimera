use hir_macro::with_context;
use thin_vec::ThinVec;

use ast::container_ref::ContainerRef as ASTContainerRef;

use crate::{
    HIRResult,
    builder::HIRBuilder,
    climbing::climb,
    context::UsageContext,
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
    #[with_context(UsageContext::Read)]
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
        let scope_climbing_iter = climb(self.current_scope_cursor, &self.scopes);
        let (at, idx) = resolve::<VarDef, VarSelector>(scope_climbing_iter, unresolved)?;
        Ok(Reference::Resolved { at, idx })
    }
}

#[cfg(test)]
mod tests {}
