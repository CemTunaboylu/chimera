use la_arena::Arena;

use crate::scope::{Scope, ScopeIdx};

pub type EnumeratedScope<'c> = (ScopeIdx, &'c Scope);

pub fn climb(current: ScopeIdx, scopes: &Arena<Scope>) -> impl Iterator<Item = EnumeratedScope> {
    ScopeClimber {
        current: Some(current),
        scopes,
    }
}

pub struct ScopeClimber<'hir> {
    current: Option<ScopeIdx>,
    scopes: &'hir Arena<Scope>,
}

impl<'hir> Iterator for ScopeClimber<'hir> {
    type Item = EnumeratedScope<'hir>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(current) = self.current {
            let scope = &self.scopes[current];
            let parent = scope.parent;
            self.current = if parent == current {
                None
            } else {
                Some(parent)
            };
            Some((current, scope))
        } else {
            None
        }
    }
}
