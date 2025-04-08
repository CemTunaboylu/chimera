use la_arena::Arena;

use crate::scope::{Scope, ScopeIdx};

pub fn climb(current: ScopeIdx, scopes: &Arena<Scope>) -> impl Iterator<Item = &Scope> {
    ScopeClimber { current, scopes }
}

pub struct ScopeClimber<'hir> {
    current: ScopeIdx,
    scopes: &'hir Arena<Scope>,
}

impl<'hir> Iterator for ScopeClimber<'hir> {
    type Item = &'hir Scope;

    fn next(&mut self) -> Option<Self::Item> {
        let scope = &self.scopes[self.current];
        let parent = scope.parent;
        if parent == self.current {
            return None;
        } else {
            self.current = parent;
            Some(&self.scopes[self.current])
        }
    }
}
