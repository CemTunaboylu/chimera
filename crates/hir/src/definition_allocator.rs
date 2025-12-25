use crate::{HIRError, HIRResult, index_types::StrIdx};
use la_arena::{Arena, Idx};
use patricia_tree::PatriciaMap;
use smol_str::SmolStr;
use std::fmt::Debug;

pub type NameToIndexTrie<T> = PatriciaMap<Idx<T>>;

pub trait NameIndexed {
    fn set_name_index(&mut self, ix: StrIdx);
    fn get_name_index(&self) -> StrIdx;
}

#[derive(Debug, Default)]
pub struct DefAllocator<D: NameIndexed> {
    pub names: Arena<SmolStr>,
    pub definitions: Arena<D>,
    pub name_to_idx_trie: NameToIndexTrie<D>,
}

impl<D: NameIndexed> DefAllocator<D> {
    pub fn alloc(&mut self, name: &SmolStr, mut d: D) -> HIRResult<Idx<D>> {
        if self.name_to_idx_trie.get(name).is_some() {
            return Err(HIRError::with_msg(format!(
                "shadowing is not allowed, {:?} is already defined",
                name
            ))
            .into());
        }
        let name_index = self.names.alloc(name.clone());
        d.set_name_index(name_index);

        let ix = self.definitions.alloc(d);
        self.name_to_idx_trie.insert(&name, ix);
        Ok(ix)
    }
}
