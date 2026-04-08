use smol_str::SmolStr;
use std::{
    collections::HashMap,
    fmt::Debug,
    hash::{Hash, Hasher},
    marker::PhantomData,
};

use thin_vec::thin_vec;

use crate::{
    expression::Expr,
    function::On,
    index_types::ExprIdx,
    resolution::{Baggage, Reference},
    typing::hindley_milner::types::{Maybe, Status, Type},
};

pub struct FingerPrints<I: Hash, H: Hasher> {
    map: HashMap<u64, I>,
    __hasher: PhantomData<H>,
}

impl<I: Debug + Hash, H: Hasher> Debug for FingerPrints<I, H> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FingerPrints")
            .field("map", &self.map)
            .field("__hasher", &self.__hasher)
            .finish()
    }
}

impl<I: Hash, H: Hasher + Default> FingerPrints<I, H> {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
            __hasher: PhantomData,
        }
    }

    pub fn get(&self, fingerprint: u64) -> Option<&I> {
        self.map.get(&fingerprint)
    }

    pub fn insert(&mut self, index: I, fingerprint: u64) -> Result<(), SmolStr> {
        if self.map.contains_key(&fingerprint) {
            return Err(SmolStr::from("key already exists"));
        }
        self.map.insert(fingerprint, index);
        Ok(())
    }

    pub fn fingerprint<V: Hash>(value: &V) -> u64 {
        let mut hasher = H::default();
        value.hash(&mut hasher);
        hasher.finish()
    }
}
