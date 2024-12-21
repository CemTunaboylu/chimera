pub trait Indexable {
    fn index(self) -> usize;
}

pub(crate) trait Set {
    fn add(&mut self, item: impl Indexable);
    fn del(&mut self, item: impl Indexable);
    fn has(self, item: impl Indexable) -> bool;
}
