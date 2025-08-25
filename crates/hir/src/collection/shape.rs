use thin_vec::{ThinVec, thin_vec};

use crate::scope::ScopedExprIdx;

#[derive(Clone, Debug, Default, Eq, Hash, PartialEq, PartialOrd)]
pub enum Dim {
    Static(usize),
    Dynamic(ScopedExprIdx),
    #[default]
    Unknown,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub enum Shape {
    Buffer(ThinVec<usize>),
    Tensor(ThinVec<Dim>),
}

impl Default for Shape {
    fn default() -> Self {
        Self::Tensor(Default::default())
    }
}
impl Shape {
    pub fn get(&self) -> Option<&[usize]> {
        match self {
            Shape::Buffer(shape) => Some(shape.as_slice()),
            Shape::Tensor(_) => None,
        }
    }
    pub fn dimensionality(&self) -> usize {
        match self {
            Shape::Buffer(shape) => shape.len(),
            Shape::Tensor(shape) => shape.len(),
        }
    }
}

/// A small struct that forms the shape of the
pub(crate) struct ShapeFormer {
    shape: ThinVec<usize>,
    depth: usize,
}
impl ShapeFormer {
    #[inline]
    pub(crate) fn new(len: usize) -> Self {
        Self {
            shape: thin_vec![len],
            depth: 1,
        }
    }
    #[inline]
    pub(crate) fn push_dim_for_shape(&mut self, dim: usize) {
        if self.shape.len() <= self.depth {
            self.shape.push(dim)
        }
    }
    #[inline]
    pub(crate) fn deepen(&mut self) {
        self.depth += 1;
    }
    #[inline]
    pub(crate) fn new_depth(&mut self, d: usize) {
        self.depth = d;
    }
    #[inline]
    pub(crate) fn get_depth(&mut self) -> usize {
        self.depth
    }
    #[inline]
    pub(crate) fn form(self) -> Shape {
        Shape::Buffer(self.shape)
    }
}
