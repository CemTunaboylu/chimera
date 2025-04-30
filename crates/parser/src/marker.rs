use drop_bomb::DropBomb;
use std::marker::PhantomData;

mod private {
    pub trait Sealed {}
}

pub trait MarkerState: private::Sealed {}

#[derive(Debug)]
pub enum Incomplete {}
impl MarkerState for Incomplete {}
impl private::Sealed for Incomplete {}
#[derive(Debug)]
pub enum Complete {}
impl MarkerState for Complete {}
impl private::Sealed for Complete {}

#[derive(Debug)]
pub struct Marker<S: MarkerState = Incomplete> {
    checkpoint: usize,
    bomb: DropBomb,
    __state: PhantomData<S>,
}

impl<S: MarkerState> Marker<S> {
    pub fn get_checkpoint(&self) -> usize {
        self.checkpoint
    }
}

pub static MARKER_BOMB_MSG: &str = "Markers cannot be left incomplete";

impl Marker<Incomplete> {
    pub fn new(checkpoint: usize) -> Self {
        Self {
            checkpoint,
            bomb: DropBomb::new(MARKER_BOMB_MSG),
            __state: PhantomData,
        }
    }

    pub fn complete(mut self) -> Marker<Complete> {
        self.bomb.defuse();
        Marker::<Complete> {
            checkpoint: self.checkpoint,
            bomb: self.bomb,
            __state: PhantomData,
        }
    }
}

impl Marker<Complete> {
    pub fn forward_parent_index_from(&self, new_marker: &Marker<Incomplete>) -> usize {
        new_marker.checkpoint - self.checkpoint
    }
}
