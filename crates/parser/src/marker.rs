use std::marker::PhantomData;

use crate::{event::Event, event_holder::EventHolder, parser::Parser, syntax::SyntaxKind};
use drop_bomb::DropBomb;

mod private {
    pub trait Sealed {}
}

pub trait MarkerState: private::Sealed {}

pub enum Incomplete {}
impl MarkerState for Incomplete {}
impl private::Sealed for Incomplete {}
pub enum Complete {}
impl MarkerState for Complete {}
impl private::Sealed for Complete {}

pub struct Marker<S: MarkerState = Incomplete> {
    checkpoint: usize,
    bomb: DropBomb,
    __state: PhantomData<S>,
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

    pub fn complete(
        mut self,
        event_holder: &mut EventHolder,
        kind: SyntaxKind,
    ) -> Marker<Complete> {
        let index = self.checkpoint;
        let corresponding_event = event_holder.get_mut(index).unwrap();

        corresponding_event.validate_marker_event(index);

        *corresponding_event = Event::new_start_node_with(kind);
        event_holder.push(Event::FinishNode);
        self.bomb.defuse();

        Marker::<Complete> {
            checkpoint: self.checkpoint,
            bomb: self.bomb,
            __state: PhantomData,
        }
    }
}

impl Marker<Complete> {
    pub fn precede(self, parser: &mut Parser) -> Marker<Incomplete> {
        let new_marker = parser.start();
        let forward_parent_index = new_marker.checkpoint - self.checkpoint;
        match parser.event_holder.get_mut(self.checkpoint) {
            // add_forward_parent_to_start_node panics if it is not a start node
            Some(event) => event.add_forward_parent_to_start_node(forward_parent_index),
            None => unreachable!(),
        }
        new_marker
    }
}
