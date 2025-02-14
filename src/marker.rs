use std::marker::PhantomData;

use crate::{
    event::{new_start_node_with, Event},
    event_holder::EventHolder,
    lexer::SyntaxKind,
    parser::Parser,
};
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
        assert_eq!(*corresponding_event, Event::Marker { checkpoint: index });
        *corresponding_event = new_start_node_with(kind);
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
        match parser.event_holder.get_mut(self.checkpoint) {
            Some(Event::StartNode {
                ref mut forward_parent,
                ..
            }) => {
                *forward_parent = Some(new_marker.checkpoint - self.checkpoint);
            }
            _ => unreachable!(),
        }
        new_marker
    }
}
