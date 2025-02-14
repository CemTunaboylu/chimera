use std::mem;

use rowan::{GreenNode, GreenNodeBuilder, Language};

use crate::{event::Event, event_holder::EventHolder, language::ChimeraLanguage};

pub(super) struct Sink {
    builder: GreenNodeBuilder<'static>,
    events: EventHolder,
}

impl Sink {
    pub(super) fn new(events: EventHolder) -> Self {
        Self {
            builder: GreenNodeBuilder::new(),
            events,
        }
    }

    pub(super) fn finish(mut self) -> GreenNode {
        dbg!(&self.events);
        let mut events: Vec<Event> = self.events.into();
        for ix in 0..events.len() {
            match mem::replace(&mut events[ix], Event::Moved) {
                Event::StartNode {
                    kind,
                    forward_parent,
                } => {
                    let mut forward_parent_kinds = vec![kind];
                    let mut ix = ix;
                    let mut forward_parent = forward_parent;

                    while let Some(forward_parent_index) = forward_parent {
                        ix += forward_parent_index;
                        if let Event::StartNode {
                            kind,
                            forward_parent: next_forward_parent,
                        } = mem::replace(&mut events[ix], Event::Moved)
                        {
                            forward_parent_kinds.push(kind);
                            forward_parent = next_forward_parent;
                        } else {
                            unreachable!()
                        }
                    }
                    for forward_parent_kind in forward_parent_kinds.iter().rev() {
                        self.builder
                            .start_node(ChimeraLanguage::kind_to_raw(*forward_parent_kind));
                    }
                }
                Event::AddToken { kind, lexeme } => self
                    .builder
                    .token(ChimeraLanguage::kind_to_raw(kind), lexeme.as_str()),
                Event::FinishNode => self.builder.finish_node(),
                Event::Marker { .. } => unreachable!(),
                Event::Moved => {}
            }
        }
        self.builder.finish()
    }
}
