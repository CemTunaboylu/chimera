use std::mem;

use rowan::{GreenNode, GreenNodeBuilder, Language};

use crate::{event::Event, language::ChimeraLanguage, lexer::SyntaxKind};

pub(super) struct Sink {
    builder: GreenNodeBuilder<'static>,
    events: Vec<Event>,
}

impl Sink {
    pub(super) fn new(events: Vec<Event>) -> Self {
        Self {
            builder: GreenNodeBuilder::new(),
            events,
        }
    }

    fn pull_with_forward_parents(
        &mut self,
        mut ix: usize,
        kind: SyntaxKind,
        mut forward_parent: Option<usize>,
    ) -> Vec<SyntaxKind> {
        let mut kinds = vec![kind];

        while let Some(forward_parent_index) = forward_parent {
            ix += forward_parent_index;
            if let Event::StartNode {
                kind,
                forward_parent: next_forward_parent,
            } = mem::replace(&mut self.events[ix], Event::Moved)
            {
                kinds.push(kind);
                forward_parent = next_forward_parent;
            } else {
                unreachable!()
            }
        }

        kinds
    }

    pub(super) fn finish(mut self) -> GreenNode {
        dbg!(&self.events);
        for ix in 0..self.events.len() {
            match mem::replace(&mut self.events[ix], Event::Moved) {
                Event::StartNode {
                    kind,
                    forward_parent,
                } => {
                    let forward_parent_kinds =
                        self.pull_with_forward_parents(ix, kind, forward_parent);

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
