use crate::errors::ParseError;

use syntax::{Syntax, syntax_kind::SyntaxKind};

#[derive(Debug, Clone, PartialEq)]
pub enum Event {
    Moved,
    StartNode {
        kind: SyntaxKind,
        forward_parent: Option<usize>,
    },
    AddSyntax {
        syntax: Syntax,
    },
    FinishNode,
    Marker {
        checkpoint: usize,
    },
    Error {
        err: ParseError,
    },
}

impl Event {
    pub fn new_start_node_with(kind: SyntaxKind) -> Event {
        Event::StartNode {
            kind,
            forward_parent: None,
        }
    }

    pub fn add_forward_parent_to_start_node(&mut self, index: usize) {
        match self {
            &mut Event::StartNode {
                ref mut forward_parent,
                ..
            } => *forward_parent = Some(index),
            _ => unreachable!(),
        }
    }

    pub fn validate_marker_event(&self, indexed: usize) {
        assert_eq!(
            Event::Marker {
                checkpoint: indexed
            },
            *self
        )
    }
}
