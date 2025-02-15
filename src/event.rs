use crate::lexer::SyntaxKind;
use smol_str::SmolStr;

#[derive(Debug, Clone, PartialEq)]
pub enum Event {
    Moved,
    StartNode {
        kind: SyntaxKind,
        forward_parent: Option<usize>,
    },
    AddToken {
        kind: SyntaxKind,
        lexeme: SmolStr,
    },
    FinishNode,
    Marker {
        checkpoint: usize,
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
            Event::StartNode {
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
