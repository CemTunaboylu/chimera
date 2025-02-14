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

pub fn new_start_node_with(kind: SyntaxKind) -> Event {
    Event::StartNode {
        kind,
        forward_parent: None,
    }
}
