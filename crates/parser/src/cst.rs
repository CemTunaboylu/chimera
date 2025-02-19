use rowan::GreenNode;

use crate::{errors::ParseError, sink::Sink};
use syntax::language::SyntaxNode;

#[derive(Debug)]
pub struct ConcreteSyntaxTree {
    pub green_node: GreenNode,
    pub errors: Vec<ParseError>,
}

impl ConcreteSyntaxTree {
    pub fn debug_tree(&self) -> String {
        let syntax_node = SyntaxNode::new_root(self.green_node.clone());
        let formatted = format!("{:#?}", syntax_node);
        formatted[0..formatted.len() - 1].to_string()
    }

    pub fn syntax_node_root(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.green_node.clone())
    }
}

impl From<Sink<'_>> for ConcreteSyntaxTree {
    fn from(sink: Sink) -> Self {
        let (green_node, errors) = sink.finish();
        Self { green_node, errors }
    }
}
