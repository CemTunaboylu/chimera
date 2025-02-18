use rowan::GreenNode;

use crate::{errors::ParseError, language::SyntaxNode, sink::Sink};

#[derive(Debug)]
pub struct ConcreteSyntaxTree {
    pub green_node: GreenNode,
    pub errors: Vec<ParseError>,
}

impl ConcreteSyntaxTree {
    pub(crate) fn from(sink: Sink) -> Self {
        let (green_node, errors) = sink.finish();
        Self { green_node, errors }
    }
    pub fn debug_tree(&self) -> String {
        let syntax_node = SyntaxNode::new_root(self.green_node.clone());
        let formatted = format!("{:#?}", syntax_node);
        formatted[0..formatted.len() - 1].to_string()
    }
}
