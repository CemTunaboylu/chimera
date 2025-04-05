use crate::sink::Sink;
use syntax::language::SyntaxNode;

use miette::Report;
use thin_vec::ThinVec;

#[derive(Debug)]
pub struct ConcreteSyntaxTree {
    pub root: SyntaxNode,
    pub errors: ThinVec<Report>,
}

impl ConcreteSyntaxTree {
    pub fn debug_tree(&self) -> String {
        let syntax_node = self.root.clone();
        let formatted = format!("{:#?}", syntax_node);
        formatted[0..formatted.len() - 1].to_string()
    }
}

impl From<Sink<'_>> for ConcreteSyntaxTree {
    fn from(sink: Sink) -> Self {
        let (green_node, errors) = sink.finish();
        Self {
            root: SyntaxNode::new_root(green_node),
            errors,
        }
    }
}
