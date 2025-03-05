use std::{collections::HashMap, ops::Range};

use crate::sink::Sink;
use syntax::{ParsedValue, language::SyntaxNode};

use miette::Report;
use thin_vec::ThinVec;

pub type IndicedParsedValues = HashMap<Range<usize>, ParsedValue>;
#[derive(Debug)]
pub struct ConcreteSyntaxTree {
    pub root: SyntaxNode,
    pub parsed_values: IndicedParsedValues,
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
        let (green_node, parsed_values, errors) = sink.finish();
        Self {
            root: SyntaxNode::new_root(green_node),
            parsed_values,
            errors,
        }
    }
}
