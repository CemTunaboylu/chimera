use std::mem;

use miette::Report;
use rowan::{GreenNode, GreenNodeBuilder, Language};
use thin_vec::{ThinVec, thin_vec};

use crate::{cst::IndicedParsedValues, event::Event};

use syntax::{language::ChimeraLanguage, syntax_kind::SyntaxKind};

pub(super) struct Sink<'input> {
    builder: GreenNodeBuilder<'static>,
    program: &'input str,
    events: ThinVec<Event>,
    errors: ThinVec<Report>,
    parsed_values: IndicedParsedValues,
}

impl<'input> Sink<'input> {
    pub(super) fn new(events: ThinVec<Event>, program: &'input str) -> Self {
        Self {
            builder: GreenNodeBuilder::new(),
            events,
            program,
            errors: thin_vec![],
            parsed_values: IndicedParsedValues::new(),
        }
    }

    fn pull_with_forward_parents(
        &mut self,
        mut ix: usize,
        kind: SyntaxKind,
        mut forward_parent: Option<usize>,
    ) -> ThinVec<SyntaxKind> {
        let mut kinds = thin_vec![kind];

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

    pub(super) fn finish(mut self) -> (GreenNode, IndicedParsedValues, ThinVec<Report>) {
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
                Event::AddSyntax { syntax } => {
                    if let Some(parsed_value) = syntax.get_parsed_value() {
                        self.parsed_values
                            .insert(syntax.get_span(), parsed_value.clone());
                    }
                    self.builder.token(
                        ChimeraLanguage::kind_to_raw(syntax.get_kind()),
                        &self.program[syntax.get_span()],
                    );
                }
                Event::FinishNode => self.builder.finish_node(),
                Event::Marker { .. } => unreachable!(),
                Event::Moved => {}
                Event::Error { err } => {
                    let report: Report = err.into();
                    let source = self.program;
                    self.errors
                        .push(report.with_source_code(source.to_string()))
                }
            }
        }
        (self.builder.finish(), self.parsed_values, self.errors)
    }
}
