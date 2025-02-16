use std::iter::Peekable;

use rowan::GreenNode;

use crate::{
    event::Event,
    event_holder::EventHolder,
    language::SyntaxNode,
    lexer::{LexResult, Lexer, SyntaxKind},
    marker::{Complete, Marker},
    s_expression::pratt_parser,
    sink::Sink,
};

use miette::Result as MietteResult;
use std::fmt::Debug;

pub struct Parse {
    green_node: GreenNode,
}

impl Parse {
    pub fn debug_tree(&self) -> String {
        let syntax_node = SyntaxNode::new_root(self.green_node.clone());
        let formatted = format!("{:#?}", syntax_node);
        formatted[0..formatted.len() - 1].to_string()
    }
}

pub trait ASTBehavior: Debug {
    fn apply(lexer: &mut Peekable<Lexer>, event_holder: &mut EventHolder);
}

fn prepare_trivias_event(lexer: &mut Peekable<Lexer>) -> Option<Event> {
    match lexer.peek() {
        Some(Ok(token)) if token.is_trivia() => {
            let token = lexer.next().unwrap().expect("expects syntax kind");
            Some(Event::AddToken { token })
        }
        _ => None,
    }
}

#[derive(Debug)]
pub enum IgnoreTrivia {}

impl ASTBehavior for IgnoreTrivia {
    fn apply(lexer: &mut Peekable<Lexer>, event_holder: &mut EventHolder) {
        if let Some(trivia_event) = prepare_trivias_event(lexer) {
            event_holder.ignore(trivia_event);
        }
    }
}

#[derive(Debug)]
pub enum NonIgnoring {}
impl ASTBehavior for NonIgnoring {
    fn apply(lexer: &mut Peekable<Lexer>, event_holder: &mut EventHolder) {
        if let Some(trivia_event) = prepare_trivias_event(lexer) {
            event_holder.push(trivia_event);
        }
    }
}

pub struct Parser<'input> {
    lexer: Peekable<Lexer<'input>>,
    pub event_holder: EventHolder,
    program: &'input str,
}

// TODO: I can slice the string, instead of smolstr
impl<'input> Parser<'input> {
    pub fn new(program: &'input str) -> Self {
        Self {
            lexer: Lexer::new(program).peekable(),
            event_holder: EventHolder::new(),
            program,
        }
    }
    pub fn parse<B: ASTBehavior>(mut self) -> MietteResult<Parse> {
        let root_marker = self.start();
        let result = pratt_parser::<B>(&mut self);
        root_marker.complete(&mut self.event_holder, SyntaxKind::Root);
        if let Some(err) = result {
            return Err(err);
        }

        // TODO: give the program string to sink
        let sink = Sink::new(self.event_holder.into(), self.program);
        Ok(Parse {
            green_node: sink.finish(),
        })
    }

    pub fn peek<B: ASTBehavior>(&mut self) -> Option<&LexResult> {
        B::apply(&mut self.lexer, &mut self.event_holder);
        self._peek()
    }

    fn _peek(&mut self) -> Option<&LexResult> {
        self.lexer.peek()
    }

    pub fn bump(&mut self) {
        if let Ok(token) = self.lexer.next().unwrap() {
            self.event_holder.push(Event::AddToken { token });
        }
    }

    pub fn is_next(&mut self, expected_kind: SyntaxKind) -> bool {
        if let Some(Ok(token)) = self._peek() {
            token.is_of_kind(expected_kind)
        } else {
            false
        }
    }

    pub fn bump_iff_or_panic(&mut self, expected_kind: SyntaxKind) {
        assert!(self.is_next(expected_kind));
        self.bump();
    }

    pub fn bump_with_marker(&mut self, kind: SyntaxKind) -> Marker<Complete> {
        let marker = self.start();
        self.bump();
        marker.complete(&mut self.event_holder, kind)
    }
    pub fn start(&mut self) -> Marker {
        let checkpoint = self.event_holder.checkpoint();
        self.event_holder.push(Event::Marker { checkpoint });
        Marker::new(checkpoint)
    }
}
