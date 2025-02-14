use std::iter::Peekable;

use rowan::GreenNode;

use crate::{
    event::Event,
    event_holder::EventHolder,
    language::SyntaxNode,
    lexer::{Lexer, SyntaxKind},
    marker::Marker,
    s_expression::pratt_parser,
    sink::Sink,
};

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
        Some((Result::Ok(kind), _lexeme)) if kind.is_trivia() => {
            let (kind, lexeme) = lexer.next().unwrap();
            Some(Event::AddToken {
                kind: kind.expect("expects syntax kind"),
                lexeme: lexeme.into(),
            })
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

pub struct Parser<'p> {
    lexer: Peekable<Lexer<'p>>,
    pub event_holder: EventHolder,
}

impl<'p> Parser<'p> {
    pub fn new(program: &'p str) -> Self {
        Self {
            lexer: Lexer::new(program).peekable(),
            event_holder: EventHolder::new(),
        }
    }
    pub fn parse<B: ASTBehavior>(mut self) -> Parse {
        let root_marker = self.start();
        pratt_parser::<B>(&mut self);
        root_marker.complete(&mut self.event_holder, SyntaxKind::Root);

        let sink = Sink::new(self.event_holder);

        Parse {
            green_node: sink.finish(),
        }
    }

    pub fn peek<B: ASTBehavior>(&mut self) -> Option<Result<SyntaxKind, ()>> {
        B::apply(&mut self.lexer, &mut self.event_holder);
        self.peek_token_kind()
    }

    fn peek_token_kind(&mut self) -> Option<Result<SyntaxKind, ()>> {
        self.lexer.peek().map(|(k, _)| *k)
    }

    pub fn bump(&mut self) {
        let (kind, lexeme) = self.lexer.next().unwrap();
        self.event_holder.push(Event::AddToken {
            kind: kind.expect("expected tokenkind"),
            lexeme: lexeme.into(),
        });
    }
    pub fn start(&mut self) -> Marker {
        let checkpoint = self.event_holder.checkpoint();
        self.event_holder.push(Event::Marker { checkpoint });
        Marker::new(checkpoint)
    }
    pub fn checkpoint(&mut self) -> usize {
        self.event_holder.checkpoint()
    }
}
