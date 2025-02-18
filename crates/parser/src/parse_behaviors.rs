use std::fmt::Debug;

use lexer::lexer::Lexer;

use crate::{event::Event, event_holder::EventHolder, syntax::Syntax};

pub trait ASTBehavior: Debug {
    fn apply(lexer: &mut Lexer, event_holder: &mut EventHolder);
}

fn prepare_trivias_event(lexer: &mut Lexer) -> Option<Event> {
    let lex_result = lexer.peek()?;
    match lex_result {
        Ok(token) => {
            let syntax = Syntax::from(token.clone());
            if syntax.is_trivia() {
                lexer.next().unwrap().expect("expects syntax kind");
                Some(Event::AddSyntax { syntax })
            } else {
                None
            }
        }
        _ => None,
    }
}

#[derive(Debug)]
pub enum IgnoreTrivia {}
impl ASTBehavior for IgnoreTrivia {
    fn apply(lexer: &mut Lexer, event_holder: &mut EventHolder) {
        if let Some(trivia_event) = prepare_trivias_event(lexer) {
            event_holder.ignore(trivia_event);
        }
    }
}

#[derive(Debug)]
pub enum NonIgnoring {}
impl ASTBehavior for NonIgnoring {
    fn apply(lexer: &mut Lexer, event_holder: &mut EventHolder) {
        if let Some(trivia_event) = prepare_trivias_event(lexer) {
            event_holder.push(trivia_event);
        }
    }
}
