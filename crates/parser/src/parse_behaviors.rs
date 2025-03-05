use std::fmt::Debug;

use lexer::lexer::Lexer;
use syntax::Syntax;

use crate::{event::Event, event_holder::EventHolder};

pub trait SyntaxingBehavior: Debug {
    fn apply(lexer: &mut Lexer, event_holder: &mut EventHolder);
    // fn apply(token_stream: &mut TokenStream, event_holder: &mut EventHolder);
}

fn prepare_trivias_event(lexer: &mut Lexer) -> Option<Event> {
    // fn prepare_trivias_event(token_stream: &mut TokenStream) -> Option<Event> {
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
impl SyntaxingBehavior for IgnoreTrivia {
    fn apply(lexer: &mut Lexer, event_holder: &mut EventHolder) {
        // fn apply(token_stream: &mut TokenStream, event_holder: &mut EventHolder) {
        if let Some(trivia_event) = prepare_trivias_event(lexer) {
            // if let Some(trivia_event) = prepare_trivias_event(token_stream) {
            // event_holder.ignore(trivia_event);
            event_holder.push(trivia_event);
        }
    }
}

#[derive(Debug)]
pub enum NonIgnoring {}
impl SyntaxingBehavior for NonIgnoring {
    fn apply(lexer: &mut Lexer, event_holder: &mut EventHolder) {
        // fn apply(token_stream: &mut TokenStream, event_holder: &mut EventHolder) {
        if let Some(trivia_event) = prepare_trivias_event(lexer) {
            // if let Some(trivia_event) = prepare_trivias_event(token_stream) {
            event_holder.push(trivia_event);
        }
    }
}
