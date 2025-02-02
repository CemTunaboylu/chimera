#![no_main]
use enum_primitive::FromPrimitive;

use crate::keywords;
use crate::{
    match_validator::ToValidate,
    trie::{MatchResult, MatchingPeekable, TokenTrie, TokeningTrieBuilder},
};
use std::{fmt::Debug, iter::Peekable};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Token<'l> {
    kind: TokenKind,
    position: Position,
    lexeme: &'l str,
}
#[derive(Clone, Copy, Debug, PartialEq)]
struct Position {
    line: u32,
    start: u32,
    end: u32,
}

pub trait Expectable: PartialEq + Debug + Copy {}
impl Expectable for Token<'_> {}
impl Expectable for char {}
pub struct Expectation<E: Expectable> {
    lhs: E,
    rhs: E,
}

impl<E: Expectable> Expectation<E> {
    pub fn new(l: E, r: E) -> Self {
        Self { lhs: l, rhs: r }
    }

    pub fn is_met(&self) -> Result<(), LexerError> {
        if self.lhs == self.rhs {
            return Result::Ok(());
        }
        return Result::Err(LexerError::ExpectationFailed("Fail"));
    }
}

#[derive(Debug, PartialEq)]
pub enum LexerError {
    ExpectationFailed(&'static str),
}

pub struct Lexer<'l> {
    program: &'l str,
    line: usize,
    trie: TokenTrie<TokenKind>,
    // trie: MatchingPeekable<'l, Token<'l>>,
}

impl<'l> Lexer<'l> {
    // TODO: build the trie
    pub fn new(program: &'l str) -> Self {
        let trie = create_chimera_trie();
        Self {
            program: program,
            line: 0,
            trie: trie,
        }
    }
    fn expect<E: Expectable>(&self, ex: Expectation<E>) -> Result<(), LexerError> {
        // TODO: peek the next token and assert equality
        Result::Ok(())
    }
    fn expect_whitespace(&mut self) -> Result<(), LexerError> {
        Result::Ok(())
        // self.expect(Expect::Char(|c| c.is_ascii_whitespace()))
    }
}
impl<'l> Iterator for Lexer<'l> {
    type Item = Token<'l>;

    fn next(&mut self) -> Option<Self::Item> {
        unimplemented!()
    }
}

// impl<'l> peekable for Lexer<'l> {}

#[cfg(test)]
mod tests {
    use super::*;
    use parameterized_test::create;

    create! {
    create_chimera_trie_tests,
    (program, expected), {
        let trie = create_chimera_trie();
        let match_finder = trie.first_match(&program);
        for (match_result, exp) in match_finder.zip(expected.iter()) {
            assert_eq!(match_result, *exp);
        }
        }
    }

    create_chimera_trie_tests! {
        order_is_preserved: ("(while", &[
        MatchResult::Full(0..1, &TokenKind::LeftParen),
        MatchResult::Full(1..6, &TokenKind::While)]),
    }
}
