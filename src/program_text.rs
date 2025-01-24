use std::{iter::Peekable, str::CharIndices};

pub type PeekableCharsWithIndices<'m> = Peekable<CharIndices<'m>>;
pub struct Program<'p> {
    length: usize,
    text: PeekableCharsWithIndices<'p>,
}

pub type Validator = fn(ch: &char) -> bool;

impl<'p> Program<'p> {
    pub fn new(input: &'p str) -> Self {
        Self {
            text: input.char_indices().peekable(),
            length: input.len(),
        }
    }
    pub fn peek(&mut self) -> Option<(usize, char)> {
        self.text.peek().map(|ix_and_ch_tuple| *ix_and_ch_tuple)
    }

    pub fn consume(&mut self) -> Option<(usize, char)> {
        self.text.next()
    }

    pub fn consume_while(&mut self, f: Validator) {
        while self.text.next_if(|e: &(usize, char)| f(&e.1)).is_some() {}
    }
    pub fn at_end(&mut self) -> bool {
        self.text.peek().is_none()
    }
    pub fn len(&self) -> usize {
        self.length
    }
}
