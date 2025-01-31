use std::{fmt::Debug, iter::Peekable, str::CharIndices};

pub type PeekableCharsWithIndices<'m> = Peekable<CharIndices<'m>>;
#[derive(Debug)]
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

    pub fn peek_index(&mut self) -> usize {
        let length = self.len();
        self.text.peek().map_or(length, |(index, _)| *index)
    }

    pub fn consume(&mut self) -> Option<(usize, char)> {
        self.text.next()
    }

    pub fn consume_while<'c>(&mut self, mut vc: impl Iterator<Item = &'c Validator> + Debug) {
        while self
            .text
            .next_if(|e: &(usize, char)| vc.next().is_some_and(|validator| validator(&e.1)))
            .is_some()
        {}
    }
    pub fn at_end(&mut self) -> bool {
        self.text.peek().is_none()
    }
    pub fn len(&self) -> usize {
        self.length
    }
}
