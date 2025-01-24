use std::ops::Range;

#[derive(Debug)]
pub struct RangeIndices {
    first_peeked: i32,
    start_of_first_match: i32,
    end_of_match: i32,
}

impl RangeIndices {
    const INVALID_INDEX: i32 = -1;
    pub fn new() -> Self {
        Self {
            first_peeked: Self::INVALID_INDEX,
            start_of_first_match: Self::INVALID_INDEX,
            end_of_match: Self::INVALID_INDEX,
        }
    }
    pub fn start_peeking(&mut self, ix_at_peek: usize) {
        self.first_peeked = ix_at_peek as i32;
        self.delete_match();
    }

    pub fn start_match(&mut self, at: usize) {
        self.start_of_first_match = at as i32;
    }

    pub fn delete_match(&mut self) {
        self.start_of_first_match = Self::INVALID_INDEX;
        self.end_of_match = Self::INVALID_INDEX;
    }

    pub fn end_match(&mut self, at: usize) {
        self.end_of_match = at as i32;
    }
    pub fn has_a_match_started(&self) -> bool {
        self.start_of_first_match != Self::INVALID_INDEX
    }
    pub fn has_an_unmatched_slice_before(&self) -> bool {
        self.start_of_first_match != Self::INVALID_INDEX
            && self.first_peeked < self.start_of_first_match
    }
    fn ternary(&self, is_answer: bool) -> usize {
        let if_no_match = if is_answer {
            self.first_peeked
        } else {
            self.end_of_match
        };
        return if self.has_a_match_started() {
            self.start_of_first_match
        } else {
            if_no_match
        } as usize;
    }
    pub fn form_answer_range(&self) -> Range<usize> {
        let s = self.ternary(true);
        let e = self.end_of_match as usize;
        s..e
    }

    pub fn form_unmatched_range(&self) -> Range<usize> {
        let s = self.first_peeked as usize;
        let e = self.ternary(false);
        s..e
    }
}
