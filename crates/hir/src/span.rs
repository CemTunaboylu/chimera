use std::{cmp::Ordering, fmt::Debug, ops::Range};

#[derive(Clone, Copy, Default, Eq, Hash, PartialEq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}..{}", self.start, self.end))
    }
}

impl PartialOrd for Span {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if let Some(result) = self.start.partial_cmp(&other.end) {
            match result {
                Ordering::Greater | Ordering::Less => Some(result),
                Ordering::Equal => self.end.partial_cmp(&other.start),
            }
        } else {
            self.end.partial_cmp(&other.start)
        }
    }
}

impl From<Range<usize>> for Span {
    fn from(value: Range<usize>) -> Self {
        Self {
            start: value.start,
            end: value.end,
        }
    }
}
