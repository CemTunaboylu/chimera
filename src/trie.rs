use trie_rs::{
    inc_search::IncSearch,
    map::{Trie, TrieBuilder},
};

use std::{
    fmt::Debug,
    iter::{Enumerate, Peekable},
    ops::Range,
    str::Chars,
};

#[derive(Debug, PartialEq)]
pub struct MatchResult<'m, V>(Range<usize>, &'m V, &'m str);

fn match_result_with<'m, V>(
    program: &'m str,
    range: Range<usize>,
    value: &'m V,
) -> MatchResult<'m, V> {
    let lexeme = &program[range.clone()];
    MatchResult(range, value, lexeme)
}

struct FirstMatchFinder<'m> {
    program: Peekable<Enumerate<Chars<'m>>>,
    search: IncSearch<'m, char, usize>,
    length: usize,
}

impl<'m> FirstMatchFinder<'m> {
    pub fn new(program: &'m str, search: IncSearch<'m, char, usize>) -> Self {
        Self {
            program: program.chars().enumerate().peekable(),
            search: search,
            length: program.len(),
        }
    }
}

impl<'m> Iterator for FirstMatchFinder<'m> {
    type Item = (Range<usize>, &'m usize);

    fn next(&mut self) -> Option<Self::Item> {
        self.search.reset();

        let mut first_ix_of_match: Option<usize> = None;
        let mut candidate: Option<Self::Item> = None;
        let mut consume_program_char;
        let mut query_the_char;

        while let Some((index, ch)) = self.program.peek().map(|t| *t) {
            (consume_program_char, query_the_char) = (true, true);
            match (first_ix_of_match, self.search.peek(&ch)) {
                // haven't seen any matched chars and next search query also does NOT match
                (None, None) => {
                    query_the_char = false; // do NOT query the char, since it will be None
                }
                // haven't seen any matched chars BUT next search query matches, i.e. a match is starting
                (None, Some(_)) => {
                    first_ix_of_match = Some(index);
                }
                // have started a match on the trie BUT next search query does NOT match, i.e. match is ending
                (Some(start_index), None) => {
                    match self.search.value() {
                        // if a FULL match, search must have a value
                        Some(v) => {
                            candidate =
                                Some((start_index..start_index + self.search.prefix_len(), v));
                            break;
                        }
                        // have a partial match , thus search needs to be reset to 'forget' the past
                        None => {
                            self.search.reset();
                            // since next search query does NOT match, disable the query
                            query_the_char = false;
                        }
                    };
                }
                // started a match on the trie and next search query ALSO matches
                // thus the peeked program char needs to be consumed, search must be queried to move on the trie
                (Some(_), Some(_)) => { /* no need to do anything */ }
            }
            if query_the_char {
                self.search.query(&ch);
            }
            if consume_program_char {
                self.program.next();
            }
        }

        let no_candidate_from_loop = candidate.is_none();
        if no_candidate_from_loop {
            candidate = Some((first_ix_of_match?..self.length, self.search.value()?));
        }

        candidate
    }
}

pub trait TokeningTrie<'t, V: 't> {
    fn first_match(&'t self, program: &'t str) -> impl Iterator<Item = MatchResult<'t, V>>;
}

pub struct IndexTrie {
    trie: Trie<char, usize>,
}

pub struct IndexTrieIterator<'i>(FirstMatchFinder<'i>, &'i str);

impl<'i> TokeningTrie<'i, usize> for IndexTrie {
    fn first_match(&'i self, program: &'i str) -> impl Iterator<Item = MatchResult<'i, usize>> {
        IndexTrieIterator(
            FirstMatchFinder::new(program, self.trie.inc_search()),
            program,
        )
    }
}

impl<'i> Iterator for IndexTrieIterator<'i> {
    type Item = MatchResult<'i, usize>;

    fn next(&mut self) -> Option<Self::Item> {
        let (range, value) = self.0.next()?;
        Some(match_result_with(self.1, range, value))
    }
}

pub struct ArenaTrie<'a, V> {
    trie: Trie<char, usize>,
    values: Box<[&'a V]>,
}
pub struct ArenaTrieIterator<'i, V>(FirstMatchFinder<'i>, &'i str, Box<[&'i V]>);

impl<'i, V: Debug> TokeningTrie<'i, V> for ArenaTrie<'i, V> {
    fn first_match(&'i self, program: &'i str) -> impl Iterator<Item = MatchResult<'i, V>> {
        ArenaTrieIterator(
            FirstMatchFinder::new(program, self.trie.inc_search()),
            program,
            self.values.clone(),
        )
    }
}

impl<'i, V: 'i + Debug> Iterator for ArenaTrieIterator<'i, V> {
    type Item = MatchResult<'i, V>;
    fn next(&mut self) -> Option<Self::Item> {
        let (range, value_index) = self.0.next()?;
        let value = self.2.get(*value_index).map(|ref_ref_val| *ref_ref_val)?;
        Some(match_result_with(self.1, range, value))
    }
}

pub struct ArenaTrieBuilder<'t, T> {
    trie_builder: TrieBuilder<char, usize>,
    values: Vec<&'t T>,
}

impl<'t, T> Default for ArenaTrieBuilder<'t, T> {
    fn default() -> Self {
        Self {
            trie_builder: TrieBuilder::<char, usize>::new(),
            values: vec![],
        }
    }
}

impl<'t, T> ArenaTrieBuilder<'t, T> {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn insert(&mut self, key: &'t str, value: &'t T) {
        let index_to_push = self.values.len();
        self.values.push(value);
        let chars: Vec<char> = key.chars().into_iter().collect();
        self.trie_builder.insert(chars, index_to_push);
    }
    pub fn build(self) -> ArenaTrie<'t, T> {
        let trie = self.trie_builder.build();
        ArenaTrie {
            trie: trie,
            values: self.values.into_boxed_slice(),
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    enum Call<'t> {
        Insert {
            to_insert: &'t [&'t str],
        },
        FirstMatch {
            to_insert_first: &'t [&'t str],
            expected_match_results: (&'t str, &'t [Option<MatchResult<'t, bool>>]),
        },
    }

    fn test_tokening_trie_cast<'test, V: 'test>(_: &impl TokeningTrie<'test, V>) {}

    macro_rules! trie_tests {
    ($($name:ident: $value:expr,)*) => {
    $(
        #[test]
        fn $name() {
            let method_to_call = $value;
            let mut ttb : ArenaTrieBuilder<bool>= ArenaTrieBuilder::new();

            match method_to_call {
                super::tests::Call::Insert{to_insert}  => {
                    for i in to_insert {
                        ttb.insert(i, &true);
                    }
                    let trie = ttb.build();
                    // ensure it is cast properly
                    test_tokening_trie_cast(&trie);
                    for (x, i) in to_insert.into_iter().enumerate() {
                        let chars: Vec<char> = i.chars().into_iter().collect();
                        let r = trie.trie.exact_match(chars);
                        assert_eq!(true, r.is_some());
                        assert_eq!(x, *r.unwrap());
                    }
                },
                super::tests::Call::FirstMatch{to_insert_first,expected_match_results}  => {
                    for i in to_insert_first {
                        ttb.insert(i, &true);
                    }
                    let trie = ttb.build();
                    test_tokening_trie_cast(&trie);
                    let mut matches = trie.first_match(expected_match_results.0);
                    for expected in expected_match_results.1.into_iter() {
                        let item = matches.next();
                        assert_eq!(*expected, item);
                        if expected.is_some() {
                            let MatchResult(_, _, lexeme) = item.unwrap();
                            let MatchResult(_, _, key) = expected.as_ref().unwrap();
                            assert_eq!(*key, lexeme)
                        }
                    }
                },
            };
        }
    )*
    }
    }

    trie_tests! {
        insert_a: Call::Insert{to_insert: &["a"]},
        insert_a_ab_abc: Call::Insert{to_insert: &["a", "ab", "abc"]},
        insert_a_abcd_ab: Call::Insert{to_insert: &["a", "abcd", "ab"]},
        insert_impl_fn_struct_for: Call::Insert{to_insert: &["impl", "fn", "struct", "for"]},
        insert_averylongvariablename: Call::Insert{to_insert: &["averylongvariablename"]},
        insert_i_i16_i32: Call::Insert{to_insert: &["i", "i16", "i32"]},
        insert_space: Call::Insert{to_insert: &[" "]},
        first_match_equals_in_between: Call::FirstMatch {
            to_insert_first: &["="],
            expected_match_results: ("let var = 5;", &[Some(MatchResult(8..9, &true, "="))]) },

        first_match_plus_in_between: Call::FirstMatch {
            to_insert_first: &["+"],
            expected_match_results:
            ("chi+=mera;", &[Some(MatchResult(3..4, &true, "+"))]) },

        first_match_impl_at_0: Call::FirstMatch {
            to_insert_first: &["impl"],
            expected_match_results: ("impl Chimeratic", &[Some(MatchResult(0..4, &true, "impl"))]) },

        first_match_at_the_end: Call::FirstMatch {
            to_insert_first: &[" ", "+", ";"],
            expected_match_results:
                (" chi+=mera;",
                &[
                    Some(MatchResult(0..1, &true, " ")),
                    Some(MatchResult(4..5, &true, "+")),
                    Some(MatchResult(10..11, &true, ";")),
                ])
                },

        first_match_whitespaces: Call::FirstMatch {
            to_insert_first: &[" ", "  "],
            expected_match_results:
                ("   ",
                &[
                    Some(MatchResult(0..2, &true, "  ")),
                    Some(MatchResult(2..3, &true, " ")),
                ])
                },

        first_match_bbbaa: Call::FirstMatch {
            to_insert_first: &["a", "bb", "ba"],
            expected_match_results:
                ("bbbaa",
                &[
                    Some(MatchResult(0..2, &true, "bb")),
                    Some(MatchResult(2..4, &true, "ba")),
                    Some(MatchResult(4..5, &true, "a")),
                ])
                },
        first_match_bbbaa_insertion_order_does_not_matter: Call::FirstMatch {
            to_insert_first: &["ba", "bb", "a"],
            expected_match_results:
                ("bbbaa",
                &[
                    Some(MatchResult(0..2, &true, "bb")),
                    Some(MatchResult(2..4, &true, "ba")),
                    Some(MatchResult(4..5, &true, "a")),
                ])
                },
    }
}
