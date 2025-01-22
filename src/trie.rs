use trie_rs::{
    inc_search::IncSearch,
    map::{Trie, TrieBuilder},
};

use std::{
    fmt::Debug,
    iter::{Enumerate, Peekable},
    marker::PhantomData,
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

type Expectation = fn(ch: char) -> bool;
struct FirstMatchFinder<'m> {
    program: Peekable<Enumerate<Chars<'m>>>,
    search: IncSearch<'m, char, usize>,
    length: usize,
    expects: Option<Expectation>,
}

impl<'m> FirstMatchFinder<'m> {
    // I might be bored a little
    pub fn vanilla(program: &'m str, search: IncSearch<'m, char, usize>) -> Self {
        Self {
            program: program.chars().enumerate().peekable(),
            search: search,
            length: program.len(),
            expects: None,
        }
    }

    pub fn expecting(
        program: &'m str,
        search: IncSearch<'m, char, usize>,
        expects: Expectation,
    ) -> Self {
        Self {
            program: program.chars().enumerate().peekable(),
            search: search,
            length: program.len(),
            expects: Some(expects),
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
            let ch_is_matched = self.search.peek(&ch).is_some();

            match (first_ix_of_match, ch_is_matched) {
                // haven't seen any matched chars and next search query also does NOT match
                (None, false) => {
                    query_the_char = false; // do NOT query the char, since it will be None
                }
                // haven't seen any matched chars BUT next search query matches, i.e. a match is starting
                (None, true) => {
                    first_ix_of_match = Some(index);
                }
                // have started a match on the trie BUT next search query does NOT match, i.e. match is ending
                (Some(start_index), false) => {
                    let value_opt = self.search.value();
                    // if a FULL match, search must have a value AND if there is any expectation(s), all must be met
                    if value_opt.is_some() && self.expects.is_none_or(|f| f(ch)) {
                        candidate = Some((
                            start_index..start_index + self.search.prefix_len(),
                            value_opt.unwrap(),
                        ));
                        break;
                    }
                    // have a partial match, OR if any, min. one expectation failed
                    // thus search needs to be reset to 'forget' the past
                    self.search.reset();
                    first_ix_of_match = None;
                    // since next search query does NOT match, disable the query
                    query_the_char = false;
                }
                // started a match on the trie and next search query ALSO matches
                // thus the peeked program char needs to be consumed, search must be queried to move on the trie
                (Some(_), true) => { /* no need to do anything */ }
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
            let value = self.search.value()?;
            // At this point, we started a match BUT could NOT finish it (search.peek() didn't hit None),
            // at best, it's a full match BUT expectation cannot be met since there is NO preceding character
            // TODO: this can be proplematic after, thus we need to be able to control this behavior
            if self.expects.is_none() {
                candidate = Some((first_ix_of_match?..self.length, value));
            }
        }

        candidate
    }
}

pub trait TokeningTrie<'t, V: 't> {
    fn first_match(&'t self, program: &'t str) -> impl Iterator<Item = MatchResult<'t, V>>;
    fn first_match_if(
        &'t self,
        program: &'t str,
        expects: Expectation,
    ) -> impl Iterator<Item = MatchResult<'t, V>>;
}

pub struct IndexTrie {
    trie: Trie<char, usize>,
}

pub struct IndexTrieIterator<'i>(FirstMatchFinder<'i>, &'i str);

impl<'i> TokeningTrie<'i, usize> for IndexTrie {
    fn first_match(&'i self, program: &'i str) -> impl Iterator<Item = MatchResult<'i, usize>> {
        IndexTrieIterator(
            FirstMatchFinder::vanilla(program, self.trie.inc_search()),
            program,
        )
    }

    fn first_match_if(
        &'i self,
        program: &'i str,
        expects: Expectation,
    ) -> impl Iterator<Item = MatchResult<'i, usize>> {
        IndexTrieIterator(
            FirstMatchFinder::expecting(program, self.trie.inc_search(), expects),
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
            FirstMatchFinder::vanilla(program, self.trie.inc_search()),
            program,
            self.values.clone(),
        )
    }

    fn first_match_if(
        &'i self,
        program: &'i str,
        expects: Expectation,
    ) -> impl Iterator<Item = MatchResult<'i, V>> {
        ArenaTrieIterator(
            FirstMatchFinder::expecting(program, self.trie.inc_search(), expects),
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

enum Index {}
enum Arena {}

trait State {}

impl State for Index {}
impl State for Arena {}

pub struct TokeningTrieBuilder<'t, T, S: State> {
    trie_builder: TrieBuilder<char, usize>,
    values: Option<Vec<&'t T>>,
    __marker: PhantomData<S>,
}
impl<'t, T, S: State> TokeningTrieBuilder<'t, T, S> {
    pub fn indexed() -> Self {
        Self {
            trie_builder: TrieBuilder::<char, usize>::new(),
            values: None,
            __marker: PhantomData,
        }
    }
    pub fn arena() -> Self {
        Self {
            trie_builder: TrieBuilder::<char, usize>::new(),
            values: Some(vec![]),
            __marker: PhantomData,
        }
    }
}

impl<'t> TokeningTrieBuilder<'t, usize, Index> {
    pub fn insert(&mut self, key: &'t str, value: &'t usize) {
        let chars: Vec<char> = key.chars().into_iter().collect();
        self.trie_builder.insert(chars, *value);
    }

    pub fn build(self) -> IndexTrie {
        let trie = self.trie_builder.build();
        IndexTrie { trie: trie }
    }
}

impl<'t, T> TokeningTrieBuilder<'t, T, Arena> {
    pub fn insert(&mut self, key: &'t str, value: &'t T) {
        let v = self.values.as_mut().unwrap();
        let index_to_push = v.len();
        v.push(value);
        let chars: Vec<char> = key.chars().into_iter().collect();
        self.trie_builder.insert(chars, index_to_push);
    }

    pub fn build(self) -> ArenaTrie<'t, T> {
        let trie = self.trie_builder.build();
        ArenaTrie {
            trie: trie,
            values: self.values.unwrap().into_boxed_slice(),
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
        FirstMatchIf {
            to_insert_first: &'t [&'t str],
            expects: Expectation,
            expected_match_results: (&'t str, &'t [Option<MatchResult<'t, bool>>]),
        },
    }

    fn has_space_at_the_end(ch: char) -> bool {
        ch == ' '
    }

    fn first_match_if_space_follows<'t>(
        to_insert_first: &'t [&'t str],
        expected_match_results: (&'t str, &'t [Option<MatchResult<'t, bool>>]),
    ) -> Call<'t> {
        Call::FirstMatchIf {
            to_insert_first: to_insert_first,
            expects: has_space_at_the_end,
            expected_match_results: expected_match_results,
        }
    }

    fn test_tokening_trie_cast<'test, V: 'test>(_: &impl TokeningTrie<'test, V>) {}

    macro_rules! trie_tests {
    ($($name:ident: $value:expr,)*) => {
    $(
        #[test]
        fn $name() {
            let method_to_call = $value;
            let mut ttb : TokeningTrieBuilder<bool, Arena> = TokeningTrieBuilder::arena();

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
                super::tests::Call::FirstMatchIf{to_insert_first, expects, expected_match_results}  => {
                    for i in to_insert_first {
                        ttb.insert(i, &true);
                    }
                    let trie = ttb.build();
                    test_tokening_trie_cast(&trie);
                    let mut matches = trie.first_match_if(expected_match_results.0, expects);
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

        first_match_if_equals_in_between_with_space: first_match_if_space_follows(
            &["="],
            ("let var = 5;", &[Some(MatchResult(8..9, &true, "="))])
        ),

        first_match_if_impl_none_since_no_space: first_match_if_space_follows(
            &["impl"],
            ("impl", &[None])),
        first_match_if_impl: first_match_if_space_follows(
            &["impl"],
            ("impl Chimeratic", &[Some(MatchResult(0..4, &true, "impl"))])),

        first_match_if_at_the_end_spaced:
            first_match_if_space_follows(
                &[" ", "+", ";"],
                (" chi+=mera; ",
                &[
                    Some(MatchResult(10..11, &true, ";")),
                ])
            ),

        first_match_if_whitespaces:
            first_match_if_space_follows(
                &[" ", "  "],
                ("   ",
                &[
                    Some(MatchResult(0..2, &true, "  ")),
                    None,
                ])
            ),

    }
}
