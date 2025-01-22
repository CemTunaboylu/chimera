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

type PostMatchCondition = fn(ch: char) -> bool;

trait SelfInjecting<'i, V> {
    fn inject_into<'a>(self, node: Node<'a, V>) -> Node<'a, V>;
}

impl<'p, V> SelfInjecting<'p, V> for PostMatchCondition {
    fn inject_into<'a>(self, mut node: Node<'a, V>) -> Node<'a, V> {
        node.post_match_condition = Some(self);
        node
    }
}

pub struct Node<'v, V> {
    value: &'v V,
    post_match_condition: Option<PostMatchCondition>,
}

impl<'v, V> Node<'v, V> {
    pub fn new(value: &'v V) -> Self {
        Self {
            value: value,
            post_match_condition: None,
        }
    }
    pub fn post_match_validation(&self, ch: char) -> bool {
        match self.post_match_condition {
            Some(f) => f(ch),
            None => true,
        }
    }
}

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

struct FirstMatchFinder<'m, V> {
    program: Peekable<Enumerate<Chars<'m>>>,
    search: IncSearch<'m, char, usize>,
    nodes: &'m [Node<'m, V>],
    length: usize,
}

impl<'m, V> FirstMatchFinder<'m, V> {
    pub fn new(
        program: &'m str,
        search: IncSearch<'m, char, usize>,
        nodes: &'m [Node<'m, V>],
    ) -> Self {
        Self {
            program: program.chars().enumerate().peekable(),
            search: search,
            nodes: nodes,
            length: program.len(),
        }
    }
}

impl<'m, V> Iterator for FirstMatchFinder<'m, V> {
    type Item = (Range<usize>, &'m V);

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
                    let node_opt = self.search.value();
                    // if a FULL match, search must have a value AND if there is any expectation(s), all must be met
                    if node_opt.is_some_and(|node_index| {
                        self.nodes.get(*node_index).is_some_and(|node| {
                            if node.post_match_validation(ch) {
                                candidate = Some((
                                    start_index..start_index + self.search.prefix_len(),
                                    node.value,
                                ));
                                return true;
                            }
                            false
                        })
                    }) {
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
            let node_index = self.search.value()?;
            let node = self.nodes.get(*node_index)?;
            // At this point, we started a match BUT could NOT finish it (search.peek() didn't hit None),
            // at best, it's a full match BUT expectation cannot be met since there is NO preceding character
            // TODO: this can be proplematic after, thus we need to be able to control this behavior
            if node.post_match_condition.is_none() {
                candidate = Some((first_ix_of_match?..self.length, node.value));
            }
        }

        candidate
    }
}

pub struct TokenTrie<'a, V> {
    trie: Trie<char, usize>,
    nodes: Box<[Node<'a, V>]>,
}
pub struct TokenTrieIterator<'i, V>(FirstMatchFinder<'i, V>, &'i str);

impl<'i, V: Debug> TokenTrie<'i, V> {
    fn first_match(&'i self, program: &'i str) -> impl Iterator<Item = MatchResult<'i, V>> {
        TokenTrieIterator(
            FirstMatchFinder::new(program, self.trie.inc_search(), self.nodes.as_ref()),
            program,
        )
    }
}

impl<'i, V: 'i + Debug> Iterator for TokenTrieIterator<'i, V> {
    type Item = MatchResult<'i, V>;
    fn next(&mut self) -> Option<Self::Item> {
        let (range, value) = self.0.next()?;
        Some(match_result_with(self.1, range, value))
    }
}

pub struct TokeningTrieBuilder<'t, T> {
    trie_builder: TrieBuilder<char, usize>,
    nodes: Vec<Node<'t, T>>,
}

impl<'t, T> TokeningTrieBuilder<'t, T> {
    pub fn new() -> Self {
        Self {
            trie_builder: TrieBuilder::<char, usize>::new(),
            nodes: vec![],
        }
    }
    pub fn insert(&mut self, key: &'t str, value: &'t T) {
        let index_to_push = self.nodes.len();
        let mut node = Node::new(value);
        self.nodes.push(node);
        let chars: Vec<char> = key.chars().into_iter().collect();
        self.trie_builder.insert(chars, index_to_push);
    }

    pub fn insert_with(&mut self, key: &'t str, value: &'t T, with: impl SelfInjecting<'t, T>) {
        let index_to_push = self.nodes.len();
        let mut node = Node::new(value);
        node = with.inject_into(node);
        self.nodes.push(node);
        let chars: Vec<char> = key.chars().into_iter().collect();
        self.trie_builder.insert(chars, index_to_push);
    }

    pub fn build(self) -> TokenTrie<'t, T> {
        let trie = self.trie_builder.build();
        TokenTrie {
            trie: trie,
            nodes: self.nodes.into_boxed_slice(),
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
            to_insert_first: Vec<(&'t str, PostMatchCondition)>,
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
            to_insert_first: to_insert_first
                .iter()
                .map(|s| (*s, has_space_at_the_end as PostMatchCondition))
                .collect(),
            expected_match_results,
        }
    }

    macro_rules! trie_tests {
    ($($name:ident: $value:expr,)*) => {
    $(
        #[test]
        fn $name() {
            let method_to_call = $value;
            let mut ttb : TokeningTrieBuilder<bool> = TokeningTrieBuilder::new();

            match method_to_call {
                super::tests::Call::Insert{to_insert}  => {
                    for i in to_insert {
                        ttb.insert(i, &true);
                    }
                    let trie = ttb.build();
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
                super::tests::Call::FirstMatchIf{to_insert_first, expected_match_results}  => {
                    for i in to_insert_first {
                        ttb.insert_with(i.0, &true, i.1);
                    }
                    let trie = ttb.build();
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
