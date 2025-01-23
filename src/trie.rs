use trie_rs::{
    inc_search::{Answer, IncSearch},
    map::{Trie, TrieBuilder},
};

use std::{
    fmt::Debug,
    iter::{Enumerate, Peekable},
    ops::Range,
    str::Chars,
};

type Validator = fn(ch: char) -> bool;

enum ValidatorType {
    PostMatchCondition(Validator),
    MatchExtender(Validator),
}

trait SelfInjecting<'i, V> {
    fn inject_into(self, node: Node<'i, V>) -> Node<'i, V>;
}

impl<'p, V> SelfInjecting<'p, V> for ValidatorType {
    fn inject_into(self, mut node: Node<'p, V>) -> Node<'p, V> {
        match self {
            ValidatorType::PostMatchCondition(pmc) => {
                node.post_match_condition = Some(pmc);
            }
            ValidatorType::MatchExtender(me) => {
                node.match_extender = Some(me);
            }
        }
        node
    }
}

pub struct Node<'v, V> {
    value: &'v V,
    post_match_condition: Option<Validator>,
    match_extender: Option<Validator>,
}

impl<'v, V> Node<'v, V> {
    pub fn new(value: &'v V) -> Self {
        Self {
            value: value,
            post_match_condition: None,
            match_extender: None,
        }
    }

    fn validate(&self, ch: char, validator: Option<Validator>) -> bool {
        match validator {
            Some(f) => f(ch),
            None => true,
        }
    }
    pub fn post_match_validation(&self, ch: char) -> bool {
        self.validate(ch, self.post_match_condition)
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

    fn peek(&mut self) -> Option<(usize, char)> {
        self.program.peek().map(|ix_and_ch_tuple| *ix_and_ch_tuple)
    }

    fn consume(&mut self) -> Option<(usize, char)> {
        self.program.next()
    }

    fn does_trie_node_have_branch_to(&mut self, ch: char) -> bool {
        self.search.peek(&ch).is_some()
    }

    fn proceed_on_trie_towards(&mut self, ch: char) -> Option<Answer> {
        self.search.query(&ch)
    }

    fn get_current_node(&self) -> Option<&'m Node<'m, V>> {
        // if a FULL match, search must have a value
        let node_ix = self.search.value()?;
        self.nodes.get(*node_ix)
    }

    fn move_back_to_root_of_the_trie(&mut self) {
        self.search.reset();
    }

    fn process_index_for_node(
        &mut self,
        mut unmatched_ix: usize,
        mut unmatched_ch: char,
    ) -> Option<(usize, &'m V)> {
        let node = self.get_current_node()?;

        let mut consumed_all = false;

        if let Some(match_extender) = node.match_extender {
            loop {
                (unmatched_ix, unmatched_ch) = match self.peek() {
                    Some(ixch) => ixch,
                    None => {
                        consumed_all = true;
                        (self.length, ' ')
                    }
                };
                if consumed_all {
                    break;
                }
                if match_extender(unmatched_ch) {
                    self.consume();
                } else {
                    break;
                }
            }
        }

        // At this point, we started a match BUT could NOT finish it either because:
        //  i. search.peek() didn't hit None, or ii. we extended the match all the way to the end
        // it's a full match (we have a value) BUT can't validate post-match since
        // NO more characters left preceeding the match
        let result = (unmatched_ix, node.value);
        if consumed_all {
            Some(result)
        } else if node.post_match_validation(unmatched_ch) {
            Some(result)
        } else {
            None
        }
    }
}

type FirstMatch<'m, V> = (Range<usize>, &'m V);

impl<'m, V> Iterator for FirstMatchFinder<'m, V> {
    type Item = FirstMatch<'m, V>;

    fn next(&mut self) -> Option<Self::Item> {
        self.search.reset();

        let mut first_ix_of_match: Option<usize> = None;
        let mut candidate: Option<Self::Item> = None;
        let mut consume_program_char;
        let mut query_the_char;

        while let Some((index, ch)) = self.peek() {
            (consume_program_char, query_the_char) = (true, true);

            match (first_ix_of_match, self.does_trie_node_have_branch_to(ch)) {
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
                    match self.process_index_for_node(index, ch) {
                        // if there are is post match validation or/and match extension, all must performed and valid
                        Some((end_index, value)) => {
                            candidate = Some((start_index..end_index, value));
                            break;
                        }
                        None => {}
                    }
                    // have a partial match, OR if any, min. one expectation failed
                    // thus search needs to be reset to 'forget' the past
                    self.move_back_to_root_of_the_trie();
                    first_ix_of_match = None;
                    // since next search query does NOT match, disable the query
                    query_the_char = false;
                }
                // started a match on the trie and next search query ALSO matches
                // thus the peeked program char needs to be consumed, search must be queried to move on the trie
                (Some(_), true) => { /* no need to do anything */ }
            }
            if query_the_char {
                self.proceed_on_trie_towards(ch);
            }
            if consume_program_char {
                self.consume();
            }
        }

        let no_candidate_from_loop = candidate.is_none();
        if no_candidate_from_loop {
            let node = self.get_current_node()?;
            // At this point, we started a match BUT could NOT finish it either because:
            //  i. search.peek() didn't hit None, or ii. we extended the match all the way to the end
            // it's a full match (we have a value) BUT can't validate post-match since
            // NO more characters left preceeding the match
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
        FirstMatchWithValidators {
            to_insert_first: Vec<(&'t str, ValidatorType)>,
            expected_match_results: (&'t str, &'t [Option<MatchResult<'t, bool>>]),
        },
    }

    fn has_space_at_the_end(ch: char) -> bool {
        ch == ' '
    }

    fn does_not_have_colon(ch: char) -> bool {
        ch != ':'
    }

    fn first_match_if_space_follows<'t>(
        to_insert_first: &'t [&'t str],
        expected_match_results: (&'t str, &'t [Option<MatchResult<'t, bool>>]),
    ) -> Call<'t> {
        Call::FirstMatchWithValidators {
            to_insert_first: to_insert_first
                .iter()
                .map(|s| (*s, ValidatorType::PostMatchCondition(has_space_at_the_end)))
                .collect(),
            expected_match_results,
        }
    }

    fn extend_to_white_space(ch: char) -> bool {
        ch.is_whitespace()
    }

    fn extend_to_newline(ch: char) -> bool {
        ch == '\n'
    }

    fn extend_to_space(ch: char) -> bool {
        ch == ' '
    }

    fn first_match_if_extend_to_following_whitespaces<'t>(
        to_insert_first: &'t [&'t str],
        expected_match_results: (&'t str, &'t [Option<MatchResult<'t, bool>>]),
    ) -> Call<'t> {
        Call::FirstMatchWithValidators {
            to_insert_first: to_insert_first
                .iter()
                .map(|s| (*s, ValidatorType::MatchExtender(extend_to_white_space)))
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

            let expected_match_results = match method_to_call {
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
                    return;
                },
                super::tests::Call::FirstMatch{to_insert_first,expected_match_results}  => {
                    for i in to_insert_first {
                        ttb.insert(i, &true);
                    }
                    expected_match_results
                },
                super::tests::Call::FirstMatchWithValidators{to_insert_first, expected_match_results}  => {
                    for i in to_insert_first {
                        ttb.insert_with(i.0, &true, i.1);
                    }
                    expected_match_results
                },
            };
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

        first_match_if_space_is_not_extended:
        first_match_if_extend_to_following_whitespaces(
            &[" "],
            ("let var", &[Some(MatchResult(3..4, &true, " "))])
        ),

        first_match_if_spaces_are_extended:
            first_match_if_extend_to_following_whitespaces(
                &[" "],
                ("   three_each   ",
                &[
                    Some(MatchResult(0..3, &true, "   ")),
                    Some(MatchResult(13..16, &true, "   ")),
                ])
            ),

        first_match_if_realistic:
            Call::FirstMatchWithValidators {
            to_insert_first : vec![
                (" ", ValidatorType::MatchExtender(extend_to_space)),
                ("\n", ValidatorType::MatchExtender(extend_to_newline)),
                ("trait", ValidatorType::PostMatchCondition(has_space_at_the_end)),
                ("impl", ValidatorType::PostMatchCondition(has_space_at_the_end)),
                (":", ValidatorType::PostMatchCondition(does_not_have_colon)),
            ],
            expected_match_results :
                ("  trait S: impl Trait::Method",
                &[
                    Some(MatchResult(0..2, &true, "  ")),
                    Some(MatchResult(2..7, &true, "trait")),
                    Some(MatchResult(7..8, &true, " ")),
                    Some(MatchResult(9..10, &true, ":")),
                    Some(MatchResult(10..11, &true, " ")),
                    Some(MatchResult(11..15, &true, "impl")),
                    Some(MatchResult(15..16, &true, " ")),
                ]),
            },

    }
}
