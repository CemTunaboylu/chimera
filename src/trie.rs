use trie_rs::{
    inc_search::IncSearch,
    map::{Trie, TrieBuilder},
};

use crate::{match_validator::*, program_text, range_indices::RangeIndices};

use std::{fmt::Debug, iter::Peekable, marker::PhantomData, ops::Range};

#[derive(Debug)]
pub struct TrieValue<Value> {
    value: Value,
    to_validate: Option<ToValidate>,
}

impl<V> TrieValue<V> {
    pub fn new(value: V) -> Self {
        Self {
            value: value,
            to_validate: None,
        }
    }
}

type TrieSearch<'s, V> = IncSearch<'s, char, TrieValue<V>>;
type InnerTrie<V> = Trie<char, TrieValue<V>>;
struct TrieNavigator<'t, V> {
    search: TrieSearch<'t, V>,
}

impl<'t, V> TrieNavigator<'t, V> {
    pub fn new(trie: &'t InnerTrie<V>) -> Self {
        Self {
            search: trie.inc_search(),
        }
    }
    pub fn took_the_branch_of(&mut self, ch: char) -> bool {
        self.search.query(&ch).is_some()
    }

    pub fn move_back_to_root(&mut self) {
        self.search.reset();
    }

    pub fn current_value(&self) -> Option<&'t TrieValue<V>> {
        self.search.value()
    }
}

#[derive(Debug, PartialEq)]
pub enum MatchResult<'m, V> {
    Unmatched(Range<usize>),
    Full(Range<usize>, &'m V),
}

type Answer<'m, V> = (Range<usize>, Option<&'m V>);

fn create_match_result<'m, V>(a: Answer<'m, V>) -> MatchResult<'m, V> {
    let range = a.0;
    match a.1 {
        Some(val) => MatchResult::Full(range, val),
        None => MatchResult::Unmatched(range),
    }
}

struct FirstMatchFinder<'m, V, MV: MatchValidator> {
    program: program_text::Program<'m>,
    trie_navigator: TrieNavigator<'m, V>,
    indices: RangeIndices,
    __match_validator: PhantomData<MV>,
    saved: Option<Answer<'m, V>>,
}

impl<'m, V, MV: MatchValidator> FirstMatchFinder<'m, V, MV> {
    pub fn new(program: &'m str, trie: &'m InnerTrie<V>) -> Self {
        Self {
            program: program_text::Program::new(program),
            trie_navigator: TrieNavigator::new(trie),
            indices: RangeIndices::new(),
            __match_validator: PhantomData,
            saved: None,
        }
    }

    fn validate_match_result(&mut self, mut next_ch: char) -> Option<&'m V> {
        let trie_value = self.trie_navigator.current_value()?;

        let mut consumed_all = false;
        // Note: for Chimera, there is no case where match is extended iff
        // post_match_condition is met thus the greed and simplicity of the consume_while
        if let Some(extended_index) =
            MV::extend_match(trie_value.to_validate.as_ref(), &mut self.program)
        {
            if self.program.at_end() {
                consumed_all = true;
                self.indices.end_match(self.program.len());
            } else {
                self.indices.end_match(extended_index);
                next_ch = self.program.peek().unwrap().1;
            }
        }
        // At this point, we started a match BUT could NOT finish it either because:
        //  i. search.peek() didn't hit None, or ii. we extended the match all the way to the end
        // it's a full match (we have a value) BUT can't validate post-match since
        // NO more characters left preceeding the match
        let result = &trie_value.value;
        if consumed_all {
            Some(result)
        } else if MV::is_match_valid(trie_value.to_validate.as_ref(), Some(next_ch)) {
            Some(result)
        } else {
            None
        }
    }

    fn process_match_candidate(
        &mut self,
        peeked_index: usize,
        peeked_ch: char,
    ) -> Option<Answer<'m, V>> {
        let mut candidate = None;
        self.indices.end_match(peeked_index);
        match self.validate_match_result(peeked_ch) {
            // if there are is post match validation or/and match extension, all must performed and valid
            Some(value) => {
                let answer = (self.indices.form_answer_range(), Some(value));
                // check if there is an unmatched str slice before this full match
                let fully_matched = Some(answer);
                if self.indices.has_an_unmatched_slice_before() {
                    // case: |<-- unmatched -->||<--full match-->|...
                    // save the candidate as the next result to be directly returned
                    self.saved = fully_matched;
                    candidate = Some((self.indices.form_unmatched_range(), None))
                    // current state: candidate: |<-- unmatched -->| , saved: |<--full match-->|
                } else {
                    candidate = fully_matched;
                }
            }
            None => {
                self.indices.delete_match();
            }
        }
        candidate
    }

    fn generate_result_for_no_candidate(&mut self) -> Option<Answer<'m, V>> {
        let prog_len = self.program.len();
        self.indices.end_match(prog_len);

        match self.trie_navigator.current_value() {
            None => Some((self.indices.form_unmatched_range(), None)),
            Some(trie_value) => {
                // At this point, we started a match BUT could NOT finish it either because:
                //  i. search.peek() didn't hit None, or ii. we extended the match all the way to the end
                // it's a full match (we have a value) BUT can't validate post-match since
                // NO more characters left preceeding the match
                let fully_matched =
                    Some((self.indices.form_answer_range(), Some(&trie_value.value)));
                if self.indices.has_an_unmatched_slice_before() {
                    self.saved = fully_matched;
                    Some((self.indices.form_unmatched_range(), None))
                } else {
                    fully_matched
                }
            }
        }
    }
}

impl<'m, V: Debug, MV: MatchValidator> Iterator for FirstMatchFinder<'m, V, MV> {
    type Item = Answer<'m, V>;

    // From the root of the trie, and the first character of the remaining program, we peek the first char and index.
    // If, from the current trie node, we have a branch to that char, it means we started a match. We record this index as
    // the starting index of the match, consume the char of the program and move to that branch of the trie.
    // We repeat the above steps until a. no more chars to look at, b. we have an unmatching char i.e. we don't have a branch
    // that follows this char from the current node of the trie.
    //  a.  We exit the loop, check if the node we currently are at is a match by grabbing its value. If we cannot grab a value, it means
    //      we could find any matces on the trie, thus we return an unmatch as the Answer.
    //  b.  We check if the node we currently are at is a match by grabbing its value.
    //          - If we cannot grab a value, it means
    //              we could not find a match, thus we erase the start index that we saved earlier, move back to the root of the trie
    //              and signal to the end of the loop that we don't want to consume this char, since we could not make use of it at this node.
    //              We need to check again from the root since it well can be a matching char.
    //          - If we can grab a value, we have strong candidate for a match. We first try to extend the match wrt. to the extension validator
    //              on the node. As long as we extend, we consume the program. At the end of the extension, if we have consumed all chars meaning that
    //              a post match condition cannot be even checked, we simply return the result we have found which is the new index that is moved to
    //              to the extended index and the value of the node. If we can check on post match condition, we do and if it is ok, we return the result
    //              else we return None.
    //              If we did'nt return a result, we perform the same steps above, clean up and move back to the root of the trie.
    //              If we returned a result, we additionally check if we have an unmatched chunk of chars before this match. If not, we return the result.
    //                  If there is an unmatched chunk, we save this fresh result, and first return the unmatched chunk as an unmatch. We perform this step
    //                  at this point, because we could not have know while starting a match if it will end up as a match.
    //
    fn next(&mut self) -> Option<Self::Item> {
        self.trie_navigator.move_back_to_root();

        if self.saved.is_some() {
            return self.saved.take();
        }

        let mut candidate: Option<Self::Item> = None;

        self.indices.start_peeking(self.program.peek()?.0);

        while let Some((index, ch)) = self.program.peek() {
            let (mut move_back_to_root, mut consume_program_char) = (true, true);
            match (
                self.indices.has_a_match_started(),
                self.trie_navigator.took_the_branch_of(ch),
            ) {
                // haven't seen any matched chars and next search query also does NOT match
                (false, false) => {}
                // haven't seen any matched chars BUT next search query matches, i.e. a match is starting
                (false, true) => {
                    self.indices.start_match(index);
                    move_back_to_root = false;
                }
                // have started a match on the trie BUT next search query does NOT match, i.e. match is ending
                (true, false) => {
                    match self.process_match_candidate(index, ch) {
                        Some(answer) => {
                            candidate = Some(answer);
                            break;
                        }
                        None => {}
                    }
                    // have a partial match, OR if any, min. one expectation failed
                    // thus search needs to be reset to 'forget' the past
                    // since next search query does NOT match, and we can still find a match with this char
                    consume_program_char = false;
                }
                // started a match on the trie and next search query ALSO matches
                // thus the peeked program char needs to be consumed, search must be queried to move on the trie
                (true, true) => {
                    move_back_to_root = false;
                }
            }
            if move_back_to_root {
                self.trie_navigator.move_back_to_root();
            }
            if consume_program_char {
                self.program.consume();
            }
        }

        let found_a_candidate_in_loop = candidate.is_some();
        if found_a_candidate_in_loop {
            return candidate;
        } else {
            candidate = self.generate_result_for_no_candidate();
        }
        candidate
    }
}

#[derive(Debug)]
pub struct TokenTrie<V> {
    trie: InnerTrie<V>,
}
pub struct TokenTrieIterator<'i, V>(FirstMatchFinder<'i, V, LazyMatchValidator>);
pub type MatchingPeekable<'i, V> = Peekable<TokenTrieIterator<'i, V>>;

impl<'i, V: 'i + Debug> TokenTrie<V> {
    pub fn first_match(&'i self, program: &'i str) -> MatchingPeekable<'i, V> {
        TokenTrieIterator(FirstMatchFinder::new(program, &self.trie)).peekable()
    }
}

impl<'i, V: 'i + Debug> Iterator for TokenTrieIterator<'i, V> {
    type Item = MatchResult<'i, V>;
    fn next(&mut self) -> Option<Self::Item> {
        let result = create_match_result(self.0.next()?);
        Some(result)
    }
}

pub struct TokeningTrieBuilder<T> {
    trie_builder: TrieBuilder<char, TrieValue<T>>,
}

impl<'t, T> TokeningTrieBuilder<T> {
    pub fn new() -> Self {
        Self {
            trie_builder: TrieBuilder::<char, TrieValue<T>>::new(),
        }
    }
    fn validate_str_slice(s: &'t str) {
        assert!(s.len() > 0);
    }
    pub fn insert(&mut self, key: &'t str, value: T) {
        Self::validate_str_slice(key);
        let trie_value = TrieValue::new(value);
        let chars: Vec<char> = key.chars().into_iter().collect();
        self.trie_builder.insert(chars, trie_value);
    }

    pub fn insert_with(&mut self, key: &'t str, value: T, with: ToValidate) {
        Self::validate_str_slice(key);
        let mut trie_value = TrieValue::new(value);
        trie_value.to_validate = Some(with);
        let chars: Vec<char> = key.chars().into_iter().collect();
        self.trie_builder.insert(chars, trie_value);
    }

    pub fn build(self) -> TokenTrie<T> {
        let trie = self.trie_builder.build();
        TokenTrie { trie: trie }
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use parameterized_test::create;

    fn is_space(ch: &char) -> bool {
        *ch == ' '
    }

    fn is_not_colon(ch: &char) -> bool {
        *ch != ':'
    }

    fn is_newline(ch: &char) -> bool {
        *ch == '\n'
    }

    fn full(r: Range<usize>, b: &'static bool) -> MatchResult<'static, bool> {
        MatchResult::Full(r, b)
    }

    fn unmatched(r: Range<usize>) -> MatchResult<'static, bool> {
        MatchResult::Unmatched(r)
    }

    create! {
        insert_tests, (to_insert), {
                let mut ttb : TokeningTrieBuilder<usize> = TokeningTrieBuilder::new();
                    for (v,l) in to_insert.into_iter().enumerate() {
                        ttb.insert(l, v);
                    }
                    let trie = ttb.build();
                    for (x, i) in to_insert.into_iter().enumerate() {
                        let chars: Vec<char> = i.chars().into_iter().collect();
                        let r = trie.trie.exact_match(chars);
                        assert_eq!(true, r.is_some());
                        assert_eq!(x, r.unwrap().value);
                    }
        }
    }

    create! {
        first_match_tests_with_insert, (to_insert_with, expected_match_results), {
            let mut ttb : TokeningTrieBuilder<bool> = TokeningTrieBuilder::new();
            for (i, v) in to_insert_with {
                if v.is_some() {
                    ttb.insert_with(i, true, v.clone().unwrap());
                } else {
                    ttb.insert(i, true);
                }
            }
            let trie = ttb.build();
            let p = expected_match_results.0.char_indices();
            let mut matches = trie.first_match(expected_match_results.0);

            for expected in expected_match_results.1.into_iter() {
                let item = matches.next();
                assert_eq!(*expected, item);
            }
        }
    }

    #[test]
    #[should_panic]
    fn try_to_insert_empty_string() {
        let mut ttb: TokeningTrieBuilder<bool> = TokeningTrieBuilder::new();
        ttb.insert("", true);
    }

    #[test]
    #[should_panic]
    fn try_to_insert_empty_string_with_validation() {
        let mut ttb: TokeningTrieBuilder<bool> = TokeningTrieBuilder::new();
        ttb.insert_with("", true, ToValidate::with_post_match_condition(is_space));
    }

    insert_tests! {
        insert_impl_fn_struct_for_i_i16_i32_and_space: &["impl", "fn", "struct", "for", "i", "i16", "i32", " "],
    }
    first_match_tests_with_insert! {
        first_match_empty_string: (&[ ("=", None)], ("",&[ None])),
        first_match_at_the_end: (
            &[
                (" ", None),
                ("+", None),
                (";", None)
            ],
            (   " chi+=mera;",
                &[
                    Some(full(0..1, &true)), // " "
                    Some(unmatched(1..4)), // "chi"
                    Some(full(4..5, &true)), // "+"
                    Some(unmatched(5..10)), // "=mera"
                    Some(full(10..11, &true)), // ";"
                ]
            ),
        ),

        first_match_whitespaces: (
            &[
                (" ", None),
                ("  ", None)
            ],
            (   "   ",
                &[
                    Some(full(0..2, &true)), // "  "
                    Some(full(2..3, &true)), // " "
                ]
            ),
        ),

        first_match_bbbaa: (
            &[
                ("a", None),
                ("bb", None), ("ba", None)
            ],
            (   "bbbaa",
                &[
                    Some(full(0..2, &true)), // "bb"
                    Some(full(2..4, &true)), // "ba"
                    Some(full(4..5, &true)), // "a"
                ])
        ),
        first_match_bbbaa_insertion_order_does_not_matter:(
            &[
                ("ba", None),
                ("bb", None),
                ("a", None)
            ],
            (   "bbbaa",
                &[
                    Some(full(0..2, &true)), // "bb"
                    Some(full(2..4, &true)), // "ba"
                    Some(full(4..5, &true)), // "a"
                ])
        ),
        first_match_post_match_equals_in_between_with_space: (
            &[
                ("=", Some(ToValidate::with_post_match_condition(is_space))),
                (";", Some(ToValidate::with_post_match_condition(is_space)))
            ],
            (
                "let var = 5;",
                &[
                    Some(MatchResult::Unmatched(0..8)), // "let var "
                    Some(MatchResult::Full(8..9, &true)), // "="
                    Some(MatchResult::Unmatched(9..11)), // " 5"
                    Some(MatchResult::Full(11..12, &true)), // ";"
                ]
            )
        ),

        first_match_post_match_whitespaces: (
            &[
                (" ", Some(ToValidate::with_post_match_condition(is_space))),
                ("  ", Some(ToValidate::with_post_match_condition(is_space)))
            ],
            (   "   ",
                &[
                    Some(MatchResult::Full(0..2, &true)), // "  "
                    // since its at the end, post_match_condition is not performed
                    Some(MatchResult::Full(2..3, &true)), // " "
                ])
            ),

        first_match_extended_spaces_are_extended:
            (
                &[
                    (" ", Some(ToValidate::with_match_extender(is_space))),
                ],
                ("   three_each   one ",
                &[
                    Some(MatchResult::Full(0..3, &true)), // "   "
                    Some(MatchResult::Unmatched(3..13)), // "three_each"
                    Some(MatchResult::Full(13..16, &true)), // "   "
                    Some(MatchResult::Unmatched(16..19)), // "one"
                    Some(MatchResult::Full(19..20, &true)), // " "
                ])
            ),

        first_match_extended_multiple_different: (
            vec![
                (" ", Some(ToValidate::with_match_extender(is_space))),
                ("trait", Some(ToValidate::with_post_match_condition(is_space))),
                ("impl", Some(ToValidate::with_post_match_condition(is_space))),
                (":", Some(ToValidate::with_post_match_condition(is_not_colon))),
            ],
            (   "  trait S: impl Trait::Method",
                &[
                    Some(MatchResult::Full(0..2, &true)), // "  "
                    Some(MatchResult::Full(2..7, &true)), // "trait"
                    Some(MatchResult::Full(7..8, &true)), // " "
                    Some(MatchResult::Unmatched(8..9)), // "S"
                    Some(MatchResult::Full(9..10, &true)), // ":"
                    Some(MatchResult::Full(10..11, &true)), // " "
                    Some(MatchResult::Full(11..15, &true)), // "impl"
                    Some(MatchResult::Full(15..16, &true)), // " "
                    Some(MatchResult::Unmatched(16..22)), // "Trait:"
                    Some(MatchResult::Full(22..23, &true)), // ":"
                    Some(MatchResult::Unmatched(23..29)), // "Method"
                ]
            )
        ),

        first_match_rust_expression: (
             vec![
                (" ", Some(ToValidate::with_match_extender(is_space))),
                ("\n", Some(ToValidate::with_match_extender(is_newline))),
                ("let", Some(ToValidate::with_post_match_condition(is_space))),
                ("=", Some(ToValidate::with_post_match_condition(is_space))),
                (";", Some(ToValidate::with_post_match_condition(is_space))),
            ],
            (   "let var = 2;",
                &[
                    Some(MatchResult::Full(0..3, &true)), // "let"
                    Some(MatchResult::Full(3..4, &true)), // " "
                    Some(MatchResult::Unmatched(4..7)), // "var"
                    Some(MatchResult::Full(7..8, &true)), // " "
                    Some(MatchResult::Full(8..9, &true)), // "="
                    Some(MatchResult::Full(9..10, &true)), // " "
                    Some(MatchResult::Unmatched(10..11)), // "2"
                    Some(MatchResult::Full(11..12, &true)), // ";"
                ]
            ),
            ),
        first_match_japanese: (
             vec![
                ("まだ", Some(ToValidate::with_match_extender(is_ma_or_da))),
            ],
            (
                "まだまだ です また",
                &[
                    Some(MatchResult::Full(0..12, &true)), // "まだまだ",
                    Some(MatchResult::Unmatched(12..26)), // " です また""
                ]),
        ),

    }
    fn is_ma_or_da(ch: &char) -> bool {
        *ch == 'ま' || *ch == 'だ'
    }
}
