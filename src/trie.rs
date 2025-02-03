use trie_rs::map::{Trie, TrieBuilder};

use crate::{
    match_finder::*,
    validator::{cycle, once, PostMatchAction, Validator, ValidatorChain},
};

use std::{fmt::Debug, iter::Peekable, ops::Range};

#[derive(Debug)]
pub struct TrieValue<Value> {
    pub value: Value,
    pub to_validate: PostMatchAction<usize>,
}

impl<V> TrieValue<V> {
    pub fn new(value: V) -> Self {
        Self {
            value,
            to_validate: PostMatchAction::None,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum MatchResult<'m, V> {
    Unmatched(Range<usize>),
    Full(Range<usize>, &'m V),
}

fn create_match_result<V>(a: Answer<V>) -> MatchResult<V> {
    let range = a.0;
    match a.1 {
        Some(val) => MatchResult::Full(range, val),
        None => MatchResult::Unmatched(range),
    }
}

#[derive(Debug)]
pub struct ValidatingTrie<Value> {
    trie: Trie<char, TrieValue<Value>>,
    validators: &'static [Validator],
    chain_indices: Vec<Vec<usize>>,
}
pub struct ValidatingTrieIterator<'i, V: Debug>(FirstMatchFinder<'i, V>);
pub type PeekableValidatingMatchFinder<'i, V> = Peekable<ValidatingTrieIterator<'i, V>>;

impl<'i, Value> ValidatingTrie<Value>
where
    Value: 'i + Debug,
{
    pub fn first_match(&'i self, program: &'i str) -> PeekableValidatingMatchFinder<'i, Value> {
        ValidatingTrieIterator(FirstMatchFinder::new(
            program,
            &self.trie,
            self.validators,
            &self.chain_indices,
        ))
        .peekable()
    }
}

impl<'i, V: 'i + Debug> Iterator for ValidatingTrieIterator<'i, V> {
    type Item = MatchResult<'i, V>;
    fn next(&mut self) -> Option<Self::Item> {
        let result = create_match_result(self.0.next()?);
        Some(result)
    }
}

#[derive(Debug, Clone)]
pub enum List {
    Once(Vec<usize>),
    Cycled(Vec<usize>),
}
#[derive(Debug, Clone)]
pub enum ValidatorIndex {
    None,
    MatchExtender(List),
    PostMatch(List),
    Both((List, List)),
}

#[derive(Debug)]
pub struct ValidatingTrieBuilder<Value> {
    trie_builder: TrieBuilder<char, TrieValue<Value>>,
    validators: &'static [Validator],
    chain_indices: Vec<Vec<usize>>,
}

impl<'t, Value: Debug> ValidatingTrieBuilder<Value> {
    pub fn new(validators: &'static [Validator]) -> Self {
        Self {
            trie_builder: TrieBuilder::<char, TrieValue<Value>>::new(),
            validators,
            chain_indices: vec![],
        }
    }
    fn validate_str_slice(s: &'t str) {
        assert!(!s.is_empty());
    }
    fn register_index_list(&mut self, list: List) -> ValidatorChain<usize> {
        let index_pushed = self.chain_indices.len();
        let (vec, validator_chain) = match list {
            List::Once(vec) => (vec, once(index_pushed)),
            List::Cycled(vec) => (vec, cycle(index_pushed)),
        };
        self.chain_indices.push(vec);
        validator_chain
    }
    fn create_validation_from_validator_index(
        &mut self,
        v: ValidatorIndex,
    ) -> PostMatchAction<usize> {
        match v {
            ValidatorIndex::None => PostMatchAction::None,
            ValidatorIndex::MatchExtender(vec) => {
                PostMatchAction::Extend(self.register_index_list(vec))
            }
            ValidatorIndex::PostMatch(vec) => {
                PostMatchAction::Validate(self.register_index_list(vec))
            }
            ValidatorIndex::Both((me_vec, pmc_vec)) => PostMatchAction::ExtendThenValidate {
                me: self.register_index_list(me_vec),
                pmc: self.register_index_list(pmc_vec),
            },
        }
    }

    pub fn insert(&mut self, key: &'t str, value: Value, with: ValidatorIndex) {
        Self::validate_str_slice(key);
        let mut trie_value = TrieValue::new(value);
        let validate = self.create_validation_from_validator_index(with);
        trie_value.to_validate = validate;
        let chars: Vec<char> = key.chars().collect();
        self.trie_builder.insert(chars, trie_value);
    }

    pub fn build(self) -> ValidatingTrie<Value> {
        let trie = self.trie_builder.build();
        ValidatingTrie {
            trie,
            validators: self.validators,
            chain_indices: self.chain_indices,
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::match_validation::tests::*;
    use parameterized_test::create;

    create! {
        insert_tests, (to_insert), {
                let mut ttb : ValidatingTrieBuilder<usize> = ValidatingTrieBuilder::new(&VALIDATORS);
                    for (v,l) in to_insert.into_iter().enumerate() {
                        ttb.insert(l, v, ValidatorIndex::None);
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
        first_match_tests_with_insert, (to_insert_with_me_pmc_tuples, expected_match_results), {
            let mut ttb = ValidatingTrieBuilder::new(&VALIDATORS);

            for (key, validator_index) in to_insert_with_me_pmc_tuples {
                ttb.insert(key, true, validator_index.clone());
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

    fn full(r: Range<usize>, b: &'static bool) -> MatchResult<'static, bool> {
        MatchResult::Full(r, b)
    }

    fn unmatched(r: Range<usize>) -> MatchResult<'static, bool> {
        MatchResult::Unmatched(r)
    }

    #[test]
    #[should_panic]
    fn try_to_insert_empty_string() {
        let mut ttb: ValidatingTrieBuilder<bool> = ValidatingTrieBuilder::new(&VALIDATORS);
        ttb.insert("", true, ValidatorIndex::None);
    }

    #[test]
    #[should_panic]
    fn try_to_insert_empty_string_with_validation() {
        let mut ttb: ValidatingTrieBuilder<bool> = ValidatingTrieBuilder::new(&VALIDATORS);
        ttb.insert(
            "",
            true,
            ValidatorIndex::Both((List::Cycled(vec![0]), List::Once(vec![0]))),
        );
    }

    insert_tests! {
        insert_impl_fn_struct_for_i_i16_i32_and_space: &["impl", "fn", "struct", "for", "i", "i16", "i32", " "],
    }

    first_match_tests_with_insert! {
        first_match_empty_string: (&[ ("=", ValidatorIndex::None)], ("",&[None])),
        first_match_at_the_end: (
            &[
                (" ", ValidatorIndex::None),
                ("+", ValidatorIndex::None),
                (";", ValidatorIndex::None)
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
                (" ", ValidatorIndex::None),
                ("  ", ValidatorIndex::None)
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
                ("a", ValidatorIndex::None),
                ("bb", ValidatorIndex::None),
                ("ba", ValidatorIndex::None)
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
                ("ba", ValidatorIndex::None),
                ("bb", ValidatorIndex::None),
                ("a", ValidatorIndex::None)
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
                ("=", ValidatorIndex::PostMatch(List::Once(vec![IS_SPACE]))),
                (";", ValidatorIndex::PostMatch(List::Once(vec![IS_SPACE]))),
            ],
            (
                "let var = 5;",
                &[
                    Some(MatchResult::Unmatched(0..8)), // "let var "
                    Some(MatchResult::Full(8..10, &true)), // "= "
                    Some(MatchResult::Unmatched(10..11)), // "5"
                    Some(MatchResult::Full(11..12, &true)), // ";"
                ]
            )
        ),

        first_match_post_match_whitespaces: (
            &[
                (" ", ValidatorIndex::MatchExtender(List::Once(vec![IS_SPACE]))),
                ("  ", ValidatorIndex::MatchExtender(List::Once(vec![IS_NEWLINE]))),
            ],
            (   "   ",
                &[
                    Some(MatchResult::Full(0..2, &true)), // "  "
                    // since its at the end, post_match_condition is not performed
                    Some(MatchResult::Full(2..3, &true)), // " "
                ])
            ),

        first_match_post_match_whitespaces_longer_match: (
            &[
                (" ", ValidatorIndex::MatchExtender(List::Cycled(vec![IS_SPACE]))),
                ("  ", ValidatorIndex::MatchExtender(List::Once(vec![IS_SPACE]))),
            ],
            (   "   ",
                &[
                    Some(MatchResult::Full(0..3, &true)), // "  "
                ])
            ),

        first_match_extended_spaces_are_extended:
            (
                &[
                    (" ", ValidatorIndex::MatchExtender(List::Cycled(vec![IS_SPACE]))),
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
                // with the ValidatorChain, we consume the post match conditions
                (" ", ValidatorIndex::MatchExtender(List::Cycled(vec![IS_SPACE]))),
                ("trait", ValidatorIndex::PostMatch(List::Cycled(vec![IS_SPACE]))),
                ("impl", ValidatorIndex::PostMatch(List::Cycled(vec![IS_SPACE]))),
                (":", ValidatorIndex::MatchExtender(List::Once(vec![IS_DOUBLE_COLONS]))),
            ],
            (   "  trait S: impl Trait::Method",
                &[
                    Some(MatchResult::Full(0..2, &true)), // "  "
                    Some(MatchResult::Full(2..8, &true)), // "trait "
                    Some(MatchResult::Unmatched(8..9)), // "S"
                    Some(MatchResult::Full(9..10, &true)), // ":"
                    Some(MatchResult::Full(10..11, &true)), // " "
                    Some(MatchResult::Full(11..16, &true)), // "impl "
                    Some(MatchResult::Unmatched(16..21)), // "Trait"
                    Some(MatchResult::Full(21..23, &true)), // "::"
                    Some(MatchResult::Unmatched(23..29)), // "Method"
                ]
            )
        ),

        first_match_rust_expression: (
             vec![
                (" ", ValidatorIndex::MatchExtender(List::Cycled(vec![IS_SPACE]))),
                ("\n", ValidatorIndex::MatchExtender(List::Cycled(vec![IS_NEWLINE]))),
                ("let", ValidatorIndex::PostMatch(List::Cycled(vec![IS_SPACE]))),
                ("=", ValidatorIndex::None),
                (";", ValidatorIndex::PostMatch(List::Cycled(vec![IS_SPACE]))),
            ],
            (   "let var = 2;",
                &[
                    Some(MatchResult::Full(0..4, &true)), // "let "
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
                ("まだ", ValidatorIndex::Both((
                        List::Once(vec![IS_MA, IS_DA]),
                        List::Once(vec![IS_SPACE, IS_DE, IS_SU]),
                    ))
                )
                ],
            (
                "まだまだ です また",
                &[
                    Some(MatchResult::Full(0..19, &true)), // "まだまだ です"
                    Some(MatchResult::Unmatched(19..26)), // " また""
                ]),
        ),
    }
}
