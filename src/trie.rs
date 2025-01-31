use trie_rs::map::{Trie, TrieBuilder};

use crate::{match_finder::*, match_validator::*, validator::ToValidate};

use std::{fmt::Debug, iter::Peekable, ops::Range};

#[derive(Debug)]
pub struct TrieValue<Value> {
    value: Value,
    to_validate: Option<ToValidate<usize>>,
}

impl<V> TrieValue<V> {
    pub fn new(value: V) -> Self {
        Self {
            value: value,
            to_validate: None,
        }
    }
}

impl<V> HasValidationComponent<usize> for TrieValue<V> {
    fn get_validation_component(&self) -> Option<&ToValidate<usize>> {
        self.to_validate.as_ref()
    }
}

impl<V> HasValue<V> for TrieValue<V> {
    fn get_value(&self) -> &V {
        &self.value
    }
}

#[derive(Debug, PartialEq)]
pub enum MatchResult<'m, V> {
    Unmatched(Range<usize>),
    Full(Range<usize>, &'m V),
}

fn create_match_result<'m, V>(a: Answer<'m, V>) -> MatchResult<'m, V> {
    let range = a.0;
    match a.1 {
        Some(val) => MatchResult::Full(range, val),
        None => MatchResult::Unmatched(range),
    }
}

#[derive(Debug)]
pub struct TokenTrie<Value> {
    trie: Trie<char, TrieValue<Value>>,
}
pub struct TokenTrieIterator<'i, V: Debug>(
    FirstMatchFinder<'i, TrieValue<V>, V, ArenaMatchValidator>,
);

pub type MatchingPeekable<'i, V> = Peekable<TokenTrieIterator<'i, V>>;

impl<'i, Value> TokenTrie<Value>
where
    Value: 'i + Debug,
{
    pub fn first_match(
        &'i self,
        program: &'i str,
        match_validator: ArenaMatchValidator,
    ) -> MatchingPeekable<'i, Value> {
        TokenTrieIterator(FirstMatchFinder::new(program, &self.trie, match_validator)).peekable()
    }
}

impl<'i, V: 'i + Debug> Iterator for TokenTrieIterator<'i, V> {
    type Item = MatchResult<'i, V>;
    fn next(&mut self) -> Option<Self::Item> {
        let result = create_match_result(self.0.next()?);
        Some(result)
    }
}

#[derive(Debug)]
pub struct TokeningTrieBuilder<Value> {
    trie_builder: TrieBuilder<char, TrieValue<Value>>,
}

impl<'t, Value: Debug> TokeningTrieBuilder<Value> {
    pub fn new() -> Self {
        Self {
            trie_builder: TrieBuilder::<char, TrieValue<Value>>::new(),
        }
    }
    fn validate_str_slice(s: &'t str) {
        assert!(s.len() > 0);
    }
    pub fn insert(&mut self, key: &'t str, value: Value) {
        Self::validate_str_slice(key);
        let trie_value = TrieValue::new(value);
        let chars: Vec<char> = key.chars().into_iter().collect();
        self.trie_builder.insert(chars, trie_value);
    }

    pub fn insert_with(&mut self, key: &'t str, value: Value, with: ToValidate<usize>) {
        Self::validate_str_slice(key);
        let mut trie_value = TrieValue::new(value);
        trie_value.to_validate = Some(with);
        let chars: Vec<char> = key.chars().into_iter().collect();
        self.trie_builder.insert(chars, trie_value);
    }

    pub fn build(self) -> TokenTrie<Value> {
        let trie = self.trie_builder.build();
        TokenTrie { trie: trie }
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::match_validator::tests::*;
    use crate::validator::{ToValidate, ValidatorChain};
    use parameterized_test::create;

    fn create_to_validate(
        pmc: Option<ValidatorChain<usize>>,
        me: Option<ValidatorChain<usize>>,
    ) -> Option<ToValidate<usize>> {
        let mut to_validate = ToValidate::default();
        to_validate.post_match_condition = pmc;
        to_validate.match_extender = me;
        Some(to_validate)
    }

    fn validation_tuple(
        me: OptTupleForValChain,
        pmc: OptTupleForValChain,
    ) -> Option<(OptTupleForValChain, OptTupleForValChain)> {
        Some((me, pmc))
    }

    pub type OptTupleForValChain = Option<(ValChainFunc, Vec<usize>)>;
    const NONE_TUPLE: Option<(OptTupleForValChain, OptTupleForValChain)> = None;
    const NONE_ME: OptTupleForValChain = None;
    const NONE_PMC: OptTupleForValChain = None;

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
        first_match_tests_with_insert, (to_insert_with_me_pmc_tuples, expected_match_results), {
            let mut ttb = TokeningTrieBuilder::new();
            let mut match_validator = ArenaMatchValidator::new(&VALIDATORS);

            for (i, me_pmc_tuple) in to_insert_with_me_pmc_tuples {
                if me_pmc_tuple.is_none() {
                    ttb.insert(i, true);
                    continue;
                }
                let (match_extender_tuple, post_match_condition_tuple)= me_pmc_tuple.clone().unwrap();

                let match_extender = match match_extender_tuple {
                    None => None,
                    Some((match_extender_f, me_ix_list)) => {
                        match_extender_f(&mut match_validator, me_ix_list)
                    }
                };
                let post_match_condition = match post_match_condition_tuple {
                    None => None,
                    Some((post_match_condition_f, pmc_ix_list)) => {
                        post_match_condition_f(&mut match_validator, pmc_ix_list)
                    }
                };

                let to_validate = create_to_validate(post_match_condition, match_extender);
                ttb.insert_with(i, true, to_validate.unwrap());

            }

            let trie = ttb.build();
            let p = expected_match_results.0.char_indices();
            let mut matches = trie.first_match(expected_match_results.0, match_validator);

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
        let mut ttb: TokeningTrieBuilder<bool> = TokeningTrieBuilder::new();
        ttb.insert("", true);
    }

    #[test]
    #[should_panic]
    fn try_to_insert_empty_string_with_validation() {
        let mut ttb: TokeningTrieBuilder<bool> = TokeningTrieBuilder::new();
        ttb.insert_with(
            "",
            true,
            ToValidate::with_post_match_condition(ValidatorChain::Once(0)),
        );
    }

    insert_tests! {
        insert_impl_fn_struct_for_i_i16_i32_and_space: &["impl", "fn", "struct", "for", "i", "i16", "i32", " "],
    }

    first_match_tests_with_insert! {
        first_match_empty_string: (&[ ("=", NONE_TUPLE)], ("",&[None])),
        first_match_at_the_end: (
            &[
                (" ", NONE_TUPLE),
                ("+", NONE_TUPLE),
                (";", NONE_TUPLE)
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
                (" ", NONE_TUPLE),
                ("  ", NONE_TUPLE)
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
                ("a", NONE_TUPLE),
                ("bb", NONE_TUPLE),
                ("ba", NONE_TUPLE)
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
                ("ba", NONE_TUPLE),
                ("bb", NONE_TUPLE),
                ("a", NONE_TUPLE)
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
                ("=", validation_tuple(NONE_ME, Some((once_validator_chain, vec![IS_SPACE])))),
                (";", validation_tuple(NONE_ME, Some((once_validator_chain, vec![IS_SPACE])))),
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
                (" ", validation_tuple(Some((once_validator_chain, vec![IS_SPACE])), None)),
                ("  ", validation_tuple(Some((once_validator_chain, vec![IS_NEWLINE])), None)),
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
                (" ", validation_tuple(Some((once_validator_chain, vec![IS_SPACE])), None)),
                ("  ", validation_tuple(Some((once_validator_chain, vec![IS_SPACE])), None)),
            ],
            (   "   ",
                &[
                    Some(MatchResult::Full(0..3, &true)), // "  "
                ])
            ),

        first_match_extended_spaces_are_extended:
            (
                &[
                    (" ", validation_tuple(Some((cycled_validator_chain, vec![IS_SPACE])), NONE_PMC)),
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
                // with the ValidatorChain, we consume the post match conditions now
                (" ", validation_tuple( NONE_ME, Some((once_validator_chain, vec![IS_SPACE])) )),
                ("trait", validation_tuple( NONE_ME, Some((once_validator_chain, vec![IS_SPACE])))),
                ("impl", validation_tuple( Some((once_validator_chain, vec![IS_SPACE])), NONE_PMC )),
                (":", validation_tuple( Some((once_validator_chain, vec![IS_DOUBLE_COLONS])), NONE_PMC )),
            ],
            (   "  trait S: impl Trait::Method",
                &[
                    Some(MatchResult::Full(0..2, &true)), // " "
                    Some(MatchResult::Full(2..8, &true)), // "trait"
                    Some(MatchResult::Unmatched(8..9)), // "S"
                    Some(MatchResult::Full(9..10, &true)), // ":"
                    Some(MatchResult::Unmatched(10..11)), // " "
                    Some(MatchResult::Full(11..16, &true)), // "impl "
                    Some(MatchResult::Unmatched(16..21)), // "Trait"
                    Some(MatchResult::Full(21..23, &true)), // "::"
                    Some(MatchResult::Unmatched(23..29)), // "Method"
                ]
            )
        ),

        first_match_rust_expression: (
             vec![
                (" ", validation_tuple( Some((once_validator_chain, vec![IS_SPACE])), NONE_PMC )),
                ("\n", validation_tuple( NONE_ME, Some((once_validator_chain, vec![IS_NEWLINE])) )),
                ("let", validation_tuple( Some((once_validator_chain, vec![IS_SPACE])), NONE_PMC) ),
                ("=", NONE_TUPLE ),
                (";", validation_tuple( Some((once_validator_chain, vec![IS_SPACE])), NONE_PMC) ),
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
                ("まだ", validation_tuple(
                        Some((once_validator_chain, vec![IS_MA, IS_DA])),
                        Some((once_validator_chain, vec![IS_SPACE, IS_DE, IS_SU])))
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
