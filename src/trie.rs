use trie_rs::{
    inc_search::Answer,
    map::{Trie, TrieBuilder},
};

pub struct TokenTrie<'t, T> {
    trie: Trie<char, usize>,
    values: Box<[&'t T]>,
}

impl<'t, T> TokenTrie<'t, T> {
    pub fn matches(&self, k: &'t str) -> Result<&T, usize> {
        let chars: Vec<char> = k.chars().into_iter().collect();
        let mut search = self.trie.inc_search();
        let query_result = search.query_until(chars);

        let answer = match query_result {
            Err(first_unmatch) => return Result::Err(first_unmatch),
            Ok(i) => i,
        };

        let prefix_length = search.prefix_len();

        let result = match answer {
            Answer::Prefix => Result::Err(prefix_length),
            Answer::Match => {
                let index = search.value().unwrap();
                Result::Ok(
                    self.values
                        .get(*index)
                        .map(|opt_ref_ref| *opt_ref_ref)
                        .unwrap(),
                )
            }
            Answer::PrefixAndMatch => Result::Err(prefix_length),
        };
        return result;
    }
}

pub struct TokenTrieBuilder<'t, T> {
    trie_builder: TrieBuilder<char, usize>,
    values: Vec<&'t T>,
}

impl<'t, T> TokenTrieBuilder<'t, T> {
    pub fn new() -> Self {
        let trie_builder = TrieBuilder::<char, usize>::new();
        Self {
            trie_builder: trie_builder,
            values: vec![],
        }
    }
    pub fn insert(&mut self, key: &'t str, value: &'t T) {
        let index_to_push = self.values.len();
        self.values.push(value);
        let chars: Vec<char> = key.chars().into_iter().collect();
        self.trie_builder.insert(chars, index_to_push);
    }
    pub fn build(self) -> TokenTrie<'t, T> {
        TokenTrie {
            trie: self.trie_builder.build(),
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
        Matches {
            to_insert_first: &'t [&'t str],
            disable_default_check: bool,
            // usize because each insertion is tested by default
            expected_matches_results: &'t [(&'t str, usize)],
        },
    }

    macro_rules! trie_tests {
    ($($name:ident: $value:expr,)*) => {
    $(
        #[test]
        fn $name() {
            let method_to_call = $value;
            let mut ttb : TokenTrieBuilder<bool>= TokenTrieBuilder::new();

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
                super::tests::Call::Matches{to_insert_first,disable_default_check, expected_matches_results}  => {
                    for i in to_insert_first {
                        ttb.insert(i, &true);
                    }
                    let trie = ttb.build();
                    if !disable_default_check {
                        for i in to_insert_first {
                            let r = trie.matches(i);
                            assert_eq!(&true, r.unwrap());
                        }
                    }
                    for (key, expected) in expected_matches_results.into_iter() {
                        let r = trie.matches(key);
                        assert_eq!(true, r.is_err());
                        if let Result::Err(v) = r {
                            assert_eq!(*expected, v);
                        } else {
                            panic!();
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
        matches_a_1_b_0: Call::Matches {
            to_insert_first: &["ab", "aa"],
            disable_default_check: false,
            expected_matches_results: &[("a", 1), ("b", 0)] },
        matches_aa_2_aaa_3: Call::Matches {
            to_insert_first: &["aaab", "aaa", "aa"],
            disable_default_check: true,
            expected_matches_results: &[("aa", 2), ("aaa", 3)] },
    }
}
