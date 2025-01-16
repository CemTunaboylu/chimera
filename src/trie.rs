use trie_rs::map::{Trie, TrieBuilder};

pub struct TokenTrie<'t, T> {
    // trie: Trie<&'t Chars<'t> , usize>,
    trie: Trie<char, usize>,
    values: Box<[&'t T]>,
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
        Insert { to_insert: &'t [&'t str] },
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
            };
        }
    )*
    }
    }

    trie_tests! {
        a: Call::Insert{to_insert: &["a"]},
        a_ab_abc: Call::Insert{to_insert: &["a", "ab", "abc"]},
        a_abcd_ab: Call::Insert{to_insert: &["a", "abcd", "ab"]},
        impl_fn_struct_for: Call::Insert{to_insert: &["impl", "fn", "struct", "for"]},
        averylongvariablename: Call::Insert{to_insert: &["averylongvariablename"]},
        i_i16_i32: Call::Insert{to_insert: &["i", "i16", "i32"]},
        space: Call::Insert{to_insert: &[" "]},
    }
}
