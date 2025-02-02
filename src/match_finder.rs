use std::{fmt::Debug, ops::Range};

use trie_rs::{inc_search::IncSearch, map::Trie};

use crate::{
    match_validation::{apply_post_match_condition, extend_match, Chain},
    program_text::Program,
    range_indices::RangeIndices,
    trie::TrieValue,
    validator::{Validator, ValidatorChain},
};

pub type Answer<'m, V> = (Range<usize>, Option<&'m V>);
type TrieSearch<'s, V> = IncSearch<'s, char, V>;
struct TrieNavigator<'t, V> {
    search: TrieSearch<'t, TrieValue<V>>,
}

impl<'t, V> TrieNavigator<'t, V> {
    pub fn new(trie: &'t Trie<char, TrieValue<V>>) -> Self {
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

pub struct FirstMatchFinder<'m, Value>
where
    Value: Debug,
{
    program: Program<'m>,
    trie_navigator: TrieNavigator<'m, Value>,
    indices: RangeIndices,
    validators: &'static [Validator],
    chain_indices: &'m Vec<Vec<usize>>,
    saved: Option<Answer<'m, Value>>,
}

impl<'m, Value> FirstMatchFinder<'m, Value>
where
    Value: Debug,
{
    pub fn new(
        program: &'m str,
        trie: &'m Trie<char, TrieValue<Value>>,
        validators: &'static [Validator],
        chain_indices: &'m Vec<Vec<usize>>,
    ) -> Self {
        Self {
            program: Program::new(program),
            trie_navigator: TrieNavigator::new(trie),
            indices: RangeIndices::new(),
            validators: validators,
            chain_indices: chain_indices,
            saved: None,
        }
    }
    pub fn prepare_validators_for(&self, vc: &ValidatorChain<usize>) -> Chain<'m> {
        let (ix, is_cycle) = match vc {
            ValidatorChain::Once(ix) => (ix, false),
            ValidatorChain::Cycle(ix) => (ix, true),
        };
        let indices = self.chain_indices.get(*ix).unwrap();
        let validators_ref = self.validators.as_ref();
        match is_cycle {
            false => Chain::once(validators_ref, &indices),
            true => Chain::cycle(validators_ref, &indices),
        }
    }

    fn validate_post_match_condition(&mut self, validator_chain: &ValidatorChain<usize>) -> bool {
        let chain = self.prepare_validators_for(&validator_chain);
        let (before, after) = apply_post_match_condition(chain, &mut self.program);
        if before < after {
            self.indices.end_match(after);
            return true;
        }
        false
    }

    fn extend_match(&mut self, validator_chain: &ValidatorChain<usize>) -> bool {
        let chain = self.prepare_validators_for(&validator_chain);
        // Note: for Chimera, there is no case where match is extended iff
        // post_match_condition is met thus the greed and simplicity of the consume_while
        if let Some(moved_index) = extend_match(chain, &mut self.program) {
            self.indices.end_match(moved_index);
            true
        } else {
            false
        }
    }

    fn validate_match_result(&mut self) -> Option<&'m Value> {
        let trie_value = self.trie_navigator.current_value()?;

        let result = match &trie_value.to_validate {
            crate::validator::PostMatchAction::None => true,
            crate::validator::PostMatchAction::Validate(validator_chain) => {
                self.validate_post_match_condition(&validator_chain)
            }
            crate::validator::PostMatchAction::Extend(validator_chain) => {
                self.extend_match(validator_chain)
            }
            crate::validator::PostMatchAction::ExtendThenValidate {
                me: me_validator_chain,
                pmc: post_match_condition_validator_chain,
            } => {
                self.extend_match(me_validator_chain)
                    && self.validate_post_match_condition(post_match_condition_validator_chain)
            }
        };
        if result {
            Some(&trie_value.value)
        } else {
            None
        }
    }

    fn process_match_candidate(&mut self, peeked_index: usize) -> Option<Answer<'m, Value>> {
        let mut candidate = None;
        self.indices.end_match(peeked_index);
        match self.validate_match_result() {
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

    fn generate_result_for_no_candidate(&mut self) -> Option<Answer<'m, Value>> {
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

impl<'m, Value> Iterator for FirstMatchFinder<'m, Value>
where
    Value: Debug,
{
    type Item = Answer<'m, Value>;

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
                    match self.process_match_candidate(index) {
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
