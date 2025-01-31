use crate::{
    program_text::{Program, Validator},
    validator::ValidatorChain,
};

use std::{fmt::Debug, iter::Cycle, slice::Iter};

pub trait MatchValidator<T> {
    fn extend_match(
        &self,
        to_validate: Option<&ValidatorChain<T>>,
        prog: &mut Program,
    ) -> Option<usize>;
    fn apply_post_match_condition(
        &self,
        to_validate: Option<&ValidatorChain<T>>,
        prog: &mut Program,
    ) -> bool;
}
pub struct ArenaMatchValidator {
    validators: &'static [Validator],
    chain_indices: Vec<Vec<usize>>,
}

impl<'l> ArenaMatchValidator {
    pub fn new(validators: &'static [Validator]) -> Self {
        Self {
            validators: validators,
            chain_indices: vec![],
        }
    }
    pub fn register_index_list(&mut self, list: Vec<usize>) -> usize {
        let index_pushed = self.chain_indices.len();
        self.chain_indices.push(list);
        index_pushed
    }
    pub fn prepare_validators_for(&'l self, vc: &ValidatorChain<usize>) -> Chain<'l> {
        let (ix, is_cycle) = match vc {
            ValidatorChain::Once(ix) => (ix, false),
            ValidatorChain::Cycle(ix) => (ix, true),
        };
        let indices = self.chain_indices.get(*ix).unwrap();
        match is_cycle {
            false => Chain::once(&self.validators, &indices),
            true => Chain::cycle(&self.validators, &indices),
        }
    }
    fn consume_with_chain(&self, chain: &ValidatorChain<usize>, prog: &mut Program) -> usize {
        let validation_chain_iter = self.prepare_validators_for(chain);
        prog.consume_while(validation_chain_iter);
        prog.peek_index()
    }
}

impl MatchValidator<usize> for ArenaMatchValidator {
    fn extend_match(
        &self,
        match_extender: Option<&ValidatorChain<usize>>,
        prog: &mut Program,
    ) -> Option<usize> {
        Some(self.consume_with_chain(match_extender?, prog))
    }
    fn apply_post_match_condition(
        &self,
        pmc: Option<&ValidatorChain<usize>>,
        prog: &mut Program,
    ) -> bool {
        if prog.peek().is_none() || pmc.is_none() {
            return true;
        }
        let pmc = pmc.unwrap();
        let before_index = prog.peek().map(|(index, _)| index).unwrap();
        let passed_chars_moved_index = self.consume_with_chain(pmc, prog);
        before_index < passed_chars_moved_index
    }
}

type ValidatorIndexIter<'v> = Iter<'v, usize>;
#[derive(Clone, Debug)]
pub enum Chain<'v> {
    Once((&'static [Validator], ValidatorIndexIter<'v>)),
    Cycle((&'static [Validator], Cycle<ValidatorIndexIter<'v>>)),
}

impl<'v> Iterator for Chain<'v> {
    type Item = &'v Validator;

    fn next(&mut self) -> Option<Self::Item> {
        let (arr, ix) = match self {
            Chain::Once((arr, iter)) => (arr, iter.next()?),
            Chain::Cycle((arr, cycle)) => (arr, cycle.next()?),
        };

        arr.get(*ix)
    }
}

impl<'v> Chain<'v> {
    pub fn once(v_arr: &'static [Validator], vec: &'v Vec<usize>) -> Self {
        Self::Once((v_arr, vec.iter()))
    }
    pub fn cycle(v_arr: &'static [Validator], vec: &'v Vec<usize>) -> Self {
        Self::Cycle((v_arr, vec.iter().cycle()))
    }
}
#[cfg(test)]
pub mod tests {

    use super::*;
    use crate::validator::ValidatorChain;

    pub const VALIDATORS: [Validator; 8] = [
        |ch: &char| -> bool { *ch == ' ' },
        |ch: &char| -> bool { *ch == '\n' },
        |ch: &char| -> bool { *ch == ':' },
        |ch: &char| -> bool { *ch == 'ま' },
        |ch: &char| -> bool { *ch == 'だ' },
        |ch: &char| -> bool { *ch == 'で' },
        |ch: &char| -> bool { *ch == 'す' },
        |ch: &char| -> bool { *ch != ':' },
    ];

    pub type ValChainFunc =
        fn(&mut ArenaMatchValidator, Vec<usize>) -> Option<ValidatorChain<usize>>;

    pub fn validator_chain_for_index_list(
        match_validator: &mut ArenaMatchValidator,
        list: Vec<usize>,
        validator_chain_f: fn(usize) -> ValidatorChain<usize>,
    ) -> ValidatorChain<usize> {
        let ix = match_validator.register_index_list(list);
        validator_chain_f(ix)
    }

    pub fn once_validator_chain(
        match_validator: &mut ArenaMatchValidator,
        list: Vec<usize>,
    ) -> Option<ValidatorChain<usize>> {
        Some(validator_chain_for_index_list(
            match_validator,
            list,
            ValidatorChain::once,
        ))
    }

    pub fn cycled_validator_chain(
        match_validator: &mut ArenaMatchValidator,
        list: Vec<usize>,
    ) -> Option<ValidatorChain<usize>> {
        Some(validator_chain_for_index_list(
            match_validator,
            list,
            ValidatorChain::cycle,
        ))
    }
    pub fn none() -> Option<(
        // fn(&mut LazyMatchValidator, Vec<usize>) -> Option<ValidatorChain<usize>>,
        ValChainFunc,
        Vec<usize>,
    )> {
        None
    }

    pub const IS_SPACE: usize = 0;
    pub const IS_NEWLINE: usize = 1;
    pub const IS_DOUBLE_COLONS: usize = 2;
    pub const IS_MA: usize = 3;
    pub const IS_DA: usize = 4;
    pub const IS_DE: usize = 5;
    pub const IS_SU: usize = 6;
    pub const IS_NOT_DOUBLE_COLONS: usize = 7;

    macro_rules! match_validator_tests {
    ($($name:ident: $value:expr,)*) => {
    $(
        #[test]
        fn $name() {
            let mut match_validator = ArenaMatchValidator::new(&VALIDATORS);
            let (program_str, match_extender_tuple, post_match_condition_tuple, exp) = $value;
            let mut program = Program::new(program_str);

            match match_extender_tuple {
                None => {},
                Some((match_extender_f, me_ix_list)) => {
                    let match_extender = match_extender_f(&mut match_validator, me_ix_list);
                    let match_extension = match_validator.extend_match(match_extender.as_ref(), &mut program);
                    assert_eq!(exp.0, match_extension);
                }
            }
            match post_match_condition_tuple {
                None => {},
                Some((post_match_condition_f, pmc_ix_list)) => {
                    let post_match_condition = post_match_condition_f(&mut match_validator, pmc_ix_list);
                    let is_match_valid = match_validator.apply_post_match_condition(post_match_condition.as_ref(), &mut program);
                    assert_eq!(exp.1, is_match_valid);
                }
            }
        }
    )*
    }
    }

    match_validator_tests! {
        extend_match_with_whitespaces: (
            "   three whitespaces as prefix",
            Some((cycled_validator_chain, vec![IS_SPACE])),
            none(),
            (Some(3), true)),
        extend_match_with_whitespaces_till_the_end: (
            "    ",
            Some((cycled_validator_chain, vec![IS_SPACE])),
            Some((once_validator_chain, vec![IS_SPACE])),
            (Some(4), true)),
        cant_extend_match_with_whitespaces_because_post_match_fails: (
            "   x",
            Some((cycled_validator_chain, vec![IS_SPACE])),
            Some((once_validator_chain, vec![IS_NEWLINE])),
            (Some(3), false)),
        nothing_to_extend_the_match_to_and_post_match_fails: (
            "x",
            Some((cycled_validator_chain, vec![IS_NEWLINE])),
            Some((once_validator_chain, vec![IS_SPACE])),
            (Some(0), false)),
        nothing_to_extend_the_match_to_and_post_match_passes: (
            " ",
            Some((cycled_validator_chain, vec![IS_NEWLINE])),
            Some((once_validator_chain, vec![IS_SPACE])),
            (Some(0), true)),
        empty_string: (
            "",
            Some((once_validator_chain, vec![IS_NEWLINE])),
            Some((once_validator_chain, vec![IS_SPACE])),
            (Some(0), true)),
        none_match_extension_and_post_match_fails: (
            " ",
            none(),
            Some((once_validator_chain, vec![IS_NEWLINE])),
            (None, false)),
        none_match_extension_and_post_match_passes: (
            "\n",
            none(),
            Some((once_validator_chain, vec![IS_NEWLINE])),
            (None, true)),
        none_match_extension_and_post_match_fails_inequality: (
            ":",
            none(),
            Some((once_validator_chain, vec![IS_NOT_DOUBLE_COLONS])),
            (None, false)),
        none_match_extension_and_post_match_passes_inequality: (
            ";",
            none(),
            Some((once_validator_chain, vec![IS_NOT_DOUBLE_COLONS])),
            (None, true)),
        japanese_characters_with_match_extension_and_post_match_condition: (
            "まだまだ です また",
            Some((cycled_validator_chain, vec![IS_MA, IS_DA])),
            Some((once_validator_chain, vec![IS_SPACE, IS_DE, IS_SU])),
            (Some(12), true)),
    }
}
