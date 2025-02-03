use crate::{program_text::Program, validator::Validator};

use std::{fmt::Debug, iter::Cycle, slice::Iter};

type ValidatorIndexIter<'v> = Iter<'v, usize>;
#[derive(Clone, Debug)]
pub enum Chain<'v> {
    Once((&'v [Validator], ValidatorIndexIter<'v>)),
    Cycle((&'v [Validator], Cycle<ValidatorIndexIter<'v>>)),
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
    pub fn once(v_arr: &'v [Validator], vec: &'v [usize]) -> Self {
        Self::Once((v_arr, vec.iter()))
    }
    pub fn cycle(v_arr: &'v [Validator], vec: &'v [usize]) -> Self {
        Self::Cycle((v_arr, vec.iter().cycle()))
    }
}

pub fn consume_with_chain(chain: Chain, prog: &mut Program) -> usize {
    prog.consume_while(chain);
    prog.peek_index()
}

pub fn extend_match(match_extender: Chain, prog: &mut Program) -> Option<usize> {
    Some(consume_with_chain(match_extender, prog))
}
pub fn apply_post_match_condition(pmc: Chain, prog: &mut Program) -> (usize, usize) {
    let before_index = prog.peek_index();
    if prog.peek().is_none() {
        return (0, before_index);
    }
    let moved_index = consume_with_chain(pmc, prog);
    (before_index, moved_index)
}
#[cfg(test)]
pub mod tests {

    enum TestValidation {
        Me((Vec<usize>, bool)),
        Pmc(Vec<usize>),
        Both(((Vec<usize>, bool), Vec<usize>)),
    }

    use super::*;

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

    pub const IS_SPACE: usize = 0;
    pub const IS_NEWLINE: usize = 1;
    pub const IS_DOUBLE_COLONS: usize = 2;
    pub const IS_MA: usize = 3;
    pub const IS_DA: usize = 4;
    pub const IS_DE: usize = 5;
    pub const IS_SU: usize = 6;
    pub const IS_NOT_DOUBLE_COLONS: usize = 7;

    fn chain_from_ix_list<'test>(ix_list: &'test Vec<usize>, is_cycle: bool) -> Chain<'test> {
        if is_cycle {
            Chain::Cycle((&VALIDATORS, ix_list.iter().cycle()))
        } else {
            Chain::Once((&VALIDATORS, ix_list.iter()))
        }
    }

    fn assert_for_me(
        ix_list: &Vec<usize>,
        is_cycle: bool,
        exp: Option<usize>,
        program: &mut Program,
    ) {
        let match_extender = chain_from_ix_list(&ix_list, is_cycle);
        let match_extension = extend_match(match_extender, program);
        assert_eq!(exp, match_extension);
    }

    fn assert_for_pmc(ix_list: &Vec<usize>, exp: (usize, usize), program: &mut Program) {
        let post_match_condition = chain_from_ix_list(&ix_list, false);
        let before_after = apply_post_match_condition(post_match_condition, program);
        assert_eq!(exp, before_after);
    }
    macro_rules! match_validator_tests {
    ($($name:ident: $value:expr,)*) => {
    $(
        #[test]
        fn $name() {
            let (program_str, validation, exp) = $value;
            let mut program = Program::new(program_str);

            match validation {
                TestValidation::Me((ix_list, is_cycle)) => {
                    assert_for_me(&ix_list, is_cycle, exp.0, &mut program);
                },
                TestValidation::Pmc(ix_list) => {
                    assert_for_pmc(&ix_list, exp.1, &mut program);
                },
                TestValidation::Both(((me_ix_list,is_cycle), pmc_ix_list)) => {
                    assert_for_me(&me_ix_list, is_cycle, exp.0, &mut program);
                    assert_for_pmc(&pmc_ix_list, exp.1, &mut program);
                },
            }
        }
    )*
    }
    }

    const CYCLED: bool = true;
    const ONCE: bool = false;

    match_validator_tests! {
        extend_match_with_whitespaces: (
            "   three whitespaces as prefix",
            (TestValidation::Me((vec![IS_SPACE], CYCLED))),
            (Some(3), (0,3))),
        extend_match_with_whitespaces_till_the_end: (
            "    ",
            (TestValidation::Both(((vec![IS_SPACE], ONCE), vec![IS_SPACE]))),
            (Some(1), (1,2))),
        cant_extend_match_with_whitespaces_because_post_match_fails: (
            "   x",
            (TestValidation::Both(((vec![IS_SPACE], CYCLED), vec![IS_NEWLINE]))),
            (Some(3), (3,3))),
        nothing_to_extend_the_match_to_and_post_match_fails: (
            "x",
            (TestValidation::Both(((vec![IS_NEWLINE], CYCLED), vec![IS_SPACE]))),
            (Some(0), (0,0))),
        nothing_to_extend_the_match_to_and_post_match_passes: (
            " ",
            (TestValidation::Both(((vec![IS_NEWLINE], CYCLED), vec![IS_SPACE]))),
            (Some(0), (0,1))),
        empty_string: (
            "",
            (TestValidation::Both(((vec![IS_NEWLINE], CYCLED), vec![IS_SPACE]))),
            (Some(0), (0,0))),
        none_match_extension_and_post_match_fails: (
            " ",
            (TestValidation::Pmc(vec![IS_NEWLINE])),
            (None, (0,0))),
        none_match_extension_and_post_match_passes: (
            "\n",
            (TestValidation::Pmc(vec![IS_NEWLINE])),
            (None, (0,1))),
        none_match_extension_and_post_match_fails_inequality: (
            ":",
            (TestValidation::Pmc(vec![IS_NOT_DOUBLE_COLONS])),
            (None, (0,0))),
        none_match_extension_and_post_match_passes_inequality: (
            ";",
            (TestValidation::Pmc(vec![IS_NOT_DOUBLE_COLONS])),
            (None, (0,1))),
        japanese_characters_with_match_extension_and_post_match_condition: (
            "まだまだ です また",
            (TestValidation::Both(((vec![IS_MA, IS_DA], CYCLED), vec![IS_SPACE, IS_DE, IS_SU]))),
            (Some(12), (12, 19))),
    }
}
