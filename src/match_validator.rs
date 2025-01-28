use crate::program_text::{Program, Validator};

use std::fmt::Debug;

#[derive(Debug, Clone)]
pub struct ToValidate {
    pub post_match_condition: Option<Validator>,
    pub match_extender: Option<Validator>,
}
impl ToValidate {
    pub fn with_match_extender(me: Validator) -> Self {
        Self {
            match_extender: Some(me),
            post_match_condition: None,
        }
    }

    pub fn with_post_match_condition(pmc: Validator) -> Self {
        Self {
            post_match_condition: Some(pmc),
            match_extender: None,
        }
    }
    pub fn validate(&self, ch: &char, validator: Option<Validator>, default: bool) -> bool {
        match validator {
            Some(f) => f(&ch),
            None => default,
        }
    }
    pub fn post_match_validation(&self, ch: &char) -> bool {
        self.validate(ch, self.post_match_condition, true)
    }
}

pub trait MatchValidator {
    fn extend_match(to_validate: Option<&ToValidate>, prog: &mut Program) -> Option<usize>;
    fn is_match_valid(to_validate: Option<&ToValidate>, next_char: Option<char>) -> bool;
}
pub struct LazyMatchValidator;

impl MatchValidator for LazyMatchValidator {
    fn extend_match(to_validate: Option<&ToValidate>, prog: &mut Program) -> Option<usize> {
        let to_validate = to_validate?;
        let match_extender = to_validate.match_extender?;
        prog.consume_while(match_extender);
        let extended_ix = prog.peek().map_or(prog.len(), |(index, _)| index);
        Some(extended_ix)
    }
    fn is_match_valid(to_validate: Option<&ToValidate>, next_char: Option<char>) -> bool {
        match next_char {
            Some(ch) => to_validate.is_none_or(|tv| tv.post_match_validation(&ch)),
            None => true,
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    fn is_space(ch: &char) -> bool {
        *ch == ' '
    }

    fn does_not_have_colon(ch: &char) -> bool {
        *ch != ':'
    }

    fn is_newline(ch: &char) -> bool {
        *ch == '\n'
    }

    fn with_both(pmc: Validator, v: Validator) -> Option<ToValidate> {
        Some(ToValidate {
            post_match_condition: Some(pmc),
            match_extender: Some(v),
        })
    }

    macro_rules! match_validator_tests {
    ($($name:ident: $value:expr,)*) => {
    $(
        #[test]
        fn $name() {
            let (program_str, to_validate, exp) = $value;
            let mut program = Program::new(program_str);
            let match_extension = LazyMatchValidator::extend_match(to_validate.as_ref(), &mut program);
            assert_eq!(exp.0, match_extension);
            let is_match_valid = LazyMatchValidator::is_match_valid(to_validate.as_ref(), program.peek().map(|(_ix, ch)| ch));
            assert_eq!(exp.1, is_match_valid);
        }
    )*
    }
    }

    match_validator_tests! {
        none_to_validate: ("does not matter", None, (None, true)),
        extend_match_with_whitespaces: ("   three whitespaces as prefix", Some(ToValidate::with_match_extender(is_space)), (Some(3), true)),
        extend_match_with_whitespaces_till_the_end: ("    ", with_both(is_space, is_space), (Some(4), true)),
        cant_extend_match_with_whitespaces_because_post_match_fails: ( "   x", with_both(is_newline, is_space), (Some(3), false)),
        nothing_to_extend_the_match_to_and_post_match_fails: ( "x", with_both(is_newline, is_space), (Some(0), false)),
        nothing_to_extend_the_match_to_and_post_match_passes: ( "\n", with_both(is_newline, is_space), (Some(0), true)),
        empty_string: ( "", with_both(is_newline, is_space), (Some(0), true)),
        none_match_extension_and_post_match_fails: ( " ", Some(ToValidate::with_post_match_condition(is_newline)), (None, false)),
        none_match_extension_and_post_match_passes: ( "\n", Some(ToValidate::with_post_match_condition(is_newline)), (None, true)),
        none_match_extension_and_post_match_fails_inequality: ( ":", Some(ToValidate::with_post_match_condition(does_not_have_colon)), (None, false)),
        none_match_extension_and_post_match_passes_inequality: ( ";",Some(ToValidate::with_post_match_condition(does_not_have_colon)), (None, true)),
    }
}
