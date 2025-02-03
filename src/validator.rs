pub type Validator = fn(ch: &char) -> bool;
#[derive(Clone, Debug)]
pub enum ValidatorChain<T> {
    Once(T),
    Cycle(T),
}

pub fn once(i: usize) -> ValidatorChain<usize> {
    ValidatorChain::Once(i)
}

pub fn cycle(i: usize) -> ValidatorChain<usize> {
    ValidatorChain::Cycle(i)
}

#[derive(Debug, Clone)]
pub enum PostMatchAction<T> {
    None,
    Extend(ValidatorChain<T>),
    Validate(ValidatorChain<T>),
    ExtendThenValidate {
        me: ValidatorChain<T>,
        pmc: ValidatorChain<T>,
    },
}

impl<T> Default for PostMatchAction<T> {
    fn default() -> Self {
        Self::None
    }
}
impl<T> PostMatchAction<T> {
    pub fn is_none(&self) -> bool {
        matches!(self, PostMatchAction::None)
    }
}
