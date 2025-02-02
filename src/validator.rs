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
pub enum Validate<T> {
    None,
    MatchExtender(ValidatorChain<T>),
    PostMatch(ValidatorChain<T>),
    // (ME, PMC)
    Both((ValidatorChain<T>, ValidatorChain<T>)),
}

impl<T> Default for Validate<T> {
    fn default() -> Self {
        Self::None
    }
}
impl<T> Validate<T> {
    pub fn is_none(&self) -> bool {
        match self {
            Validate::None => true,
            _ => false,
        }
    }
}
