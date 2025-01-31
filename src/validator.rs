#[derive(Clone, Debug)]
pub enum ValidatorChain<T> {
    Once(T),
    Cycle(T),
}

impl<T> ValidatorChain<T> {
    pub fn once(t: T) -> Self {
        Self::Once(t)
    }
    pub fn cycle(t: T) -> Self {
        Self::Cycle(t)
    }
}

#[derive(Debug, Clone)]
pub struct ToValidate<T> {
    pub post_match_condition: Option<ValidatorChain<T>>,
    pub match_extender: Option<ValidatorChain<T>>,
}

impl<T> Default for ToValidate<T> {
    fn default() -> Self {
        Self {
            post_match_condition: None,
            match_extender: None,
        }
    }
}

impl<T> ToValidate<T> {
    pub fn new(me: ValidatorChain<T>, pmc: ValidatorChain<T>) -> Self {
        Self {
            match_extender: Some(me),
            post_match_condition: Some(pmc),
        }
    }
    pub fn with_match_extender(me: ValidatorChain<T>) -> Self {
        Self {
            match_extender: Some(me),
            ..Self::default()
        }
    }

    pub fn with_post_match_condition(pmc: ValidatorChain<T>) -> Self {
        Self {
            post_match_condition: Some(pmc),
            ..Self::default()
        }
    }
}
