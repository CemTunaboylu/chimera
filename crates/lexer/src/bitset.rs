use std::ops::{Add, AddAssign, BitAnd, BitAndAssign, Sub, SubAssign};

use crate::token_type::TokenType;

#[derive(Clone, Copy, Debug, Default, PartialEq, PartialOrd)]
pub struct TokenTypeBitSet(i32);

impl TokenTypeBitSet {
    const LARGEST_INDEX: u32 = i32::BITS - 1;
    pub fn empty() -> Self {
        Self(0 as i32)
    }
    pub fn clear(&mut self) {
        self.0 = 0;
    }
    pub fn contains(&self, other: impl Into<TokenTypeBitSet>) -> bool {
        self.0 & other.into().0 > 0
    }

    pub fn has_any(&self) -> bool {
        self.0 > 0
    }
}

impl From<TokenType> for TokenTypeBitSet {
    fn from(value: TokenType) -> Self {
        use TokenType::*;
        let index: i32 = match value {
            Root => 0,
            Attribute => 1,
            Branch => 2,
            ClosingDelimiter(_) => 3,
            Comment => 4,
            Error(_) => 5,
            Identifier => 6,
            Keyword => 7,
            Literal => 8,
            MayNeedSep => 9,
            OpeningDelimiter(_) => 10,
            Operator => 11,
            OperatorEq(_) => 12,
            ReturnTypeIndicator => 13,
            Semi => 14,
            Seperator => 15,
            Type => 16,
            WhiteSpace => 17,
        };
        Self(index)
    }
}

impl From<&TokenType> for TokenTypeBitSet {
    fn from(value: &TokenType) -> Self {
        use TokenType::*;
        let index: i32 = match value {
            Root => 0,
            Attribute => 1,
            Branch => 2,
            ClosingDelimiter(_) => 3,
            Comment => 4,
            Error(_) => 5,
            Identifier => 6,
            Keyword => 7,
            Literal => 8,
            MayNeedSep => 9,
            OpeningDelimiter(_) => 10,
            Operator => 11,
            OperatorEq(_) => 12,
            ReturnTypeIndicator => 13,
            Semi => 14,
            Seperator => 15,
            Type => 16,
            WhiteSpace => 17,
        };
        Self(index)
    }
}

impl Add for TokenTypeBitSet {
    type Output = TokenTypeBitSet;

    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0 | rhs.0)
    }
}

impl AddAssign for TokenTypeBitSet {
    fn add_assign(&mut self, rhs: Self) {
        self.0 |= rhs.0;
    }
}

impl AddAssign for &mut TokenTypeBitSet {
    fn add_assign(&mut self, rhs: Self) {
        self.0 |= rhs.0;
    }
}

impl BitAndAssign for TokenTypeBitSet {
    fn bitand_assign(&mut self, rhs: Self) {
        self.0 &= rhs.0;
    }
}

impl BitAnd for TokenTypeBitSet {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        Self(self.0 & rhs.0)
    }
}

impl BitAndAssign for &mut TokenTypeBitSet {
    fn bitand_assign(&mut self, rhs: Self) {
        self.0 &= rhs.0;
    }
}

impl Sub for TokenTypeBitSet {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self(self.0 & !rhs.0)
    }
}

impl SubAssign for TokenTypeBitSet {
    fn sub_assign(&mut self, rhs: Self) {
        self.0 &= !rhs.0;
    }
}

impl From<&[TokenType]> for TokenTypeBitSet {
    fn from(value: &[TokenType]) -> Self {
        let mut set = Self::empty();
        value.iter().for_each(|ttype| set += ttype.into());
        set
    }
}
