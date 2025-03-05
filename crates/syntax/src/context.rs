use std::ops::{Add, AddAssign, BitAnd, BitAndAssign, Not, SubAssign};

use thin_vec::ThinVec;

use crate::bitset::SyntaxKindBitSet;

#[derive(Copy, Clone, Debug, Default, PartialEq)]
pub struct ParserContext {
    expectations: SyntaxKindBitSet,
    recovery_set: SyntaxKindBitSet,
    allowed: SyntaxKindBitSet,
}

impl ParserContext {
    pub fn new() -> Self {
        Self {
            expectations: SyntaxKindBitSet::empty(),
            recovery_set: SyntaxKindBitSet::empty(),
            allowed: SyntaxKindBitSet::empty(),
        }
    }

    pub fn get_expectations(&self) -> SyntaxKindBitSet {
        self.expectations
    }
    pub fn expect(&mut self, exp: impl Into<SyntaxKindBitSet>) {
        self.expectations += exp.into()
    }
    pub fn del_expectation(&mut self, exp: impl Into<SyntaxKindBitSet>) {
        self.expectations -= exp.into()
    }
    pub fn is_expected(&self, exp: impl Into<SyntaxKindBitSet>) -> bool {
        self.expectations.intersect(exp)
    }

    pub fn get_recovery_set(&self) -> SyntaxKindBitSet {
        self.recovery_set
    }
    pub fn disallow_recovery_of(&mut self, exp: impl Into<SyntaxKindBitSet>) {
        self.recovery_set += exp.into()
    }
    pub fn allow_recovery_of(&mut self, exp: impl Into<SyntaxKindBitSet>) {
        self.recovery_set -= exp.into()
    }
    pub fn is_recovery_allowed(&self, rs: impl Into<SyntaxKindBitSet>) -> bool {
        !self.recovery_set.intersect(rs)
    }

    pub fn get_allowed(&self) -> SyntaxKindBitSet {
        self.allowed
    }
    pub fn forbid(&mut self, exp: impl Into<SyntaxKindBitSet>) {
        self.allowed -= exp.into()
    }
    pub fn allow(&mut self, exp: impl Into<SyntaxKindBitSet>) {
        self.allowed += exp.into()
    }
    pub fn is_allowed(&self, res: impl Into<SyntaxKindBitSet>) -> bool {
        self.allowed.intersect(res)
    }
}
impl Add for ParserContext {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        let mut new = Self::new();
        new.expectations = self.expectations + rhs.expectations;
        new.recovery_set = self.recovery_set + rhs.recovery_set;
        new.allowed = self.allowed + rhs.allowed;
        new
    }
}

impl AddAssign for ParserContext {
    fn add_assign(&mut self, rhs: Self) {
        self.expectations += rhs.expectations;
        self.recovery_set += rhs.recovery_set;
        self.allowed += rhs.allowed;
    }
}

impl BitAndAssign for ParserContext {
    fn bitand_assign(&mut self, rhs: Self) {
        self.expectations &= rhs.expectations;
        self.recovery_set &= rhs.recovery_set;
        self.allowed &= rhs.allowed;
    }
}

impl BitAnd for ParserContext {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        let mut new = Self::new();
        new.expectations = self.expectations & rhs.expectations;
        new.recovery_set = self.recovery_set & rhs.recovery_set;
        new.allowed = self.allowed & rhs.allowed;
        new
    }
}

impl SubAssign for ParserContext {
    fn sub_assign(&mut self, rhs: Self) {
        self.expectations -= rhs.expectations;
        self.recovery_set -= rhs.recovery_set;
        self.allowed -= rhs.allowed;
    }
}

impl AddAssign for &mut ParserContext {
    fn add_assign(&mut self, rhs: Self) {
        self.expectations += rhs.expectations;
        self.recovery_set += rhs.recovery_set;
        self.allowed += rhs.allowed;
    }
}

impl SubAssign for &mut ParserContext {
    fn sub_assign(&mut self, rhs: Self) {
        self.expectations -= rhs.expectations;
        self.recovery_set -= rhs.recovery_set;
        self.allowed -= rhs.allowed;
    }
}

impl BitAndAssign for &mut ParserContext {
    fn bitand_assign(&mut self, rhs: Self) {
        self.expectations &= rhs.expectations;
        self.recovery_set &= rhs.recovery_set;
        self.allowed &= rhs.allowed;
    }
}

impl Not for ParserContext {
    type Output = Self;

    fn not(self) -> Self::Output {
        let mut flipped = Self::new();
        flipped.expectations = !self.expectations;
        flipped.recovery_set = !self.recovery_set;
        flipped.allowed = !self.allowed;
        flipped
    }
}

impl From<ThinVec<SyntaxKindBitSet>> for ParserContext {
    fn from(value: ThinVec<SyntaxKindBitSet>) -> Self {
        assert!(value.len() == 3);
        Self {
            expectations: value[0],
            recovery_set: value[1],
            allowed: value[2],
        }
    }
}
