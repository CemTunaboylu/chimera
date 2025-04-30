use std::{
    cell::Cell,
    ops::{Add, AddAssign, BitAnd, BitAndAssign, Not, SubAssign},
};

use crate::{anchor::RollingBackAnchor, bitset::SyntaxKindBitSet};

#[derive(Clone, Debug, Default, PartialEq)]
pub struct ParserContext {
    expectations: Cell<SyntaxKindBitSet>,
    recovery_set: Cell<SyntaxKindBitSet>,
    allowed: Cell<SyntaxKindBitSet>,
}

#[derive(Clone, Copy, Debug)]
enum CellOf {
    Expectations,
    RecoverySet,
    Allowed,
}
impl ParserContext {
    pub fn new() -> Self {
        Self {
            expectations: Cell::new(SyntaxKindBitSet::empty()),
            recovery_set: Cell::new(SyntaxKindBitSet::empty()),
            allowed: Cell::new(SyntaxKindBitSet::empty()),
        }
    }

    pub unsafe fn rolling_back_anchor<'caller>(raw_ptr: *mut Self) -> RollingBackAnchor<'caller> {
        unsafe { RollingBackAnchor::with(raw_ptr) }
    }

    pub fn take(&self, ctx: ParserContext) {
        self.expectations.set(ctx.expectations.get());
        self.recovery_set.set(ctx.recovery_set.get());
        self.allowed.set(ctx.allowed.get());
    }

    fn get_cell_as_ref(&self, cell: CellOf) -> &Cell<SyntaxKindBitSet> {
        match cell {
            CellOf::Expectations => &self.expectations,
            CellOf::RecoverySet => &self.recovery_set,
            CellOf::Allowed => &self.allowed,
        }
    }

    fn add_cell(&self, cell: CellOf, other: SyntaxKindBitSet) {
        let cell = self.get_cell_as_ref(cell);
        let mut v = cell.get();
        v += other;
        cell.set(v);
    }

    fn sub_cell(&self, cell: CellOf, other: SyntaxKindBitSet) {
        let cell = self.get_cell_as_ref(cell);
        let mut v = cell.get();
        v -= other;
        cell.set(v);
    }

    fn and_cell(&self, cell: CellOf, other: SyntaxKindBitSet) {
        let cell = self.get_cell_as_ref(cell);
        let mut v = cell.get();
        v &= other;
        cell.set(v);
    }
    fn not_cell(&self, cell: CellOf) {
        let cell = self.get_cell_as_ref(cell);
        let v = cell.get();
        cell.set(!v);
    }

    pub fn get_expectations(&self) -> SyntaxKindBitSet {
        self.expectations.get()
    }
    pub fn expect(&self, exp: impl Into<SyntaxKindBitSet>) {
        self.add_cell(CellOf::Expectations, exp.into());
    }

    pub fn expect_only(&self, exp: impl Into<SyntaxKindBitSet>) {
        self.and_cell(CellOf::Expectations, SyntaxKindBitSet::empty());
        self.add_cell(CellOf::Expectations, exp.into());
    }

    pub fn del_expectation(&self, exp: impl Into<SyntaxKindBitSet>) {
        self.sub_cell(CellOf::Expectations, exp.into());
    }
    pub fn is_expected(&self, exp: impl Into<SyntaxKindBitSet>) -> bool {
        self.expectations.get().intersect(exp)
    }

    pub fn get_recovery_set(&self) -> SyntaxKindBitSet {
        self.recovery_set.get()
    }
    pub fn disallow_recovery_of(&self, exp: impl Into<SyntaxKindBitSet>) {
        self.add_cell(CellOf::RecoverySet, exp.into());
    }
    pub fn allow_recovery_of(&self, exp: impl Into<SyntaxKindBitSet>) {
        self.sub_cell(CellOf::RecoverySet, exp.into());
    }
    pub fn is_recovery_allowed(&self, rs: impl Into<SyntaxKindBitSet>) -> bool {
        !self.recovery_set.get().intersect(rs)
    }

    pub fn get_allowed(&self) -> SyntaxKindBitSet {
        self.allowed.get()
    }
    pub fn forbid(&self, exp: impl Into<SyntaxKindBitSet>) {
        self.sub_cell(CellOf::Allowed, exp.into());
    }

    pub fn forbid_all(&self) {
        self.and_cell(CellOf::Allowed, SyntaxKindBitSet::empty());
    }
    pub fn forbid_only(&self, exp: impl Into<SyntaxKindBitSet>) {
        self.allow_all();
        self.sub_cell(CellOf::Allowed, exp.into());
    }
    pub fn allow(&self, exp: impl Into<SyntaxKindBitSet>) {
        self.add_cell(CellOf::Allowed, exp.into());
    }

    pub fn allow_all(&self) {
        self.add_cell(CellOf::Allowed, !SyntaxKindBitSet::empty());
    }
    pub fn allow_only(&self, exp: impl Into<SyntaxKindBitSet>) {
        self.and_cell(CellOf::Allowed, SyntaxKindBitSet::empty());
        self.add_cell(CellOf::Allowed, exp.into());
    }
    pub fn is_allowed(&self, res: impl Into<SyntaxKindBitSet>) -> bool {
        self.allowed.get().intersect(res)
    }
}
impl Add for ParserContext {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        let new = Self::new();
        new.add_cell(CellOf::Expectations, rhs.get_expectations());
        new.add_cell(CellOf::RecoverySet, rhs.get_recovery_set());
        new.add_cell(CellOf::Allowed, rhs.get_allowed());
        new
    }
}

impl AddAssign for ParserContext {
    fn add_assign(&mut self, rhs: Self) {
        self.add_cell(CellOf::Expectations, rhs.get_expectations());
        self.add_cell(CellOf::RecoverySet, rhs.get_recovery_set());
        self.add_cell(CellOf::Allowed, rhs.get_allowed());
    }
}

impl BitAndAssign for ParserContext {
    fn bitand_assign(&mut self, rhs: Self) {
        self.and_cell(CellOf::Expectations, rhs.get_expectations());
        self.and_cell(CellOf::RecoverySet, rhs.get_recovery_set());
        self.and_cell(CellOf::Allowed, rhs.get_allowed());
    }
}

impl BitAnd for ParserContext {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        let new = Self::new();
        new.and_cell(CellOf::Expectations, rhs.get_expectations());
        new.and_cell(CellOf::RecoverySet, rhs.get_recovery_set());
        new.and_cell(CellOf::Allowed, rhs.get_allowed());
        new
    }
}

impl SubAssign for ParserContext {
    fn sub_assign(&mut self, rhs: Self) {
        self.sub_cell(CellOf::Expectations, rhs.get_expectations());
        self.sub_cell(CellOf::RecoverySet, rhs.get_recovery_set());
        self.sub_cell(CellOf::Allowed, rhs.get_allowed());
    }
}

impl AddAssign for &mut ParserContext {
    fn add_assign(&mut self, rhs: Self) {
        self.add_cell(CellOf::Expectations, rhs.get_expectations());
        self.add_cell(CellOf::RecoverySet, rhs.get_recovery_set());
        self.add_cell(CellOf::Allowed, rhs.get_allowed());
    }
}

impl SubAssign for &mut ParserContext {
    fn sub_assign(&mut self, rhs: Self) {
        self.sub_cell(CellOf::Expectations, rhs.get_expectations());
        self.sub_cell(CellOf::RecoverySet, rhs.get_recovery_set());
        self.sub_cell(CellOf::Allowed, rhs.get_allowed());
    }
}

impl BitAndAssign for &mut ParserContext {
    fn bitand_assign(&mut self, rhs: Self) {
        self.and_cell(CellOf::Expectations, rhs.get_expectations());
        self.and_cell(CellOf::RecoverySet, rhs.get_recovery_set());
        self.and_cell(CellOf::Allowed, rhs.get_allowed());
    }
}

impl Not for ParserContext {
    type Output = Self;

    fn not(self) -> Self::Output {
        let flipped = Self::new();
        flipped.not_cell(CellOf::Expectations);
        flipped.not_cell(CellOf::RecoverySet);
        flipped.not_cell(CellOf::Allowed);
        flipped
    }
}

impl From<&[SyntaxKindBitSet]> for ParserContext {
    fn from(value: &[SyntaxKindBitSet]) -> Self {
        assert!(value.len() == 3);
        Self {
            expectations: Cell::new(value[0]),
            recovery_set: Cell::new(value[1]),
            allowed: Cell::new(value[2]),
        }
    }
}

#[cfg(test)]
mod tests {
    use num_traits::{FromPrimitive, ToPrimitive};

    use crate::syntax_kind::SyntaxKind;

    use super::*;

    #[test]
    fn test_forbid_all() {
        let ctx = ParserContext::new();
        let allowed = SyntaxKind::Xor;
        ctx.allow(allowed);
        assert!(ctx.is_allowed(allowed), "{:?} should be allowed", allowed);
        ctx.forbid_all();
        for k in SyntaxKind::And.to_u16().unwrap()..=SyntaxKind::Xor.to_u16().unwrap() {
            let kind = SyntaxKind::from_u16(k).unwrap();
            let kind_bitset: SyntaxKindBitSet = kind.into();
            assert!(
                !ctx.is_allowed(kind_bitset),
                "{:?} should NOT be allowed",
                kind
            );
        }
    }

    #[test]
    fn test_allow_all() {
        let ctx = ParserContext::new();
        ctx.allow_all();
        for k in SyntaxKind::And.to_u16().unwrap()..=SyntaxKind::Xor.to_u16().unwrap() {
            let kind = SyntaxKind::from_u16(k).unwrap();
            let kind_bitset: SyntaxKindBitSet = kind.into();
            assert!(ctx.is_allowed(kind_bitset), "{:?} should be allowed", kind);
        }
    }

    #[test]
    fn test_allow_only() {
        let ctx = ParserContext::new();
        let only_allowed = SyntaxKind::LParen;
        ctx.allow_all();

        ctx.allow_only(only_allowed);
        assert!(
            ctx.is_allowed(only_allowed),
            "{:?} should be allowed",
            only_allowed
        );

        let not_allowed = SyntaxKind::RParen;
        assert!(
            !ctx.is_allowed(not_allowed),
            "{:?} should NOT be allowed",
            not_allowed
        );
    }

    #[test]
    fn test_add() {
        let ctx = ParserContext::new();
        let allow = SyntaxKind::LParen;
        ctx.add_cell(CellOf::Allowed, allow.into());
        assert!(ctx.is_allowed(allow));
    }

    #[test]
    fn test_sub() {
        let ctx = ParserContext::new();
        let forbid = SyntaxKind::LParen;
        let cell = CellOf::Allowed;
        ctx.add_cell(cell, forbid.into());
        assert!(ctx.is_allowed(forbid));
        ctx.sub_cell(cell, forbid.into());
        assert!(!ctx.is_allowed(forbid));
    }

    #[test]
    fn test_and() {
        let ctx = ParserContext::new();
        let cell = CellOf::Allowed;
        let present: SyntaxKindBitSet = [SyntaxKind::And, SyntaxKind::AndAnd, SyntaxKind::Or]
            .as_ref()
            .into();
        ctx.allow(present);
        assert!(ctx.is_allowed(present));
        let zero = SyntaxKindBitSet::empty();
        ctx.and_cell(cell, zero);
        assert!(!ctx.is_allowed(present));
    }

    #[test]
    fn test_not() {
        let ctx = ParserContext::new();
        let cell = CellOf::Allowed;
        let present: SyntaxKindBitSet = [SyntaxKind::And, SyntaxKind::AndAnd, SyntaxKind::Or]
            .as_ref()
            .into();
        ctx.allow(present);
        assert!(ctx.is_allowed(present));
        ctx.not_cell(cell);
        assert!(!ctx.is_allowed(present));
        assert!(ctx.is_allowed(!present));
    }
}
