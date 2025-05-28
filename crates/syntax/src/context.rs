use std::{
    cell::Cell,
    ops::{Add, AddAssign, BitAnd, BitAndAssign, Not, SubAssign},
};

use crate::{anchor::RollingBackAnchor, bitset::SyntaxKindBitSet, syntax_kind::SyntaxKind};

#[derive(Clone, Debug, Default, PartialEq)]
pub struct CelledBits {
    celled: Cell<SyntaxKindBitSet>,
}
impl CelledBits {
    fn new() -> Self {
        Self {
            celled: Cell::new(SyntaxKindBitSet::empty()),
        }
    }
    fn with(bitset: SyntaxKindBitSet) -> Self {
        Self {
            celled: Cell::new(bitset),
        }
    }
    fn get_cell_as_ref(&self) -> &Cell<SyntaxKindBitSet> {
        &self.celled
    }
    fn take_bits_from(&self, cbits: CelledBits) {
        let cell = self.get_cell_as_ref();
        let other_bits = cbits.get_cell_as_ref().get();
        cell.set(other_bits);
    }
    fn add_cell(&self, other: SyntaxKindBitSet) {
        let cell = self.get_cell_as_ref();
        let mut v = cell.get();
        v += other;
        cell.set(v);
    }
    fn sub_cell(&self, other: SyntaxKindBitSet) {
        let cell = self.get_cell_as_ref();
        let mut v = cell.get();
        v -= other;
        cell.set(v);
    }
    fn and_cell(&self, other: SyntaxKindBitSet) {
        let cell = self.get_cell_as_ref();
        let mut v = cell.get();
        v &= other;
        cell.set(v);
    }
    fn not_cell(&self) {
        let cell = self.get_cell_as_ref();
        let v = cell.get();
        cell.set(!v);
    }
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct ParserContext {
    expectation: CelledBits,
    recovery_set: CelledBits,
    allowed: CelledBits,
    in_the_middle_of: CelledBits,
}

#[derive(Clone, Copy, Debug)]
enum CellOf {
    Expectations,
    RecoverySet,
    Allowed,
    InTheMiddleOf,
}

impl CellOf {
    fn iter() -> impl Iterator<Item = Self> {
        [
            CellOf::Expectations,
            CellOf::RecoverySet,
            CellOf::Allowed,
            CellOf::InTheMiddleOf,
        ]
        .into_iter()
    }
}

#[derive(Clone, Copy, Debug)]
enum Op {
    Add,
    And,
    Sub,
    Not,
}
impl ParserContext {
    pub fn new() -> Self {
        Self {
            // expectation denotes the syntax kind that is expected to be parsed next, if a syntax kind is not in this set, AND it is not forbidden, it means it is time to stop parsing this branch without erroring. It is used to expect a single kind, if multiple kinds are expected, we use allowed set instead.
            expectation: CelledBits::new(),
            // recovery set is the set of syntax kinds that are allowed to be recovered from, i.e. the kind that we are at can be bumped into an error node, if it is in this set. Kinds that are in expectation set generally are not in recovery set, e.g. ')'.
            recovery_set: CelledBits::new(),
            // allowed is the set of syntax kinds that are allowed to be parsed, for cases where we don't expect a specific kind, but multiple kinds are allowed to be parsed next. If a syntax kind is not in this set, it means we are in an error state, and we should recover from the error according to recovery set.
            allowed: CelledBits::new(),
            // in_the_middle_of denotes the composite syntax kind that we are parsing at the moment. It is helpful to manage expectations and allowed kinds, and reporting the error. We populate this set with a specific syntax kind to enforce more granular control. E.g. when we are parsing a VarDef, we expect a semicolon at the end. Even though during parsing w.r.t. binding power we error on a semicolon, we allow it to be parsed, and not error by checking if we are parsing a VarDef.
            in_the_middle_of: CelledBits::new(),
        }
    }

    pub unsafe fn rolling_back_anchor<'caller>(raw_ptr: *mut Self) -> RollingBackAnchor<'caller> {
        unsafe { RollingBackAnchor::with(raw_ptr) }
    }

    pub fn take(&self, ctx: ParserContext) {
        self.expectation.take_bits_from(ctx.expectation);
        self.recovery_set.take_bits_from(ctx.recovery_set);
        self.allowed.take_bits_from(ctx.allowed);
        self.in_the_middle_of.take_bits_from(ctx.in_the_middle_of);
    }

    fn get_celled_as_ref(&self, cell: CellOf) -> &CelledBits {
        match cell {
            CellOf::Expectations => &self.expectation,
            CellOf::RecoverySet => &self.recovery_set,
            CellOf::Allowed => &self.allowed,
            CellOf::InTheMiddleOf => &self.in_the_middle_of,
        }
    }

    fn add_cell(&self, cell: CellOf, other: SyntaxKindBitSet) {
        let cell = self.get_celled_as_ref(cell);
        cell.add_cell(other);
    }

    fn sub_cell(&self, cell: CellOf, other: SyntaxKindBitSet) {
        let cell = self.get_celled_as_ref(cell);
        cell.sub_cell(other);
    }

    fn and_cell(&self, cell: CellOf, other: SyntaxKindBitSet) {
        let cell = self.get_celled_as_ref(cell);
        cell.and_cell(other);
    }
    fn not_cell(&self, cell: CellOf) {
        let cell = self.get_celled_as_ref(cell);
        cell.not_cell();
    }
    pub fn get_expectations(&self) -> SyntaxKindBitSet {
        self.expectation.get_cell_as_ref().get()
    }
    pub fn expect(&self, exp: impl Into<SyntaxKindBitSet>) {
        self.add_cell(CellOf::Expectations, exp.into());
    }
    pub fn expect_only(&self, exp: impl Into<SyntaxKindBitSet>) {
        self.and_cell(CellOf::Expectations, SyntaxKindBitSet::empty());
        self.add_cell(CellOf::Expectations, exp.into());
    }
    pub fn expect_but(&self, exp: impl Into<SyntaxKindBitSet>) {
        self.add_cell(CellOf::Expectations, exp.into().not());
    }
    pub fn del_expectation(&self, exp: impl Into<SyntaxKindBitSet>) {
        self.sub_cell(CellOf::Expectations, exp.into());
    }
    pub fn is_expected(&self, exp: impl Into<SyntaxKindBitSet>) -> bool {
        self.expectation.get_cell_as_ref().get().intersect(exp)
    }
    pub fn get_recovery_set(&self) -> SyntaxKindBitSet {
        self.recovery_set.get_cell_as_ref().get()
    }
    pub fn disallow_recovery_of(&self, exp: impl Into<SyntaxKindBitSet>) {
        self.add_cell(CellOf::RecoverySet, exp.into());
    }
    pub fn allow_recovery_of(&self, exp: impl Into<SyntaxKindBitSet>) {
        self.sub_cell(CellOf::RecoverySet, exp.into());
    }
    pub fn allow_recovery_of_only(&self, exp: impl Into<SyntaxKindBitSet>) {
        self.and_cell(CellOf::RecoverySet, SyntaxKindBitSet::empty().not());
        self.sub_cell(CellOf::RecoverySet, exp.into());
    }
    pub fn is_recovery_allowed(&self, rs: impl Into<SyntaxKindBitSet>) -> bool {
        !self.recovery_set.get_cell_as_ref().get().intersect(rs)
    }
    pub fn get_allowed(&self) -> SyntaxKindBitSet {
        self.allowed.get_cell_as_ref().get()
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
        self.allowed.get_cell_as_ref().get().intersect(res)
    }

    fn get_in_the_middle_of(&self) -> SyntaxKindBitSet {
        self.in_the_middle_of.get_cell_as_ref().get()
    }

    fn op_on_ctx_cell(&self, op: Op, cell: CellOf, other: impl Into<SyntaxKindBitSet>) {
        match op {
            Op::Add => self.add_cell(cell, other.into()),
            Op::And => self.and_cell(cell, other.into()),
            Op::Sub => self.sub_cell(cell, other.into()),
            Op::Not => self.not_cell(cell),
        }
    }
    fn not_whole_ctx(&self) {
        for cell in CellOf::iter() {
            self.not_cell(cell)
        }
    }
    fn op_on_whole_ctx(&self, op: Op, other: &ParserContext) {
        for cell in CellOf::iter() {
            self.op_on_ctx_cell(
                op,
                cell,
                other.get_celled_as_ref(cell).get_cell_as_ref().get(),
            );
        }
    }
}
impl Add for ParserContext {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        let new = Self::new();
        new.op_on_whole_ctx(Op::Add, &rhs);
        new
    }
}

impl AddAssign for ParserContext {
    fn add_assign(&mut self, rhs: Self) {
        self.op_on_whole_ctx(Op::Add, &rhs);
    }
}

impl BitAndAssign for ParserContext {
    fn bitand_assign(&mut self, rhs: Self) {
        self.op_on_whole_ctx(Op::And, &rhs);
    }
}

impl BitAnd for ParserContext {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        let new = Self::new();
        new.op_on_whole_ctx(Op::And, &rhs);
        new
    }
}

impl SubAssign for ParserContext {
    fn sub_assign(&mut self, rhs: Self) {
        self.op_on_whole_ctx(Op::Sub, &rhs);
    }
}

impl AddAssign for &mut ParserContext {
    fn add_assign(&mut self, rhs: Self) {
        self.op_on_whole_ctx(Op::Add, rhs);
    }
}

impl SubAssign for &mut ParserContext {
    fn sub_assign(&mut self, rhs: Self) {
        self.op_on_whole_ctx(Op::Sub, rhs);
    }
}

impl BitAndAssign for &mut ParserContext {
    fn bitand_assign(&mut self, rhs: Self) {
        self.op_on_whole_ctx(Op::And, rhs);
    }
}

impl Not for ParserContext {
    type Output = Self;

    fn not(self) -> Self::Output {
        let flipped = Self::new();
        flipped.not_whole_ctx();
        flipped
    }
}

impl From<&[SyntaxKindBitSet]> for ParserContext {
    fn from(value: &[SyntaxKindBitSet]) -> Self {
        assert!(value.len() == 3);
        Self {
            expectations: CelledBits::with(value[0]),
            recovery_set: CelledBits::with(value[1]),
            allowed: CelledBits::with(value[2]),
            in_the_middle_of: CelledBits::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    use num_traits::{FromPrimitive, ToPrimitive};

    use crate::syntax_kind::SyntaxKind;

    use super::*;

    #[test]
    fn test_expect_but() {
        let ctx = ParserContext::new();
        let cell = CellOf::Expectations;
        let present: SyntaxKindBitSet = [SyntaxKind::And, SyntaxKind::AndAnd, SyntaxKind::Or]
            .as_ref()
            .into();
        ctx.expect_but(present);
        assert!(!ctx.is_expected(present));
        ctx.not_cell(cell);
        assert!(ctx.is_expected(present));
        assert!(!ctx.is_expected(!present));
    }
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
