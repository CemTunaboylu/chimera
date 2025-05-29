use num_traits::{FromPrimitive, ToPrimitive, Zero};
use thin_vec::{ThinVec, thin_vec};

use std::fmt::Debug;
use std::ops::{Add, AddAssign, BitAnd, BitAndAssign, Not, Sub, SubAssign};

use crate::syntax_kind::SyntaxKind;

#[derive(Clone, Copy, PartialEq, PartialOrd)]
pub struct SyntaxKindBitSet(u128);

impl Debug for SyntaxKindBitSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let kinds: ThinVec<SyntaxKind> = (*self).into();
        let for_debug = kinds
            .iter()
            .map(|kind| format!("{:?}", kind))
            .collect::<ThinVec<_>>();
        f.write_str(for_debug.join(" or ").as_str())
    }
}

impl SyntaxKindBitSet {
    const LARGEST_INDEX: u32 = i128::BITS - 1;
    pub fn empty() -> Self {
        Self(0)
    }
    pub fn clear(&mut self) {
        self.0 = 0;
    }
    // TODO: do not take this as ref, SyntaxKind is smaller than ptr.
    pub fn contains(&self, kind: &SyntaxKind) -> bool {
        self.0 & (1 << kind.to_u16().unwrap()) != 0
    }

    pub fn intersect(&self, other: impl Into<SyntaxKindBitSet>) -> bool {
        self.0 & other.into().0 != 0
    }

    pub fn has_any(&self) -> bool {
        self.0 != 0
    }
}

impl Default for SyntaxKindBitSet {
    fn default() -> Self {
        Self::empty()
    }
}
#[allow(clippy::suspicious_arithmetic_impl)]
#[allow(clippy::suspicious_op_assign_impl)]
impl Add for SyntaxKindBitSet {
    type Output = SyntaxKindBitSet;

    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0 | rhs.0)
    }
}

#[allow(clippy::suspicious_arithmetic_impl)]
#[allow(clippy::suspicious_op_assign_impl)]
impl AddAssign for SyntaxKindBitSet {
    fn add_assign(&mut self, rhs: Self) {
        self.0 |= rhs.0;
    }
}

#[allow(clippy::suspicious_arithmetic_impl)]
#[allow(clippy::suspicious_op_assign_impl)]
impl AddAssign for &mut SyntaxKindBitSet {
    fn add_assign(&mut self, rhs: Self) {
        self.0 |= rhs.0;
    }
}

impl BitAndAssign for SyntaxKindBitSet {
    fn bitand_assign(&mut self, rhs: Self) {
        self.0 &= rhs.0;
    }
}

impl BitAnd for SyntaxKindBitSet {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        Self(self.0 & rhs.0)
    }
}

impl BitAndAssign for &mut SyntaxKindBitSet {
    fn bitand_assign(&mut self, rhs: Self) {
        self.0 &= rhs.0;
    }
}

impl Sub for SyntaxKindBitSet {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self(self.0 & !rhs.0)
    }
}

impl SubAssign for SyntaxKindBitSet {
    fn sub_assign(&mut self, rhs: Self) {
        self.0 &= !rhs.0;
    }
}

impl From<SyntaxKind> for SyntaxKindBitSet {
    fn from(value: SyntaxKind) -> Self {
        Self(1 << value.to_u16().unwrap())
    }
}

impl From<&SyntaxKind> for SyntaxKindBitSet {
    fn from(value: &SyntaxKind) -> Self {
        Self(1 << value.to_u16().unwrap())
    }
}

impl Not for SyntaxKindBitSet {
    type Output = Self;

    fn not(self) -> Self::Output {
        Self(!self.0)
    }
}

impl From<SyntaxKindBitSet> for ThinVec<SyntaxKind> {
    fn from(val: SyntaxKindBitSet) -> Self {
        let mut bits = val.0;
        let mut kinds = thin_vec![];
        while !bits.is_zero() {
            let from_right = (SyntaxKindBitSet::LARGEST_INDEX - bits.leading_zeros()) as i128;
            if let Some(kind) = SyntaxKind::from_u16(from_right as u16) {
                kinds.push(kind);
            }
            bits &= !(1 << from_right);
        }
        kinds
    }
}

impl From<&[SyntaxKind]> for SyntaxKindBitSet {
    fn from(value: &[SyntaxKind]) -> Self {
        let mut bitset = SyntaxKindBitSet::empty();
        for kind in value.iter() {
            bitset += kind.into();
        }
        bitset
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::syntax_kind::SyntaxKind;
    #[test]
    fn into_thin_vec_for_bitset() {
        let mut kinds = thin_vec![
            SyntaxKind::Literal,
            SyntaxKind::Plus,
            SyntaxKind::TyBool,
            SyntaxKind::RBrace,
            SyntaxKind::LParen,
            SyntaxKind::Int,
            SyntaxKind::Ident,
            SyntaxKind::AndAnd,
            SyntaxKind::Semi,
            SyntaxKind::ColonColon,
            SyntaxKind::Xor,
        ];
        kinds.sort();
        let mut expectations = SyntaxKindBitSet::empty();
        for k in &kinds {
            expectations += k.clone().into();
        }
        let mut from_exp: ThinVec<SyntaxKind> = expectations.into();
        from_exp.sort();
        assert_eq!(kinds, from_exp);
    }

    #[test]
    fn into_thin_vec_deduplicates() {
        let mut kinds = thin_vec![
            SyntaxKind::Literal,
            SyntaxKind::Plus,
            SyntaxKind::Semi,
            SyntaxKind::RBrace,
            SyntaxKind::LParen,
            SyntaxKind::Int,
            SyntaxKind::Ident,
            SyntaxKind::AndAnd,
            SyntaxKind::Semi,
            SyntaxKind::ColonColon,
            SyntaxKind::RBrace,
            SyntaxKind::LParen,
            SyntaxKind::Int,
            SyntaxKind::Ident,
            SyntaxKind::AndAnd,
            SyntaxKind::Semi,
            SyntaxKind::ColonColon,
            SyntaxKind::Xor,
        ];
        kinds.sort();
        let mut expectations = SyntaxKindBitSet::empty();
        for k in &kinds {
            expectations += k.clone().into();
        }
        kinds.dedup();
        let mut from_exp: ThinVec<SyntaxKind> = expectations.into();
        from_exp.sort();
        assert_eq!(kinds, from_exp);
    }

    #[test]
    fn arithmetic_holds() {
        let kinds = thin_vec![
            SyntaxKind::AndAnd,
            SyntaxKind::ColonColon,
            SyntaxKind::Ident,
            SyntaxKind::Int,
            SyntaxKind::LParen,
            SyntaxKind::Literal,
            SyntaxKind::Plus,
            SyntaxKind::RBrace,
            SyntaxKind::Semi,
        ];
        let all_exp: SyntaxKindBitSet = kinds.clone().as_ref().into();
        let chunked_exp = kinds.chunks(2).fold(SyntaxKindBitSet::empty(), |acc, x| {
            acc + x
                .iter()
                .map(|s| *s)
                .collect::<ThinVec<SyntaxKind>>()
                .as_ref()
                .into()
        });
        assert_eq!(all_exp, chunked_exp);
    }
}
