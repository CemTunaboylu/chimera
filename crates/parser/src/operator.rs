use std::fmt::Debug;

use lexer::token_type::TokenType;
use syntax::{Syntax, syntax_kind::SyntaxKind};

use num_derive::{FromPrimitive, ToPrimitive};

pub fn starting_precedence() -> Bound {
    Bound::Included(Precedence::Base)
}

pub trait Op: Into<SyntaxKind> + Debug + Clone {
    fn precedence(&self) -> Precedence;
    fn associativity(&self) -> Associativity;
    fn from_syntax(syntax: &Syntax) -> Option<impl Op>;
    fn from_syntax_kind(kind: SyntaxKind) -> Option<impl Op>;
}

#[derive(Clone, Debug, FromPrimitive, PartialEq, PartialOrd, ToPrimitive)]
pub enum Associativity {
    Left,
    Right,
    None,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Bound {
    Included(Precedence),
    Excluded(Precedence),
}

impl Bound {
    pub fn from_op(op: impl Op) -> Self {
        use Associativity::*;
        let prec = op.precedence();
        match op.associativity() {
            Right => Bound::Included(prec),
            Left | None => Bound::Excluded(prec),
        }
    }
    pub fn lt(&self, rhs: &Precedence) -> bool {
        match self {
            Bound::Included(lhs) => lhs <= rhs,
            Bound::Excluded(lhs) => lhs < rhs,
        }
    }
    pub fn gt(&self, rhs: &Precedence) -> bool {
        match self {
            Bound::Included(lhs) => lhs > rhs,
            Bound::Excluded(lhs) => lhs >= rhs,
        }
    }
}

#[derive(Clone, Debug, FromPrimitive, PartialEq, PartialOrd, ToPrimitive)]
// Grouped under 'logical' hypernyms
pub enum Precedence {
    Base,
    // = += -= *= /= %= != ^= &= |= >>= <<=
    Assignment,
    // _ (ranging) start_end(exclusive), it is here to allow a-b_a+b -> [a-b, a+b)
    Ranging,
    BooleanOr,
    BooleanAnd,
    // == != < <= > >=
    Comparison,
    BitwiseOr,
    BitwiseXor,
    BitwiseAnd,
    // bit shifts >> <<
    Shift,
    // + -
    Additive,
    // * / %
    Multiplicative,
    // unary operators : *(deref), &(ref), -, !(not)
    Prefix,
    // coupling operators that couple its operands:
    // .(member) , :(typing), ::(namespace), ?(unwrap)
    Coupling,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum AssocBinOp {
    Add,
    BoolAnd,
    Assgmt,
    AssgmtWith(SyntaxKind),
    BitAnd,
    BitOr,
    Div,
    Dot,
    EqEq,
    Ge,
    Gt,
    LShift,
    Le,
    Lt,
    Mod,
    Mul,
    Namespaced,
    NotEq,
    BoolOr,
    RShift,
    Range,
    Sub,
    TypeHint, // <identifier> : <type keyword>
    Xor,
}

impl Op for AssocBinOp {
    // in ascending order
    fn precedence(&self) -> Precedence {
        use AssocBinOp::*;
        use Precedence::*;
        match self {
            Add | Sub => Additive,
            Assgmt | AssgmtWith(_) => Assignment,
            BitAnd => BitwiseAnd,
            BitOr => BitwiseOr,
            BoolAnd => BooleanAnd,
            BoolOr => BooleanOr,
            Dot | TypeHint | Namespaced => Coupling,
            EqEq | Gt | Ge | Lt | Le | NotEq => Comparison,
            LShift | RShift => Shift,
            Mul | Div | Mod => Multiplicative,
            Range => Ranging,
            Xor => BitwiseXor,
        }
    }

    fn associativity(&self) -> Associativity {
        use AssocBinOp::*;
        use Associativity::*;
        match self {
            Assgmt | AssgmtWith(_) => Right,
            Range | Gt | Ge | Lt | Le | EqEq | NotEq => None,
            _ => Left,
        }
    }

    fn from_syntax(syntax: &Syntax) -> Option<impl Op> {
        match syntax.get_token_type() {
            TokenType::Operator | TokenType::OperatorEq(_) => {
                Self::from_syntax_kind(syntax.get_kind())
            }
            _ => None,
        }
    }

    fn from_syntax_kind(kind: SyntaxKind) -> Option<impl Op> {
        use SyntaxKind::*;
        let op = match kind {
            And => Self::BitAnd,
            AndAnd => Self::BoolAnd,
            AndEq => Self::AssgmtWith(And),
            Colon => Self::TypeHint,
            ColonColon => Self::Namespaced,
            Dot => Self::Dot,
            Eq => Self::Assgmt,
            EqEq => Self::EqEq,
            Gt => Self::Gt,
            Ge => Self::Ge,
            LShift => Self::LShift,
            LShiftEq => Self::AssgmtWith(LShift),
            Lt => Self::Lt,
            Le => Self::Le,
            Minus => Self::Sub,
            MinusEq => Self::AssgmtWith(Minus),
            NotEq => Self::NotEq,
            OrEq => Self::AssgmtWith(Or),
            Or => Self::BitOr,
            OrOr => Self::BoolOr,
            Percent => Self::Mod,
            PercentEq => Self::AssgmtWith(Percent),
            Plus => Self::Add,
            PlusEq => Self::AssgmtWith(Plus),
            RShift => Self::RShift,
            RShiftEq => Self::AssgmtWith(RShift),
            Slash => Self::Div,
            SlashEq => Self::AssgmtWith(Slash),
            Star => Self::Mul,
            StarEq => Self::AssgmtWith(Star),
            Under => Self::Range,
            Xor => Self::Xor,
            _ => return None,
        };
        Some(op)
    }
}

#[allow(clippy::from_over_into)]
impl Into<SyntaxKind> for AssocBinOp {
    fn into(self) -> SyntaxKind {
        if self == AssocBinOp::TypeHint {
            SyntaxKind::TypeHint
        } else {
            SyntaxKind::InfixBinOp
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum AssocUnOp {
    // prefix
    Deref, // *
    Ref,   // &
    Neg,
    Not,
    // Postfix
    QMark,
}

impl Op for AssocUnOp {
    // in ascending order
    fn precedence(&self) -> Precedence {
        if matches!(self, AssocUnOp::QMark) {
            Precedence::Coupling
        } else {
            Precedence::Prefix
        }
    }

    fn associativity(&self) -> Associativity {
        if matches!(self, AssocUnOp::QMark) {
            Associativity::Left
        } else {
            Associativity::Right
        }
    }

    fn from_syntax(syntax: &Syntax) -> Option<impl Op> {
        match syntax.get_token_type() {
            TokenType::Operator => Self::from_syntax_kind(syntax.get_kind()),
            _ => None,
        }
    }

    fn from_syntax_kind(kind: SyntaxKind) -> Option<impl Op> {
        use SyntaxKind::*;
        let op = match kind {
            And => Self::Ref,
            Excl => Self::Not,
            Minus => Self::Neg,
            QMark => Self::QMark,
            Star => Self::Deref,
            _ => return None,
        };
        Some(op)
    }
}

#[allow(clippy::from_over_into)]
impl Into<SyntaxKind> for AssocUnOp {
    fn into(self) -> SyntaxKind {
        use AssocUnOp::*;
        match self {
            QMark => SyntaxKind::PostfixUnaryOp,
            Not | Neg | Deref | Ref => SyntaxKind::PrefixUnaryOp,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn precedence_comparison_test() {
        assert!(Precedence::Additive < Precedence::Multiplicative);
        assert!(Precedence::Prefix < Precedence::Coupling);
    }
}
