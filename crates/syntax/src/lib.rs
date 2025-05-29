pub mod anchor;
pub mod bitset;
pub mod context;
pub mod language;
pub mod syntax_kind;

use std::ops::Range;

use bitset::SyntaxKindBitSet;
use lexer::{lexer::Token, token_type::TokenType};
use num_traits::{FromPrimitive, ToPrimitive};
use syntax_kind::SyntaxKind;

use lazy_static::lazy_static;

lazy_static! {
    pub static ref ASSIGNMENTS: SyntaxKindBitSet = SyntaxKind::assignments().as_ref().into();
    pub static ref CAN_BE_PARAMETERS: SyntaxKindBitSet =
        SyntaxKind::can_be_parameter().as_ref().into();
    pub static ref OPERATORS: SyntaxKindBitSet = SyntaxKind::operators().as_ref().into();
    pub static ref BINARY_OPERATORS: SyntaxKindBitSet = {
        use SyntaxKind::*;
        let ranges = [
            (And, ColonColon),
            (Dot, Excl),
            (Lt, NotEq),
            (Or, QMark),
            (Slash, StarEq),
        ];
        let mut set = SyntaxKindBitSet::empty();
        for (start, end) in ranges {
            for kind in start.to_u16().unwrap()..=end.to_u16().unwrap() {
                let kind: SyntaxKind = SyntaxKind::from_u16(kind).unwrap();
                set += kind.into();
            }
        }

        set += [Gt, Ge, LShift, LShiftEq, RShift, RShiftEq, Under, Xor]
            .as_ref()
            .into();
        set
    };
    pub static ref TYPES: SyntaxKindBitSet = SyntaxKind::types().as_ref().into();
}

pub fn is_an_assignment(kind: SyntaxKind) -> bool {
    ASSIGNMENTS.contains(kind)
}
pub fn is_an_operator(kind: SyntaxKind) -> bool {
    OPERATORS.contains(kind)
}
pub fn is_a_binary_operator(kind: SyntaxKind) -> bool {
    BINARY_OPERATORS.contains(kind)
}
pub fn can_be_a_parameter(kind: SyntaxKind) -> bool {
    CAN_BE_PARAMETERS.contains(kind)
}

pub fn can_be_a_parameter_with_mut(kind: SyntaxKind) -> bool {
    CAN_BE_PARAMETERS.contains(kind) || kind == SyntaxKind::Mut
}

pub fn non_assigning_operators() -> SyntaxKindBitSet {
    let a: SyntaxKindBitSet = SyntaxKind::assignments().as_ref().into();
    let o: SyntaxKindBitSet = SyntaxKind::operators().as_ref().into();
    o - a
}

pub fn is_a_type(kind: SyntaxKind) -> bool {
    TYPES.contains(kind)
}

pub fn is_of_type(syntax: &Syntax) -> bool {
    TYPES.contains(syntax.get_kind())
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum RestrictionType {
    Add(SyntaxKindBitSet),
    Override(SyntaxKindBitSet),
    Sub(SyntaxKindBitSet),
    None,
}

impl RestrictionType {
    pub fn apply(self, to: SyntaxKindBitSet) -> SyntaxKindBitSet {
        use RestrictionType::*;
        match self {
            Add(r) => to + r,
            Sub(r) => to - r,
            Override(r) => r,
            None => to,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Syntax {
    kind: SyntaxKind,
    span: Range<usize>,
    token_type: TokenType,
}

#[allow(dead_code)]
impl Syntax {
    pub fn get_kind(&self) -> SyntaxKind {
        self.kind
    }
    pub fn set_kind(&mut self, kind: SyntaxKind) {
        self.kind = kind
    }

    pub fn get_span(&self) -> Range<usize> {
        self.span.clone()
    }

    pub fn get_token_type(&self) -> &TokenType {
        &self.token_type
    }

    pub fn is_trivia(&self) -> bool {
        matches!(
            self.kind,
            SyntaxKind::Whitespace | SyntaxKind::NullTerm | SyntaxKind::Comment
        )
    }
    pub fn is_of_kind(&self, kind: SyntaxKind) -> bool {
        self.kind == kind
    }

    pub fn is_of_type(&self, token_type: TokenType) -> bool {
        self.token_type == token_type
    }

    pub fn imposed_restrictions(&self) -> [RestrictionType; 4] {
        use SyntaxKind::*;
        let mut context_update = [RestrictionType::None; 4];
        match self.token_type {
            TokenType::OpeningDelimiter(closing) => {
                let closing_syntax_kind = SyntaxKind::from(closing);
                // expectation
                context_update[0] = RestrictionType::Add(closing_syntax_kind.into());
                // recovery set
                context_update[1] = RestrictionType::Add(closing_syntax_kind.into());
                // allowed
                context_update[2] = RestrictionType::Add(closing_syntax_kind.into());
            }
            TokenType::Operator if is_an_assignment(self.kind) => {
                context_update[0] = RestrictionType::Add(Semi.into());
            }
            _ => {}
        }
        context_update
    }
}

impl From<Token> for Syntax {
    fn from(token: Token) -> Self {
        let kind: SyntaxKind = match token.ttype {
            TokenType::Error(_) => SyntaxKind::Recovered,
            // precedence is shared
            // TokenType::OperatorEq(_) => SyntaxKind::Eq,
            _ => token.kind.into(),
        };
        Self {
            kind,
            span: token.span,
            token_type: token.ttype,
        }
    }
}
