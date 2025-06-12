pub mod anchor;
pub mod bitset;
pub mod context;
pub mod language;
pub mod syntax_kind;

use std::ops::Range;

use bitset::SyntaxKindBitSet;
use lexer::{lexer::Token, token_type::TokenType};
use syntax_kind::SyntaxKind;

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
            TokenType::Operator if self.kind.is_assignment() => {
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
