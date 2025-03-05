pub mod bitset;
pub mod context;
pub mod language;
pub mod syntax_kind;

use std::ops::Range;

use bitset::SyntaxKindBitSet;
use lexer::{lexer::Token, token_kind::TokenKind, token_type::TokenType};
use syntax_kind::SyntaxKind;

#[derive(Clone, Debug, PartialEq)]
pub enum ParsedValue {
    Char(char),
    I32(i32),
    F32(f32),
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
    parsed_value: Option<ParsedValue>,
    token_type: TokenType,
}

#[allow(dead_code)]
impl Syntax {
    pub fn get_kind(&self) -> SyntaxKind {
        self.kind
    }

    pub fn get_span(&self) -> Range<usize> {
        self.span.clone()
    }

    pub fn get_parsed_value(&self) -> Option<&ParsedValue> {
        self.parsed_value.as_ref()
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

    pub fn imposed_restrictions(&self) -> [RestrictionType; 3] {
        let mut context_update = [RestrictionType::None; 3];
        match self.token_type {
            TokenType::OpeningDelimiter(closing) => {
                let mut expectations = SyntaxKindBitSet::empty();
                let closing_syntax_kind = SyntaxKind::from(closing);
                expectations += closing_syntax_kind.into();
                context_update[0] = RestrictionType::Add(expectations);

                let mut recovery_set = SyntaxKindBitSet::empty();
                recovery_set -= closing_syntax_kind.into();
                recovery_set += SyntaxKind::Semi.into();
                context_update[1] = RestrictionType::Override(recovery_set);

                let mut restrictions = SyntaxKindBitSet::empty();
                restrictions += closing_syntax_kind.into();
                context_update[2] = RestrictionType::Override(restrictions);
            }
            // TODO: add type based restrictions such as literals and keywords
            TokenType::Operator => {}
            _ => {}
        }
        context_update
    }
}

fn get_parsed_value_from_token(token: &Token) -> Option<ParsedValue> {
    match token.kind {
        TokenKind::CharLiteral(char) => Some(ParsedValue::Char(char)),
        TokenKind::Integer(i) => Some(ParsedValue::I32(i)),
        TokenKind::Float(f) => Some(ParsedValue::F32(f)),
        _ => None,
    }
}

impl From<Token> for Syntax {
    fn from(token: Token) -> Self {
        let parsed_value = get_parsed_value_from_token(&token);
        let kind: SyntaxKind = match token.ttype {
            TokenType::Error(_) => SyntaxKind::Errored,
            // precedence is shared
            TokenType::OperatorEq(_) => SyntaxKind::Eq,
            _ => token.kind.into(),
        };
        Self {
            kind,
            span: token.span,
            parsed_value,
            token_type: token.ttype,
        }
    }
}
