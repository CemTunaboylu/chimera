use std::ops::Range;

use lexer::{lexer::Token, token_kind::TokenKind};
use num_derive::{FromPrimitive, ToPrimitive};

#[derive(Copy, Clone, Debug, FromPrimitive, Eq, Hash, Ord, PartialEq, PartialOrd, ToPrimitive)]
pub enum SyntaxKind {
    // recovering
    Recovered,
    Atom,
    // n-ary operators
    PrefixOp,
    PostFixaryOp,
    InfixBinaryOp,
    PrefixUnaryOp,
    // composite tokens
    Literal,
    VariableRef,
    VariableDef,
    ParenExpr,
    // separators
    Whitespace,
    Comma,
    Colon,
    SemiColon,
    // kw
    FnKw,
    LetKw,
    ReturnKw,
    // values
    Identifier,
    Number,
    // ops
    And,
    Or,
    Exclamation,
    GreaterThan,
    LessThan,
    Eq,
    NotEq,
    Plus,
    Minus,
    Modulus,
    Not,
    Star,
    Slash,
    //
    LParen,
    RParen,
    LBrace,
    RBrace,
    LSquareBrac,
    RSquareBrac,
    Comment,
    Root,
}

impl SyntaxKind {
    pub fn operators() -> [SyntaxKind; 14] {
        [
            Self::Colon,
            Self::And,
            Self::Or,
            Self::Exclamation,
            Self::GreaterThan,
            Self::LessThan,
            Self::Eq,
            Self::NotEq,
            Self::Plus,
            Self::Minus,
            Self::Modulus,
            Self::Not,
            Self::Star,
            Self::Slash,
        ]
    }
    pub fn is_trivia(self) -> bool {
        matches!(self, Self::Whitespace | Self::Comment)
    }

    pub fn is_a_separator(&self) -> bool {
        matches!(
            self,
            SyntaxKind::Whitespace
                | SyntaxKind::Comment
                | SyntaxKind::Comma
                | SyntaxKind::SemiColon
                | SyntaxKind::Colon
                | SyntaxKind::RBrace
                | SyntaxKind::RParen
                | SyntaxKind::RSquareBrac
        )
    }
}

impl From<TokenKind> for SyntaxKind {
    fn from(token_kind: TokenKind) -> Self {
        match token_kind {
            TokenKind::Space | TokenKind::Newline | TokenKind::Tab => Self::Whitespace,
            TokenKind::Comma => Self::Comma,
            TokenKind::StringLiteral | TokenKind::CharLiteral => Self::Literal,
            TokenKind::Colon => Self::Colon,
            TokenKind::SemiColon => Self::SemiColon,
            TokenKind::KwFn => Self::FnKw,
            TokenKind::KwLet => Self::LetKw,
            TokenKind::KwReturn => Self::ReturnKw,
            TokenKind::Identifier => Self::Identifier,
            TokenKind::Number => Self::Number,
            TokenKind::And => Self::And,
            TokenKind::Or => Self::Or,
            TokenKind::Exclamation => Self::Exclamation,
            TokenKind::GreaterThan => Self::GreaterThan,
            TokenKind::LessThan => Self::LessThan,
            TokenKind::Eq => Self::Eq,
            TokenKind::NotEq => Self::NotEq,
            TokenKind::Plus => Self::Plus,
            TokenKind::Minus => Self::Minus,
            TokenKind::Modulus => Self::Modulus,
            TokenKind::Not => Self::Not,
            TokenKind::Star => Self::Star,
            TokenKind::Slash => Self::Slash,
            TokenKind::LeftParen => Self::LParen,
            TokenKind::RightParen => Self::RParen,
            TokenKind::LeftBrace => Self::LBrace,
            TokenKind::RightBrace => Self::RBrace,
            TokenKind::LeftSquareBrac => Self::LSquareBrac,
            TokenKind::RightSquareBrac => Self::RSquareBrac,
            TokenKind::BlockComment | TokenKind::LineComment => Self::Comment,
            TokenKind::Root => SyntaxKind::Root,
            _ => todo!(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Syntax {
    pub kind: SyntaxKind,
    pub span: Range<usize>,
}

#[allow(dead_code)]
impl Syntax {
    pub fn is_trivia(&self) -> bool {
        matches!(self.kind, SyntaxKind::Whitespace | SyntaxKind::Comment)
    }
    pub fn is_of_kind(&self, kind: SyntaxKind) -> bool {
        self.kind == kind
    }

    pub fn is_a_separator(syntax: &Syntax) -> bool {
        syntax.kind.is_a_separator()
    }
}

impl From<Token> for Syntax {
    fn from(token: Token) -> Self {
        Self {
            kind: token.kind.into(),
            span: token.span,
        }
    }
}
