use std::ops::Range;

use lexer::{lexer::Token, token_kind::TokenKind};
use num_derive::{FromPrimitive, ToPrimitive};

#[derive(Copy, Clone, Debug, FromPrimitive, Eq, Hash, Ord, PartialEq, PartialOrd, ToPrimitive)]
pub enum SyntaxKind {
    // recovering
    Recovered,
    Atom,
    // n-ary operators
    InfixBinaryOp,
    PrefixUnaryOp,
    PostFixUnaryOp,
    // composite tokens
    StringLiteral,
    CharLiteral,
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
    Dot,
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
    pub fn is_literal_value(&self) -> bool {
        matches!(self, Self::Number | Self::StringLiteral | Self::CharLiteral)
    }
    pub fn is_binary_operator(&self) -> bool {
        matches!(
            self,
            Self::Plus | Self::Minus | Self::Star | Self::Slash | Self::Dot
        )
    }

    pub fn is_unary_operator(&self) -> bool {
        matches!(self, Self::Minus)
    }
    pub fn operators() -> [SyntaxKind; 15] {
        [
            Self::And,
            Self::Colon,
            Self::Dot,
            Self::Eq,
            Self::Exclamation,
            Self::GreaterThan,
            Self::LessThan,
            Self::Minus,
            Self::Modulus,
            Self::Not,
            Self::NotEq,
            Self::Or,
            Self::Plus,
            Self::Slash,
            Self::Star,
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
            TokenKind::And => Self::And,
            TokenKind::BlockComment | TokenKind::LineComment => Self::Comment,
            TokenKind::CharLiteral => Self::CharLiteral,
            TokenKind::Colon => Self::Colon,
            TokenKind::Comma => Self::Comma,
            TokenKind::Dot => SyntaxKind::Dot,
            TokenKind::Eq => Self::Eq,
            TokenKind::Exclamation => Self::Exclamation,
            TokenKind::GreaterThan => Self::GreaterThan,
            TokenKind::Identifier => Self::Identifier,
            TokenKind::KwFn => Self::FnKw,
            TokenKind::KwLet => Self::LetKw,
            TokenKind::KwReturn => Self::ReturnKw,
            TokenKind::LeftBrace => Self::LBrace,
            TokenKind::LeftParen => Self::LParen,
            TokenKind::LeftSquareBrac => Self::LSquareBrac,
            TokenKind::LessThan => Self::LessThan,
            TokenKind::Minus => Self::Minus,
            TokenKind::Modulus => Self::Modulus,
            TokenKind::Not => Self::Not,
            TokenKind::NotEq => Self::NotEq,
            TokenKind::Number => Self::Number,
            TokenKind::Or => Self::Or,
            TokenKind::Plus => Self::Plus,
            TokenKind::RightBrace => Self::RBrace,
            TokenKind::RightParen => Self::RParen,
            TokenKind::RightSquareBrac => Self::RSquareBrac,
            TokenKind::Root => SyntaxKind::Root,
            TokenKind::SemiColon => Self::SemiColon,
            TokenKind::Slash => Self::Slash,
            TokenKind::Space | TokenKind::Newline | TokenKind::Tab => Self::Whitespace,
            TokenKind::Star => Self::Star,
            TokenKind::StringLiteral => Self::StringLiteral,
            _ => {
                let msg = format!("{:?} don't have a corresponding syntaxkind", token_kind);
                panic!("{}", msg)
            }
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
