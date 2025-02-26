use crate::token_kind::TokenKind;

#[derive(Clone, Debug, PartialEq)]
pub enum TokenType {
    // special kind since only logos needs it
    Root,
    Attribute,
    Branch,
    ClosingDelimiter(TokenKind),
    Comment,
    // has a TokenKind to suggest
    Error(TokenKind),
    Identifier,
    Keyword,
    Literal,
    // Vec<Vec<_>> at the end produces RightShift rather than 2 GreaterThan, thus needs separation
    MayNeedSep,
    // expects the TokenKind to be closed afterwards
    OpeningDelimiter(TokenKind),
    Operator,
    ReturnTypeIndicator,
    Semi,
    Seperator,
    Type,
    WhiteSpace,
}

impl From<TokenKind> for TokenType {
    fn from(value: TokenKind) -> Self {
        match value {
            TokenKind::Root => Self::Root,
            TokenKind::Attribute => Self::Attribute,
            TokenKind::FatRightArrow => Self::Branch,
            TokenKind::Comma => Self::Seperator,
            TokenKind::And
            | TokenKind::AndAnd
            | TokenKind::AndEq
            | TokenKind::Colon
            | TokenKind::Dot
            | TokenKind::Eq
            | TokenKind::EqEq
            | TokenKind::Exclamation
            | TokenKind::GreaterThan
            | TokenKind::LeftShiftEq
            | TokenKind::LessThan
            | TokenKind::Minus
            | TokenKind::MinusEq
            | TokenKind::Percent
            | TokenKind::PercentEq
            | TokenKind::NamespaceSep
            | TokenKind::NotEq
            | TokenKind::Or
            | TokenKind::OrEq
            | TokenKind::OrOr
            | TokenKind::Plus
            | TokenKind::PlusEq
            | TokenKind::QuestionMark
            | TokenKind::Slash
            | TokenKind::SlashEq
            | TokenKind::Star
            | TokenKind::StarEq
            | TokenKind::Underscore => Self::Operator,
            TokenKind::Identifier => Self::Identifier,
            TokenKind::IdentifierCannotBegin => Self::Error(TokenKind::Identifier),
            TokenKind::KwBreak
            | TokenKind::KwConst
            | TokenKind::KwContinue
            | TokenKind::KwElif
            | TokenKind::KwElse
            | TokenKind::KwEnum
            | TokenKind::KwFalse
            | TokenKind::KwFn
            | TokenKind::KwFor
            | TokenKind::KwIf
            | TokenKind::KwImport
            | TokenKind::KwIn
            | TokenKind::KwLet
            | TokenKind::KwMatch
            | TokenKind::KwModule
            | TokenKind::KwMut
            | TokenKind::KwReturn
            | TokenKind::KwSelf
            | TokenKind::KwStruct
            | TokenKind::KwTrue
            | TokenKind::KwType
            | TokenKind::KwWhile
            | TokenKind::Kwself => Self::Keyword,
            TokenKind::LeftBrace => Self::OpeningDelimiter(TokenKind::RightBrace),
            TokenKind::LeftParen => Self::OpeningDelimiter(TokenKind::RightParen),
            TokenKind::RightBrace => Self::ClosingDelimiter(TokenKind::LeftBrace),
            TokenKind::RightParen => Self::ClosingDelimiter(TokenKind::LeftParen),
            TokenKind::LeftShift                        // << OR <<T as> ..>
            | TokenKind::RightShift                     // >> OR T<_> > T<_>
            | TokenKind::RightShiftEq                   // >>= OR T<T<>>= 
            | TokenKind::LeftArrow => Self::MayNeedSep, // <- OR -2<-1
            TokenKind::LeftSquareBrac | TokenKind::RightSquareBrac => Self::Operator,
            TokenKind::RightArrow => Self::ReturnTypeIndicator,
            TokenKind::SemiColon => Self::Semi,
            // literals
            TokenKind::CharLiteral(_)
            | TokenKind::Float(_)
            | TokenKind::Integer(_)
            | TokenKind::StringLiteral => Self::Literal,

            TokenKind::IntegerHasNonDigit(i) => Self::Error(TokenKind::Integer(i)),
            TokenKind::CharLiteralMissingRight => Self::Error(TokenKind::CharLiteral(' ')),

            TokenKind::StringLiteralMissingRightDoubleQuote => {
                Self::Error(TokenKind::StringLiteral)
            }

            TokenKind::Space | TokenKind::Newline | TokenKind::Tab | TokenKind::NullTerminator => {
                Self::WhiteSpace
            }
            TokenKind::TypeBool
            | TokenKind::TypeByte
            | TokenKind::TypeChar
            | TokenKind::TypeF32
            | TokenKind::TypeI32
            | TokenKind::TypeStrSlice
            | TokenKind::TypeString
            | TokenKind::TypeU32 => Self::Type,
            TokenKind::BlockComment | TokenKind::LineComment => Self::Comment,
            TokenKind::BlockCommentLeftStarMissing | TokenKind::BlockCommentRightStarMissing => {
                Self::Error(TokenKind::BlockComment)
            }
        }
    }
}
