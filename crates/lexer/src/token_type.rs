use thin_vec::{ThinVec, thin_vec};

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
    OperatorEq(TokenKind),
    ReturnTypeIndicator,
    Semi,
    Seperator,
    Type,
    WhiteSpace,
}

impl TokenType {
    pub fn operators() -> ThinVec<TokenKind>{
        use TokenKind::*;
        thin_vec![
            And
            ,AndAnd
            ,Colon
            ,Dot
            ,Eq
            ,EqEq
            ,Exclamation
            ,GreaterThan
            ,GreaterThanOrEq
            ,LessThan
            ,LessThanOrEq
            ,Minus
            ,Percent
            ,ColonColon
            ,Or
            ,OrOr
            ,Plus
            ,QuestionMark
            ,Slash
            ,Star
            ,Underscore 
            ,Xor
            ]

    }
}

impl From<TokenKind> for TokenType {
    fn from(value: TokenKind) -> Self {
        use TokenKind::*;
        match value {
            Root => Self::Root,
            Attribute => Self::Attribute,
            FatRightArrow => Self::Branch,
            Comma => Self::Seperator,
            AndEq => Self::OperatorEq(TokenKind::And),
            LeftShiftEq => Self::OperatorEq(TokenKind::LeftShift),
            MinusEq => Self::OperatorEq(TokenKind::Minus),
            PercentEq => Self::OperatorEq(TokenKind::Percent),
            // TODO: this also is a boolean binary op 
            NotEq=> Self::OperatorEq(TokenKind::Exclamation),
            OrEq=> Self::OperatorEq(TokenKind::Or),
            PlusEq=> Self::OperatorEq(TokenKind::Plus),
            SlashEq=> Self::OperatorEq(TokenKind::Slash),
            StarEq=> Self::OperatorEq(TokenKind::Star),
            And
            | AndAnd
            | Colon
            | Dot
            | Eq
            | EqEq
            | Exclamation
            | GreaterThan
            | GreaterThanOrEq
            | LessThan
            | LessThanOrEq
            | Minus
            | Percent
            | ColonColon
            | Or
            | OrOr
            | Plus
            | QuestionMark
            | Slash
            | Star
            | Underscore 
            | Xor
            => Self::Operator,
            Identifier => Self::Identifier,
            IdentifierCannotBegin => Self::Error(TokenKind::Identifier),
            KwBreak
            | KwBuffer
            | KwConst
            | KwContinue
            | KwElif
            | KwElse
            | KwEnum
            | KwFalse
            | KwFn
            | KwFor
            | KwIf
            | KwImport
            | KwImpl
            | KwIn
            | KwLet
            | KwMatch
            | KwModule
            | KwMut
            | KwReturn
            | KwSelf
            | KwStruct
            | KwTensor
            | KwTrue
            | KwType
            | KwWhile
            | Kwself => Self::Keyword,
            LeftBrace => Self::OpeningDelimiter(RightBrace),
            LeftParen => Self::OpeningDelimiter(RightParen),
            RightBrace => Self::ClosingDelimiter(LeftBrace),
            RightParen => Self::ClosingDelimiter(LeftParen),
            LeftSquareBrac => Self::OpeningDelimiter(RightSquareBrac),
            RightSquareBrac => Self::ClosingDelimiter(LeftSquareBrac),

            LeftShift                        // << OR <<T as> ..>
            | RightShift                     // >> OR T<_> > T<_>
            | RightShiftEq                   // >>= OR T<T<>>= 
            | LeftArrow => Self::MayNeedSep, // <- OR -2<-1
            RightArrow => Self::ReturnTypeIndicator,
            SemiColon => Self::Semi,
            // literals
            CharLiteral
            | Float
            | Integer
            | StringLiteral => Self::Literal,

            CharLiteralMissingRight => Self::Error(CharLiteral),

            StringLiteralMissingRightDoubleQuote => {
                Self::Error(StringLiteral)
            }

            Space | Newline | Tab | NullTerminator => {
                Self::WhiteSpace
            }
            TypeBool
            | TypeChar
            | TypeF32
            | TypeI32
            | TypeStr
            => Self::Type,
            BlockComment | LineComment => Self::Comment,
            // BlockCommentLeftStarMissing | 
            BlockCommentRightStarMissing => {
                Self::Error(BlockComment)
            }
        }
    }
}
