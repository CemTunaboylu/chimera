use std::{
    char::ParseCharError,
    num::{ParseFloatError, ParseIntError},
};

use logos::Logos;

#[derive(Clone, Debug, Default, PartialEq)]
pub enum ParseError {
    Char(ParseCharError),
    I32(ParseIntError),
    F32(ParseFloatError),
    #[default]
    No,
}

fn to_i32(lex: &mut logos::Lexer<TokenKind>) -> Result<i32, ParseError> {
    let slice = lex.slice();
    slice.parse::<i32>().map_err(|e| ParseError::I32(e))
}

fn to_char(lex: &mut logos::Lexer<TokenKind>) -> Result<char, ParseError> {
    let slice = lex.slice();
    let last = slice.len() - 1;
    slice[1..last]
        .parse::<char>()
        .map_err(|e| ParseError::Char(e))
}

fn to_f32(lex: &mut logos::Lexer<TokenKind>) -> Result<f32, ParseError> {
    let slice = lex.slice();
    slice.parse().map_err(|e| ParseError::F32(e))
}
fn i32_recover(lex: &mut logos::Lexer<TokenKind>) -> Result<i32, ParseError> {
    let slice = lex.slice();
    slice
        .chars()
        .filter(|ch| ch.is_ascii_digit())
        .collect::<String>()
        .parse::<i32>()
        .map_err(|e| ParseError::I32(e))
}
#[derive(Copy, Clone, Debug, Logos, PartialEq)]
#[logos(error = ParseError)]
pub enum TokenKind {
    Root,
    #[token("&")]
    And,
    #[token("&&")]
    AndAnd,
    #[token("&=")]
    AndEq,
    // type coercion will be dealt later, if time permits
    // #[token("as")]
    // As,
    #[token("@")]
    Attribute,
    #[regex(r"/\*([^*]|\*[^*/])*\*/")]
    BlockComment,
    // possible errors to catch for BlockComment
    #[regex(r"/([^*]|\*[^*/])*\*/")] // / ... */
    BlockCommentLeftStarMissing,
    #[regex(r"/\*([^*/])*")]
    BlockCommentRightStarMissing,
    #[regex("[\'].[\']", to_char)]
    CharLiteral(char),
    // possible errors to catch for CharLiteral
    #[regex("[\']([^\'])*")]
    CharLiteralMissingRight,
    #[token(":")]
    Colon,
    #[token(",")]
    Comma,
    #[token(".")]
    Dot,
    // #[token("..=")]
    // DotDotEq,
    #[token("=")]
    Eq,
    #[token("==")]
    EqEq,
    #[token("!")]
    Exclamation,
    #[token("=>")]
    FatRightArrow,
    #[regex(r"([0-9]*[.][0-9]+|[0-9]+[.][0-9]*)", to_f32)]
    Float(f32),
    #[token(">")]
    GreaterThan,
    #[regex("[A-Za-z][A-Za-z0-9_]*")]
    Identifier,
    #[regex("[0-9]+[A-Za-z][A-Za-z0-9_]*")]
    IdentifierCannotBegin,
    #[regex("[0-9]+", to_i32)]
    Integer(i32),
    #[regex(r"\d+[^\d^'^.^,^\s]+\d+", i32_recover)]
    IntegerHasNonDigit(i32),
    #[token("break")]
    KwBreak,
    #[token("const")]
    KwConst,
    #[token("continue")]
    KwContinue,
    #[token("elif")]
    KwElif,
    #[token("else")]
    KwElse,
    #[token("enum")]
    KwEnum,
    #[token("false")]
    KwFalse,
    #[token("fn")]
    KwFn,
    #[token("for")]
    KwFor,
    #[token("if")]
    KwIf,
    // #[token("impl")]
    // KwImpl,
    #[token("import")]
    KwImport,
    #[token("in")]
    KwIn,
    #[token("let")]
    KwLet,
    #[token("match")]
    KwMatch,
    #[token("mod")]
    KwModule,
    #[token("mut")]
    KwMut,
    // #[token("pkg")] // same as crate in rust
    // KwPackage,
    // #[token("pub")]
    // KwPub,
    #[token("return")]
    KwReturn,
    #[token("Self")]
    KwSelf,
    // #[token("static")]
    // KwStatic,
    #[token("struct")]
    KwStruct,
    // #[token("trait")]
    // KwTrait,
    #[token("true")]
    KwTrue,
    #[token("type")]
    KwType,
    #[token("while")]
    KwWhile,
    // #[token("yield")]
    // KwYield,
    // #[token("zk")]
    // KwZk,
    #[token("self")]
    Kwself,
    #[token("<-")]
    LeftArrow,
    #[token("{")]
    LeftBrace,
    #[token("(")]
    LeftParen,
    #[token("<<")]
    LeftShift,
    #[token("<<=")]
    LeftShiftEq,
    #[token("[")]
    LeftSquareBrac,
    #[token("<")]
    LessThan,
    #[regex("//[^\n]*\n?")]
    LineComment,
    #[token("-")]
    Minus,
    #[token("-=")]
    MinusEq,
    #[token("%")]
    Percent,
    #[token("%=")]
    PercentEq,
    #[token("::")]
    NamespaceSep,
    #[regex("(\\r\\n|\\r|\\n)+")] // [<windows line break> | <linux> | <mac> ]
    Newline,
    #[token("!=")]
    NotEq,
    #[regex("\0*")]
    NullTerminator,
    #[token("|")]
    Or,
    #[token("|=")]
    OrEq,
    #[token("||")]
    OrOr,
    #[token("+")]
    Plus,
    #[token("+=")]
    PlusEq,
    #[token("?")]
    QuestionMark,
    #[token("->")]
    RightArrow,
    #[token("}")]
    RightBrace,
    #[token(")")]
    RightParen,
    #[token(">>")]
    RightShift,
    #[token(">>=")]
    RightShiftEq,
    #[token("]")]
    RightSquareBrac,
    #[token(";")]
    SemiColon,
    #[token("/")]
    Slash,
    #[token("/=")]
    SlashEq,
    #[regex(" +")]
    Space,
    #[token("*")]
    Star,
    #[token("*=")]
    StarEq,
    #[regex(r#""([^"\\\x00-\x1F]|\\(["\\bnfrt/]|u[a-fA-F0-9]{4}))*""#)]
    StringLiteral,
    #[regex(r#""([^"\\\x00-\x1F]|\\(["\\bnfrt/]|u[a-fA-F0-9]{4}))*"#)]
    StringLiteralMissingRightDoubleQuote,
    #[regex("\t+")]
    Tab,
    #[token("bool")]
    TypeBool,
    #[token("byte")]
    TypeByte,
    #[token("char")]
    TypeChar,
    #[token("f32")]
    TypeF32,
    // #[token("f64")]
    // TypeF64,
    #[token("i32")]
    TypeI32,
    // #[token("i64")]
    // TypeI64,
    #[token("str")]
    TypeStrSlice,
    #[token("String")]
    TypeString,
    #[token("u32")]
    TypeU32,
    // #[token("u64")]
    // TypeU64,
    #[token("_")] // range operator
    Underscore,
}
