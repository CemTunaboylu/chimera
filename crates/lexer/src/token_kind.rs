use logos::Logos;
use thin_vec::{ThinVec, thin_vec};

#[derive(Copy, Clone, Debug, Logos, PartialEq)]
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
    // #[regex(r"/([^*]|\*[^*/])*\*/")] // / ... */
    // BlockCommentLeftStarMissing,
    #[regex(r"/\*([^*/])*/*")]
    BlockCommentRightStarMissing,
    #[regex("[\'].[\']")]
    CharLiteral,
    // possible errors to catch for CharLiteral
    #[regex("[\']([^\'])*")]
    CharLiteralMissingRight,
    #[token(":")]
    Colon,
    #[token("::")]
    ColonColon,
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
    #[regex(r"([0-9]*[.][0-9]+|[0-9]+[.][0-9]*)")]
    Float,
    #[token(">")]
    GreaterThan,
    #[token(">=")]
    GreaterThanOrEq,
    #[regex("[A-Za-z][A-Za-z0-9_]*")]
    Identifier,
    #[regex("[0-9]+[A-Za-z][A-Za-z0-9_]*")]
    IdentifierCannotBegin,
    #[regex("[0-9]+")]
    Integer,
    #[token("buffer")]
    KwBuffer,
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
    #[token("impl")]
    KwImpl,
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
    #[token("tensor")]
    KwTensor,
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
    #[token("<=")]
    LessThanOrEq,
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
    TypeStr,
    // #[token("u32")]
    // TypeU32,
    // #[token("u64")]
    // TypeU64,
    #[token("_")] // range operator
    Underscore,
    #[token("^")] // range operator
    Xor,
}

impl TokenKind {
    pub fn other_delimiters(&self) -> Option<ThinVec<TokenKind>> {
        use TokenKind::*;
        let others = match self {
            LeftParen => thin_vec![LeftBrace, LeftSquareBrac],
            LeftBrace => thin_vec![LeftParen, LeftSquareBrac],
            LeftSquareBrac => thin_vec![LeftParen, LeftBrace],
            RightParen => thin_vec![RightBrace, RightSquareBrac],
            RightBrace => thin_vec![RightParen, RightSquareBrac],
            RightSquareBrac => thin_vec![RightParen, RightBrace],
            _ => return None,
        };
        Some(others)
    }
}
