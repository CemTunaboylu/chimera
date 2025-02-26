use logos::Logos;

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
    #[regex(r"/([^*]|\*[^*/])*\*/")] // / ... */
    BlockCommentLeftStarMissing,
    #[regex(r"/\*([^*/])*")]
    BlockCommentRightStarMissing,
    #[regex("[\'].[\']")]
    CharLiteral,
    // possible errors to catch for BlockComment
    #[regex("[\']([^\'])+")]
    CharLiteralMissingRight,
    #[regex("([^\'^/])+[\']")]
    CharLiteralMissingLeft,
    #[token(":")]
    Colon,
    #[token(",")]
    Comma,
    #[token(".")]
    Dot,
    #[token("..")]
    DotDot,
    #[token("...")]
    DotDotDot,
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
    #[regex("([0-9]*[.][0-9]+|[0-9]+[.][0-9]*)")]
    Float,
    #[token(">")]
    GreaterThan,
    #[regex("[A-Za-z][A-Za-z0-9_]*")]
    Identifier,
    #[regex("[0-9]+[A-Za-z][A-Za-z0-9_]*")]
    IdentifierCannotBegin,
    #[regex("[0-9]+")]
    Integer,
    #[regex("[0-9]+[^0-9^'^.^,]+[0-9]+")]
    IntegerHasNonDigit,
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
    #[regex("/[^\n^*^/^=]+")]
    LineCommentMissingSlash,
    #[token("-")]
    Minus,
    #[token("-=")]
    MinusEq,
    #[token("%")]
    Modulus,
    #[token("%=")]
    ModulusEq,
    #[token("::")]
    NamespaceSep,
    #[regex("(\\r\\n|\\r|\\n)+")] // [<windows line break> | <linux> | <mac> ]
    Newline,
    #[token("~")]
    Not,
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
    #[regex(r#"([^"^'^/\\\x00-\x1F]|\\(["\\bnfrt/]|u[a-fA-F0-9]{4}))+""#)]
    StringLiteralMissingLeftDoubleQuote,
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
    // non-critical, will be dealt with later if time permits
    // #[token("_")] // a number can be written with _ to separate and easen reading
    // Underscore,
}
