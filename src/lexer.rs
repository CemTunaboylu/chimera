use logos::Logos;
use num_derive::{FromPrimitive, ToPrimitive};

pub struct Lexer<'l> {
    inner_lexer: logos::Lexer<'l, SyntaxKind>,
}

impl<'l> Lexer<'l> {
    pub fn new(program: &'l str) -> Self {
        Self {
            inner_lexer: SyntaxKind::lexer(program),
        }
    }

    pub fn slice(&self) -> &'l str {
        self.inner_lexer.slice()
    }
}

impl<'l> Iterator for Lexer<'l> {
    type Item = (Result<SyntaxKind, ()>, &'l str);

    fn next(&mut self) -> Option<Self::Item> {
        let kind = self.inner_lexer.next()?;
        let text = self.inner_lexer.slice();

        Some((kind, text))
    }
}

#[derive(
    Copy, Clone, Debug, Eq, FromPrimitive, Hash, Logos, Ord, PartialEq, PartialOrd, ToPrimitive,
)]
pub enum SyntaxKind {
    // composite nodes for rowan
    Atom,
    // n-ary operators
    PrefixOp,
    PostFixaryOp,
    InfixBinaryOp,
    PrefixUnaryOp,
    // composite tokens
    Literal,
    VariableRef,
    ParenExpr,

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
    #[regex("[\'].[\']")]
    CharLiteral,
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
    #[token(">")]
    GreaterThan,
    #[regex("[A-Za-z][A-Za-z0-9_]*")]
    Identifier,
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
    #[regex("\n+")]
    Newline,
    #[token("~")]
    Not,
    #[token("!=")]
    NotEq,
    #[regex("[0-9]+")]
    Number,
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

impl SyntaxKind {
    pub(crate) fn is_trivia(self) -> bool {
        matches!(
            self,
            Self::Space | Self::LineComment | Self::BlockComment | Self::Newline
        )
    }
}

#[cfg(test)]
mod tests {
    use parameterized_test::create;

    use super::*;

    fn assert_token_kind(with: &str, token_kind: SyntaxKind) {
        let mut lexer = Lexer::new(with);
        let result = lexer.next().unwrap();
        assert_eq!(token_kind, result.0.expect("expected a tokenkind"));
        assert_eq!(with, result.1);
    }

    create! {
        create_lexer_test,
        (prog, expected_token_kind), {
            assert_token_kind(prog, expected_token_kind);
        }
    }
    create_lexer_test! {
            and: ("&", SyntaxKind::And),
            andand: ("&&", SyntaxKind::AndAnd),
            andeq: ("&=", SyntaxKind::AndEq),
            // as_: ("as", TokenKind::As),
            attribute: ("@", SyntaxKind::Attribute),
            block_comment: ("/* block comment */", SyntaxKind::BlockComment),
            char_literal: ("'c'", SyntaxKind::CharLiteral),
            colon: (":", SyntaxKind::Colon),
            comma: (",", SyntaxKind::Comma),
            dot: (".", SyntaxKind::Dot),
            dotdot: ("..", SyntaxKind::DotDot),
            dotdotdot: ("...", SyntaxKind::DotDotDot),
            // dotdoteq: ("..=", TokenKind::DotDotEq),
            eq: ("=", SyntaxKind::Eq),
            eqeq: ("==", SyntaxKind::EqEq),
            exclamation: ("!", SyntaxKind::Exclamation),
            fat_right_arrow: ("=>", SyntaxKind::FatRightArrow),
            greater_than: (">", SyntaxKind::GreaterThan),
            identifier: ("l0n9_id3nt1f13r", SyntaxKind::Identifier),
            kw_break: ("break", SyntaxKind::KwBreak),
            kw_const: ("const", SyntaxKind::KwConst),
            kw_continue: ("continue", SyntaxKind::KwContinue),
            kw_elif: ("elif", SyntaxKind::KwElif),
            kw_else: ("else", SyntaxKind::KwElse),
            kw_enum: ("enum", SyntaxKind::KwEnum),
            kw_false: ("false", SyntaxKind::KwFalse),
            kw_fn: ("fn", SyntaxKind::KwFn),
            kw_for: ("for", SyntaxKind::KwFor),
            // kw_go: ("go", TokenKind::KwGo),
            kw_if: ("if", SyntaxKind::KwIf),
            // kw_impl: ("impl", TokenKind::KwImpl),
            kw_import: ("import", SyntaxKind::KwImport),
            kw_in: ("in", SyntaxKind::KwIn),
            kw_let: ("let", SyntaxKind::KwLet),
            kw_module: ("mod", SyntaxKind::KwModule),
            kw_mut: ("mut", SyntaxKind::KwMut),
            // kw_package: ("pkg", TokenKind::KwPackage),
            // kw_pub: ("pub", TokenKind::KwPub),
            kw_return: ("return", SyntaxKind::KwReturn),
            kw_self: ("Self", SyntaxKind::KwSelf),
            // kw_static: ("static", TokenKind::KwStatic),
            kw_struct: ("struct", SyntaxKind::KwStruct),
            // kw_trait: ("trait", TokenKind::KwTrait),
            kw_true: ("true", SyntaxKind::KwTrue),
            kw_type: ("type", SyntaxKind::KwType),
            kw_while: ("while", SyntaxKind::KwWhile),
            // kw_yield: ("yield", TokenKind::KwYield),
            // kw_zk: ("zk", TokenKind::KwZk),
            kwself: ("self", SyntaxKind::Kwself),
            left_arrow: ("<-", SyntaxKind::LeftArrow),
            left_brace: ("{", SyntaxKind::LeftBrace),
            left_paren: ("(", SyntaxKind::LeftParen),
            left_shift: ("<<", SyntaxKind::LeftShift),
            left_shift_eq: ("<<=", SyntaxKind::LeftShiftEq),
            left_square_brac: ("[", SyntaxKind::LeftSquareBrac),
            less_than: ("<", SyntaxKind::LessThan),
            line_comment: ("// line comment \n", SyntaxKind::LineComment),
            minus: ("-", SyntaxKind::Minus),
            minus_eq: ("-=", SyntaxKind::MinusEq),
            modulus: ("%", SyntaxKind::Modulus),
            modulus_eq: ("%=", SyntaxKind::ModulusEq),
            namespace_sep: ("::", SyntaxKind::NamespaceSep),
            newline: ("\n\n", SyntaxKind::Newline),
            not: ("~", SyntaxKind::Not),
            not_eq: ("!=", SyntaxKind::NotEq),
            number: ("314", SyntaxKind::Number),
            or: ("|", SyntaxKind::Or),
            or_eq: ("|=", SyntaxKind::OrEq),
            or_or: ("||", SyntaxKind::OrOr),
            plus: ("+", SyntaxKind::Plus),
            plus_eq: ("+=", SyntaxKind::PlusEq),
            question_mark: ("?", SyntaxKind::QuestionMark),
            right_arrow: ("->", SyntaxKind::RightArrow),
            right_brace: ("}", SyntaxKind::RightBrace),
            right_paren: (")", SyntaxKind::RightParen),
            right_shift: (">>", SyntaxKind::RightShift),
            right_shift_eq: (">>=", SyntaxKind::RightShiftEq),
            right_square_brac: ("]", SyntaxKind::RightSquareBrac),
            semi_colon: (";", SyntaxKind::SemiColon),
            slash: ("/", SyntaxKind::Slash),
            slash_eq: ("/=", SyntaxKind::SlashEq),
            space: ("   ", SyntaxKind::Space),
            star: ("*", SyntaxKind::Star),
            star_eq: ("*=", SyntaxKind::StarEq),
            string_literal: ("\"string literal.\"", SyntaxKind::StringLiteral),
            tab: ("\t\t", SyntaxKind::Tab),
            type_byte: ("byte", SyntaxKind::TypeByte),
            type_bool: ("bool", SyntaxKind::TypeBool),
            type_char: ("char", SyntaxKind::TypeChar),
            type_f32: ("f32", SyntaxKind::TypeF32),
            // type_f64: ("f64", TokenKind::TypeF64),
            type_i32: ("i32", SyntaxKind::TypeI32),
            // type_i64: ("i64", TokenKind::TypeI64),
            type_str_slice: ("str", SyntaxKind::TypeStrSlice),
            type_string: ("String", SyntaxKind::TypeString),
            type_u32: ("u32", SyntaxKind::TypeU32),
            // type_u64: ("u64", TokenKind::TypeU64),
            // underscore: ("_", TokenKind::Underscore),

    }
}
