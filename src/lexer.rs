use logos::Logos;

#[derive(Copy, Clone, Debug, Logos, PartialEq)]
pub enum TokenKind {
    #[token("&")]
    And,
    #[token("&&")]
    AndAnd,
    #[token("&=")]
    AndEq,
    #[token("as")]
    As,
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
    #[token("..=")]
    DotDotEq,
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
    #[token("go")]
    KwGo, // same as go command as in Golang
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
    #[token("mod")]
    KwModule,
    #[token("mut")]
    KwMut,
    #[token("pkg")] // same as crate in rust
    KwPackage,
    #[token("pub")]
    KwPub,
    #[token("return")]
    KwReturn,
    #[token("Self")]
    KwSelf,
    #[token("static")]
    KwStatic,
    #[token("struct")]
    KwStruct,
    #[token("trait")]
    KwTrait,
    #[token("true")]
    KwTrue,
    #[token("type")]
    KwType,
    #[token("while")]
    KwWhile,
    #[token("yield")]
    KwYield,
    #[token("zk")]
    KwZk,
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
    #[token("f64")]
    TypeF64,
    #[token("i32")]
    TypeI32,
    #[token("i64")]
    TypeI64,
    #[token("str")]
    TypeStrSlice,
    #[token("String")]
    TypeString,
    #[token("u32")]
    TypeU32,
    #[token("u64")]
    TypeU64,
    #[token("_")] // a number can be written with _ to separate and easen reading
    Underscore,
}

#[cfg(test)]
mod tests {
    use parameterized_test::create;

    use super::*;

    fn assert_token_kind(with: &str, token_kind: TokenKind) {
        let mut lexer = TokenKind::lexer(with);
        assert_eq!(
            token_kind,
            lexer.next().unwrap().expect("expected a tokenkind")
        );
        assert_eq!(with, lexer.slice());
    }

    create! {
        create_lexer_test,
        (prog, expected_token_kind), {
            assert_token_kind(prog, expected_token_kind);
        }
    }
    create_lexer_test! {
            and: ("&", TokenKind::And),
            andand: ("&&", TokenKind::AndAnd),
            andeq: ("&=", TokenKind::AndEq),
            as_: ("as", TokenKind::As),
            attribute: ("@", TokenKind::Attribute),
            block_comment: ("/* block comment */", TokenKind::BlockComment),
            char_literal: ("'c'", TokenKind::CharLiteral),
            colon: (":", TokenKind::Colon),
            comma: (",", TokenKind::Comma),
            dot: (".", TokenKind::Dot),
            dotdot: ("..", TokenKind::DotDot),
            dotdotdot: ("...", TokenKind::DotDotDot),
            dotdoteq: ("..=", TokenKind::DotDotEq),
            eq: ("=", TokenKind::Eq),
            eqeq: ("==", TokenKind::EqEq),
            exclamation: ("!", TokenKind::Exclamation),
            fat_right_arrow: ("=>", TokenKind::FatRightArrow),
            greater_than: (">", TokenKind::GreaterThan),
            identifier: ("l0n9_id3nt1f13r", TokenKind::Identifier),
            kw_break: ("break", TokenKind::KwBreak),
            kw_const: ("const", TokenKind::KwConst),
            kw_continue: ("continue", TokenKind::KwContinue),
            kw_elif: ("elif", TokenKind::KwElif),
            kw_else: ("else", TokenKind::KwElse),
            kw_enum: ("enum", TokenKind::KwEnum),
            kw_false: ("false", TokenKind::KwFalse),
            kw_fn: ("fn", TokenKind::KwFn),
            kw_for: ("for", TokenKind::KwFor),
            kw_go: ("go", TokenKind::KwGo),
            kw_if: ("if", TokenKind::KwIf),
            kw_impl: ("impl", TokenKind::KwImpl),
            kw_import: ("import", TokenKind::KwImport),
            kw_in: ("in", TokenKind::KwIn),
            kw_let: ("let", TokenKind::KwLet),
            kw_module: ("mod", TokenKind::KwModule),
            kw_mut: ("mut", TokenKind::KwMut),
            kw_package: ("pkg", TokenKind::KwPackage),
            kw_pub: ("pub", TokenKind::KwPub),
            kw_return: ("return", TokenKind::KwReturn),
            kw_self: ("Self", TokenKind::KwSelf),
            kw_static: ("static", TokenKind::KwStatic),
            kw_struct: ("struct", TokenKind::KwStruct),
            kw_trait: ("trait", TokenKind::KwTrait),
            kw_true: ("true", TokenKind::KwTrue),
            kw_type: ("type", TokenKind::KwType),
            kw_while: ("while", TokenKind::KwWhile),
            kw_yield: ("yield", TokenKind::KwYield),
            kw_zk: ("zk", TokenKind::KwZk),
            kwself: ("self", TokenKind::Kwself),
            left_arrow: ("<-", TokenKind::LeftArrow),
            left_brace: ("{", TokenKind::LeftBrace),
            left_paren: ("(", TokenKind::LeftParen),
            left_shift: ("<<", TokenKind::LeftShift),
            left_shift_eq: ("<<=", TokenKind::LeftShiftEq),
            left_square_brac: ("[", TokenKind::LeftSquareBrac),
            less_than: ("<", TokenKind::LessThan),
            line_comment: ("// line comment \n", TokenKind::LineComment),
            minus: ("-", TokenKind::Minus),
            minus_eq: ("-=", TokenKind::MinusEq),
            modulus: ("%", TokenKind::Modulus),
            modulus_eq: ("%=", TokenKind::ModulusEq),
            namespace_sep: ("::", TokenKind::NamespaceSep),
            newline: ("\n\n", TokenKind::Newline),
            not: ("~", TokenKind::Not),
            not_eq: ("!=", TokenKind::NotEq),
            number: ("314", TokenKind::Number),
            or: ("|", TokenKind::Or),
            or_eq: ("|=", TokenKind::OrEq),
            or_or: ("||", TokenKind::OrOr),
            plus: ("+", TokenKind::Plus),
            plus_eq: ("+=", TokenKind::PlusEq),
            question_mark: ("?", TokenKind::QuestionMark),
            right_arrow: ("->", TokenKind::RightArrow),
            right_brace: ("}", TokenKind::RightBrace),
            right_paren: (")", TokenKind::RightParen),
            right_shift: (">>", TokenKind::RightShift),
            right_shift_eq: (">>=", TokenKind::RightShiftEq),
            right_square_brac: ("]", TokenKind::RightSquareBrac),
            semi_colon: (";", TokenKind::SemiColon),
            slash: ("/", TokenKind::Slash),
            slash_eq: ("/=", TokenKind::SlashEq),
            space: ("   ", TokenKind::Space),
            star: ("*", TokenKind::Star),
            star_eq: ("*=", TokenKind::StarEq),
            string_literal: ("\"string literal.\"", TokenKind::StringLiteral),
            tab: ("\t\t", TokenKind::Tab),
            type_byte: ("byte", TokenKind::TypeByte),
            type_bool: ("bool", TokenKind::TypeBool),
            type_char: ("char", TokenKind::TypeChar),
            type_f32: ("f32", TokenKind::TypeF32),
            type_f64: ("f64", TokenKind::TypeF64),
            type_i32: ("i32", TokenKind::TypeI32),
            type_i64: ("i64", TokenKind::TypeI64),
            type_str_slice: ("str", TokenKind::TypeStrSlice),
            type_string: ("String", TokenKind::TypeString),
            type_u32: ("u32", TokenKind::TypeU32),
            type_u64: ("u64", TokenKind::TypeU64),
            underscore: ("_", TokenKind::Underscore),

    }
}
