use std::ops::Range;

use crate::{errors::LexError, token_kind::TokenKind, token_type::TokenType};

use logos::Logos;
use smol_str::{SmolStr, ToSmolStr};

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub ttype: TokenType,
    pub span: Range<usize>,
    pub slice: Option<SmolStr>,
}

pub type LexResult = Result<Token, LexError>;
#[derive(Clone, Debug)]
pub struct Lexer<'input> {
    inner_lexer: logos::Lexer<'input, TokenKind>,
    peeked: Option<Option<<Self as Iterator>::Item>>,
}

impl<'input> Lexer<'input> {
    pub fn new(program: &'input str) -> Self {
        Self {
            inner_lexer: TokenKind::lexer(program),
            peeked: None,
        }
    }

    pub fn span(&self) -> Range<usize> {
        self.inner_lexer.span()
    }

    pub fn peek(&mut self) -> Option<&<Self as Iterator>::Item> {
        if self.peeked.is_none() {
            let next = self.next();
            return self.peeked.get_or_insert(next).as_ref();
        }
        self.peeked.as_ref().unwrap().as_ref()
    }
    pub fn source(&self) -> &str {
        self.inner_lexer.source().as_ref()
    }
}

impl Iterator for Lexer<'_> {
    type Item = LexResult;

    fn next(&mut self) -> Option<Self::Item> {
        if self.peeked.is_some() {
            return self.peeked.take().unwrap();
        }
        let result = match self.inner_lexer.next()? {
            Ok(kind) => {
                let slice = (kind == TokenKind::StringLiteral)
                    .then_some(self.inner_lexer.slice().to_smolstr());
                Ok(Token {
                    kind,
                    ttype: kind.clone().into(),
                    span: self.inner_lexer.span(),
                    slice,
                })
            }
            Err(parse_error) => {
                let err_span = self.inner_lexer.span();
                Err(LexError::new(err_span, format!("{:?}", parse_error)))
            }
        };
        Some(result)
    }
}

#[cfg(test)]
mod tests {
    use parameterized_test::create;

    use super::*;

    #[test]
    fn ensure_peek_works() {
        let program = "1 ";
        let mut lexer = Lexer::new(program);
        assert_eq!(
            Some(&Ok(Token {
                kind: TokenKind::Integer,
                ttype: TokenKind::Integer.into(),
                span: 0..1,
                slice: None,
            })),
            lexer.peek()
        );

        assert_eq!(
            Some(Ok(Token {
                kind: TokenKind::Integer,
                ttype: TokenKind::Integer.into(),
                span: 0..1,
                slice: None,
            })),
            lexer.next()
        );

        assert_eq!(
            Some(&Ok(Token {
                kind: TokenKind::Space,
                ttype: TokenKind::Space.into(),
                span: 1..2,
                slice: None,
            })),
            lexer.peek()
        );
    }

    fn assert_token_kind(with: &str, kind: TokenKind) {
        let mut lexer = Lexer::new(with);
        let result = lexer.next().unwrap();
        let span = 0..with.len();
        let ttype: TokenType = kind.clone().into();
        let slice =
            (kind == TokenKind::StringLiteral).then_some(lexer.inner_lexer.slice().to_smolstr());
        let token = Token {
            kind,
            ttype,
            span,
            slice,
        };
        assert_eq!(token, result.expect("expected a tokenkind"));
    }

    create! {
        create_lexer_test,
        (prog, expected_kind), {
            assert_token_kind(prog, expected_kind);
        }
    }

    create! {
        create_lexer_test_multiple_tokens_exp,
        (prog, expected_tokens), {
        let mut lexer = Lexer::new(prog);
        for exp in expected_tokens {
            let token = lexer.next().unwrap();
            assert_eq!(exp, &token.expect("expected a token"));
        }
        assert!(lexer.next().is_none());
        }
    }
    create_lexer_test! {
            and: ("&", TokenKind::And),
            andand: ("&&", TokenKind::AndAnd),
            andeq: ("&=", TokenKind::AndEq),
            // as_: ("as", TokenKind::As),
            attribute: ("@", TokenKind::Attribute),
            block_comment: ("/* block comment */", TokenKind::BlockComment),
            block_comment_right_star_missing: ("/* block comment /", TokenKind::BlockCommentRightStarMissing),
            block_comment_right_star_missing_ends_line_comment: ("/* block comment //", TokenKind::BlockCommentRightStarMissing),
            char_literal: ("'c'", TokenKind::CharLiteral),
            char_literal_multiple_char: ("'ch", TokenKind::CharLiteralMissingRight),
            char_literal_missing_right_single_quote: ("'c", TokenKind::CharLiteralMissingRight),
            char_literal_single_quote_only: ("'", TokenKind::CharLiteralMissingRight),
            colon: (":", TokenKind::Colon),
            colon_colon: ("::", TokenKind::ColonColon),
            comma: (",", TokenKind::Comma),
            dot: (".", TokenKind::Dot),
            eq: ("=", TokenKind::Eq),
            eqeq: ("==", TokenKind::EqEq),
            exclamation: ("!", TokenKind::Exclamation),
            fat_right_arrow: ("=>", TokenKind::FatRightArrow),
            float_full: ("3.14", TokenKind::Float),
            float_no_left: (".14", TokenKind::Float),
            float_no_right: ("3.", TokenKind::Float),
            greater_than: (">", TokenKind::GreaterThan),
            greater_than_or_eq: (">=", TokenKind::GreaterThanOrEq),
            identifier: ("l0n9_id3nt1f13r", TokenKind::Identifier),
            identifier_first_digit: ("9l0n9_id3nt1f13r", TokenKind::IdentifierCannotBegin),
            integer: ("314", TokenKind::Integer),
            kw_break: ("break", TokenKind::KwBreak),
            kw_buffer: ("buffer", TokenKind::KwBuffer),
            kw_const: ("const", TokenKind::KwConst),
            kw_continue: ("continue", TokenKind::KwContinue),
            kw_elif: ("elif", TokenKind::KwElif),
            kw_else: ("else", TokenKind::KwElse),
            kw_enum: ("enum", TokenKind::KwEnum),
            kw_false: ("false", TokenKind::KwFalse),
            kw_fn: ("fn", TokenKind::KwFn),
            kw_for: ("for", TokenKind::KwFor),
            // kw_go: ("go", TokenKind::KwGo),
            kw_if: ("if", TokenKind::KwIf),
            kw_impl: ("impl", TokenKind::KwImpl),
            kw_import: ("import", TokenKind::KwImport),
            kw_in: ("in", TokenKind::KwIn),
            kw_let: ("let", TokenKind::KwLet),
            kw_match: ("match", TokenKind::KwMatch),
            kw_module: ("mod", TokenKind::KwModule),
            kw_mut: ("mut", TokenKind::KwMut),
            // kw_package: ("pkg", TokenKind::KwPackage),
            // kw_pub: ("pub", TokenKind::KwPub),
            kw_return: ("return", TokenKind::KwReturn),
            kw_self: ("Self", TokenKind::KwSelf),
            // kw_static: ("static", TokenKind::KwStatic),
            kw_struct: ("struct", TokenKind::KwStruct),
            // kw_trait: ("trait", TokenKind::KwTrait),
            kw_tensor: ("tensor", TokenKind::KwTensor),
            kw_true: ("true", TokenKind::KwTrue),
            kw_type: ("type", TokenKind::KwType),
            kw_while: ("while", TokenKind::KwWhile),
            // kw_yield: ("yield", TokenKind::KwYield),
            // kw_zk: ("zk", TokenKind::KwZk),
            kwself: ("self", TokenKind::Kwself),
            left_arrow: ("<-", TokenKind::LeftArrow),
            left_brace: ("{", TokenKind::LeftBrace),
            left_paren: ("(", TokenKind::LeftParen),
            left_shift: ("<<", TokenKind::LeftShift),
            left_shift_eq: ("<<=", TokenKind::LeftShiftEq),
            left_square_brac: ("[", TokenKind::LeftSquareBrac),
            less_than: ("<", TokenKind::LessThan),
            less_than_or_eq: ("<=", TokenKind::LessThanOrEq),
            line_comment: ("// line comment \n", TokenKind::LineComment),
            line_comment_no_newline: ("// line comment ", TokenKind::LineComment),
            minus: ("-", TokenKind::Minus),
            minus_eq: ("-=", TokenKind::MinusEq),
            modulus: ("%", TokenKind::Percent),
            modulus_eq: ("%=", TokenKind::PercentEq),
            newline: ("\n", TokenKind::Newline),
            two_newlines: ("\n\n", TokenKind::Newline),
            not_eq: ("!=", TokenKind::NotEq),
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
            space: (" ", TokenKind::Space),
            three_spaces: ("   ", TokenKind::Space),
            star: ("*", TokenKind::Star),
            star_eq: ("*=", TokenKind::StarEq),
            string_literal: ("\"string literal.\"", TokenKind::StringLiteral),
            string_literal_missing_right_double_quote: ("\"string literal.", TokenKind::StringLiteralMissingRightDoubleQuote),
            string_literal_missing_rigith_double_quote_with_a_single_quote_following: ("\" '", TokenKind::StringLiteralMissingRightDoubleQuote),
            string_literal_only_one_double_quote: ("\"", TokenKind::StringLiteralMissingRightDoubleQuote),
            tab: ("\t", TokenKind::Tab),
            two_tabs: ("\t\t", TokenKind::Tab),
            // type_byte: ("byte", TokenKind::TypeByte),
            type_bool: ("bool", TokenKind::TypeBool),
            type_char: ("char", TokenKind::TypeChar),
            type_f32: ("f32", TokenKind::TypeF32),
            // type_f64: ("f64", TokenKind::TypeF64),
            type_i32: ("i32", TokenKind::TypeI32),
            // type_i64: ("i64", TokenKind::TypeI64),
            type_string: ("str", TokenKind::TypeStr),
            // type_u32: ("u32", TokenKind::TypeU32),
            // type_u64: ("u64", TokenKind::TypeU64),
            underscore: ("_", TokenKind::Underscore),
            xor: ("^", TokenKind::Xor),

    }
    create_lexer_test_multiple_tokens_exp! {
        block_comment_multiple_line_comments_within: ("/* // nested comments // */", &[
            Token {
            kind: TokenKind::BlockComment,
            ttype: TokenKind::BlockComment.into(),
            span: 0..27,
            slice: None,
            },
        ]),
        block_comment_right_star_missing: ("/* /////", &[
            Token {
            kind: TokenKind::BlockCommentRightStarMissing,
            ttype: TokenKind::BlockCommentRightStarMissing.into(),
            span: 0..8,
            slice: None,
            },
        ]),
        block_comment_nested: ("/* /* nested block comment */ */", &[
            Token {
            kind: TokenKind::BlockComment,
            ttype: TokenKind::BlockComment.into(),
            span: 0..29,
            slice: None,
            },
            Token{
            kind: TokenKind::Space,
            ttype: TokenKind::Space.into(),
            span: 29..30,
            slice: None,
            },
            Token{
            kind: TokenKind::Star,
            ttype: TokenKind::Star.into(),
            span: 30..31,
            slice: None,
            },
            Token{
            kind: TokenKind::Slash,
            ttype: TokenKind::Slash.into(),
            span: 31..32,
            slice: None,
            },
        ]),
        float_with_two_dots: ("3..14", &[
            Token {
            kind: TokenKind::Float,
            ttype: TokenKind::Float.into(),
            span: 0..2,
            slice: None,
            },
            Token {
            kind: TokenKind::Float,
            ttype: TokenKind::Float.into(),
            span: 2..5,
            slice: None,
            },
        ]),

        integer_space_in_between: ("3 1", &[
            Token{
            kind: TokenKind::Integer,
            ttype: TokenKind::Integer.into(),
            span: 0..1,
            slice: None,
            },
            Token{
            kind: TokenKind::Space,
            ttype: TokenKind::Space.into(),
            span: 1..2,
            slice: None,
            },
            Token{
            kind: TokenKind::Integer,
            ttype: TokenKind::Integer.into(),
            span: 2..3,
            slice: None,
            },
        ]),
        integer_comma_in_between: ("3,1", &[
            Token{
            kind: TokenKind::Integer,
            ttype: TokenKind::Integer.into(),
            span: 0..1,
            slice: None,
            },
            Token{
            kind: TokenKind::Comma,
            ttype: TokenKind::Comma.into(),
            span: 1..2,
            slice: None,
            },
            Token{
            kind: TokenKind::Integer,
            ttype: TokenKind::Integer.into(),
            span: 2..3,
            slice: None,
            },
        ]),
        integer_single_quote_in_between: ("3'1", &[
            Token{
            kind: TokenKind::Integer,
            ttype: TokenKind::Integer.into(),
            span: 0..1,
            slice: None,
            },
            Token{
            kind: TokenKind::CharLiteralMissingRight,
            ttype: TokenKind::CharLiteralMissingRight.into(),
            span: 1..3,
            slice: None,
            },
        ]),
        underscored_identifier: ("_ident", &[
            Token{
            kind: TokenKind::Underscore,
            ttype: TokenKind::Underscore.into(),
            span: 0..1,
            slice: None,
            },
            Token{
            kind: TokenKind::Identifier,
            ttype: TokenKind::Identifier.into(),
            span: 1..6,
            slice: None,
            },
        ]),
        member_and_two_bin_op: ("h.w+1/2", &[
            Token{
            kind: TokenKind::Identifier,
            ttype: TokenKind::Identifier.into(),
            span: 0..1,
            slice: None,
            },
            Token{
            kind: TokenKind::Dot,
            ttype: TokenKind::Dot.into(),
            span: 1..2,
            slice: None,
            },
            Token{
            kind: TokenKind::Identifier,
            ttype: TokenKind::Identifier.into(),
            span: 2..3,
            slice: None,
            },
            Token{
            kind: TokenKind::Plus,
            ttype: TokenKind::Plus.into(),
            span: 3..4,
            slice: None,
            },
            Token{
            kind: TokenKind::Integer,
            ttype: TokenKind::Integer.into(),
            span: 4..5,
            slice: None,
            },
            Token{
            kind: TokenKind::Slash,
            ttype: TokenKind::Slash.into(),
            span: 5..6,
            slice: None,
            },
            Token{
            kind: TokenKind::Integer,
            ttype: TokenKind::Integer.into(),
            span: 6..7,
            slice: None,
            },
        ]),
    }
}
