use std::ops::Range;

use miette::{Diagnostic, Result as MietteResult, SourceSpan};
use thiserror::Error;

use crate::token_kind::TokenKind;

use logos::Logos;

#[derive(Clone, Default, Diagnostic, Debug, PartialEq, Error)]
#[diagnostic()]
#[error("LexError")]
pub struct LexError {
    #[source_code]
    src: String,

    #[label = "Here"]
    err_span: Range<usize>,

    #[label("Related")]
    related: Option<SourceSpan>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Range<usize>,
}

pub type LexResult = MietteResult<Token, LexError>;
#[derive(Debug)]
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
}

impl Iterator for Lexer<'_> {
    type Item = LexResult;

    fn next(&mut self) -> Option<Self::Item> {
        if self.peeked.is_some() {
            return self.peeked.take().unwrap();
        }
        let result = match self.inner_lexer.next()? {
            Ok(kind) => Ok(Token {
                kind,
                span: self.inner_lexer.span(),
            }),
            Err(_) => {
                let src = self.inner_lexer.source().to_string();
                let err_span = self.inner_lexer.span();
                Err(LexError {
                    src,
                    err_span,
                    related: None,
                })
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
        let program = "1 2";
        let mut lexer = Lexer::new(program);
        assert_eq!(
            Some(&Ok(Token {
                kind: TokenKind::Number,
                span: 0..1
            })),
            lexer.peek()
        );

        assert_eq!(
            Some(Ok(Token {
                kind: TokenKind::Number,
                span: 0..1
            })),
            lexer.next()
        );

        assert_eq!(
            Some(&Ok(Token {
                kind: TokenKind::Space,
                span: 1..2
            })),
            lexer.peek()
        );
    }

    fn assert_token_kind(with: &str, kind: TokenKind) {
        let mut lexer = Lexer::new(with);
        let result = lexer.next().unwrap();
        let span = 0..with.len();
        let token = Token { kind, span };
        assert_eq!(token, result.expect("expected a tokenkind"));
    }

    create! {
        create_lexer_test,
        (prog, expected_kind), {
            assert_token_kind(prog, expected_kind);
        }
    }
    create_lexer_test! {
            and: ("&", TokenKind::And),
            andand: ("&&", TokenKind::AndAnd),
            andeq: ("&=", TokenKind::AndEq),
            // as_: ("as", TokenKind::As),
            attribute: ("@", TokenKind::Attribute),
            block_comment: ("/* block comment */", TokenKind::BlockComment),
            char_literal: ("'c'", TokenKind::CharLiteral),
            colon: (":", TokenKind::Colon),
            comma: (",", TokenKind::Comma),
            dot: (".", TokenKind::Dot),
            dotdot: ("..", TokenKind::DotDot),
            dotdotdot: ("...", TokenKind::DotDotDot),
            // dotdoteq: ("..=", TokenKind::DotDotEq),
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
            // kw_go: ("go", TokenKind::KwGo),
            kw_if: ("if", TokenKind::KwIf),
            // kw_impl: ("impl", TokenKind::KwImpl),
            kw_import: ("import", TokenKind::KwImport),
            kw_in: ("in", TokenKind::KwIn),
            kw_let: ("let", TokenKind::KwLet),
            kw_module: ("mod", TokenKind::KwModule),
            kw_mut: ("mut", TokenKind::KwMut),
            // kw_package: ("pkg", TokenKind::KwPackage),
            // kw_pub: ("pub", TokenKind::KwPub),
            kw_return: ("return", TokenKind::KwReturn),
            kw_self: ("Self", TokenKind::KwSelf),
            // kw_static: ("static", TokenKind::KwStatic),
            kw_struct: ("struct", TokenKind::KwStruct),
            // kw_trait: ("trait", TokenKind::KwTrait),
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
            line_comment: ("// line comment \n", TokenKind::LineComment),
            line_comment_no_newline: ("// line comment ", TokenKind::LineComment),
            minus: ("-", TokenKind::Minus),
            minus_eq: ("-=", TokenKind::MinusEq),
            modulus: ("%", TokenKind::Modulus),
            modulus_eq: ("%=", TokenKind::ModulusEq),
            namespace_sep: ("::", TokenKind::NamespaceSep),
            newline: ("\n", TokenKind::Newline),
            two_newlines: ("\n\n", TokenKind::Newline),
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
            space: (" ", TokenKind::Space),
            three_spaces: ("   ", TokenKind::Space),
            star: ("*", TokenKind::Star),
            star_eq: ("*=", TokenKind::StarEq),
            string_literal: ("\"string literal.\"", TokenKind::StringLiteral),
            tab: ("\t", TokenKind::Tab),
            two_tabs: ("\t\t", TokenKind::Tab),
            type_byte: ("byte", TokenKind::TypeByte),
            type_bool: ("bool", TokenKind::TypeBool),
            type_char: ("char", TokenKind::TypeChar),
            type_f32: ("f32", TokenKind::TypeF32),
            // type_f64: ("f64", TokenKind::TypeF64),
            type_i32: ("i32", TokenKind::TypeI32),
            // type_i64: ("i64", TokenKind::TypeI64),
            type_str_slice: ("str", TokenKind::TypeStrSlice),
            type_string: ("String", TokenKind::TypeString),
            type_u32: ("u32", TokenKind::TypeU32),
            // type_u64: ("u64", TokenKind::TypeU64),
            // underscore: ("_", TokenKind::Underscore),

    }
}
