use crate::{
    parse::Finished,
    parser::{IsNext, Parser},
};

use lexer::token_type::TokenType;
use syntax::{Syntax, syntax_kind::SyntaxKind::*};

#[allow(unused_variables)]
impl Parser<'_> {
    #[allow(unused_variables)] // for rollback anchor
    pub fn parse_delimited(&self, syntax: Syntax) -> Option<Finished> {
        let kind = syntax.get_kind();
        if !self.is_allowed(kind) {
            // note: returns None thus shortcircuits from here
            self.recover_restricted(kind)?;
        }
        use TokenType::*;
        match syntax.get_token_type() {
            OpeningDelimiter(expected_token_kind) => match kind {
                LParen => self.parse_parenthesised(),
                LBrace => self.parse_block(),
                LBrack => {
                    if self.is_expected(ContainerRef) {
                        self.parse_container_indexing();
                    } else {
                        self.parse_buffer_literal();
                    }
                    None
                }
                _ => unreachable!(),
            },
            ClosingDelimiter(expected_token_kind) => None,
            _ => unreachable!(),
        }
    }

    #[allow(unused_variables)] // for rollback anchor
    pub fn parse_block(&self) -> Option<Finished> {
        let marker = self.start();
        {
            let rollback_when_dropped = self.impose_context_for_parsing(Block);
            self.expect_and_bump(LBrace);
            while IsNext::No == self.is_next_strict(RBrace) && self.parse_statement().is_some() {}
        }

        self.expect_and_bump(RBrace);
        Some(self.complete_marker_with(marker, Block))
    }
}

#[cfg(test)]
mod tests {
    use crate::parse::tests::check;
    use expect_test::expect;
    use parameterized_test::create;

    create! {
        create_parser_test,
        (prog, expect), {
            check(prog, expect);
        }
    }
    create_parser_test! {
        empty_block: ("{}",
            expect![[r#"
                Root@0..2
                  Block@0..2
                    LBrace@0..1 "{"
                    RBrace@1..2 "}""#]],
        ),
        block_with_returning: ("{let a = 0; a}",
            expect![[r#"
                Root@0..14
                  Block@0..14
                    LBrace@0..1 "{"
                    LetBinding@1..11
                      KwLet@1..4 "let"
                      Whitespace@4..5 " "
                      InfixBinOp@5..10
                        VarRef@5..7
                          Ident@5..6 "a"
                          Whitespace@6..7 " "
                        Eq@7..8 "="
                        Whitespace@8..9 " "
                        Literal@9..10
                          Int@9..10 "0"
                      Semi@10..11 ";"
                    Whitespace@11..12 " "
                    VarRef@12..13
                      Ident@12..13 "a"
                    RBrace@13..14 "}""#]],
        ),

        block_with_multiple_statements_and_expressions: ("{let a = 0;\n let b = 1; let c= 2; a+b+c}",
            expect![[r#"
                Root@0..40
                  Block@0..40
                    LBrace@0..1 "{"
                    LetBinding@1..11
                      KwLet@1..4 "let"
                      Whitespace@4..5 " "
                      InfixBinOp@5..10
                        VarRef@5..7
                          Ident@5..6 "a"
                          Whitespace@6..7 " "
                        Eq@7..8 "="
                        Whitespace@8..9 " "
                        Literal@9..10
                          Int@9..10 "0"
                      Semi@10..11 ";"
                    Whitespace@11..12 "\n"
                    Whitespace@12..13 " "
                    LetBinding@13..23
                      KwLet@13..16 "let"
                      Whitespace@16..17 " "
                      InfixBinOp@17..22
                        VarRef@17..19
                          Ident@17..18 "b"
                          Whitespace@18..19 " "
                        Eq@19..20 "="
                        Whitespace@20..21 " "
                        Literal@21..22
                          Int@21..22 "1"
                      Semi@22..23 ";"
                    Whitespace@23..24 " "
                    LetBinding@24..33
                      KwLet@24..27 "let"
                      Whitespace@27..28 " "
                      InfixBinOp@28..32
                        VarRef@28..29
                          Ident@28..29 "c"
                        Eq@29..30 "="
                        Whitespace@30..31 " "
                        Literal@31..32
                          Int@31..32 "2"
                      Semi@32..33 ";"
                    Whitespace@33..34 " "
                    InfixBinOp@34..39
                      InfixBinOp@34..37
                        VarRef@34..35
                          Ident@34..35 "a"
                        Plus@35..36 "+"
                        VarRef@36..37
                          Ident@36..37 "b"
                      Plus@37..38 "+"
                      VarRef@38..39
                        Ident@38..39 "c"
                    RBrace@39..40 "}""#]],
        ),
        block_with_self: ("{ self.x += by.x; self.y += by.y;}",
            expect![[r#"
                Root@0..34
                  Block@0..34
                    LBrace@0..1 "{"
                    Whitespace@1..2 " "
                    Semi@2..17
                      InfixBinOp@2..16
                        InfixBinOp@2..9
                          SelfRef@2..6
                            Kwself@2..6 "self"
                          Dot@6..7 "."
                          VarRef@7..9
                            Ident@7..8 "x"
                            Whitespace@8..9 " "
                        PlusEq@9..11 "+="
                        Whitespace@11..12 " "
                        InfixBinOp@12..16
                          VarRef@12..14
                            Ident@12..14 "by"
                          Dot@14..15 "."
                          VarRef@15..16
                            Ident@15..16 "x"
                      Semi@16..17 ";"
                    Whitespace@17..18 " "
                    Semi@18..33
                      InfixBinOp@18..32
                        InfixBinOp@18..25
                          SelfRef@18..22
                            Kwself@18..22 "self"
                          Dot@22..23 "."
                          VarRef@23..25
                            Ident@23..24 "y"
                            Whitespace@24..25 " "
                        PlusEq@25..27 "+="
                        Whitespace@27..28 " "
                        InfixBinOp@28..32
                          VarRef@28..30
                            Ident@28..30 "by"
                          Dot@30..31 "."
                          VarRef@31..32
                            Ident@31..32 "y"
                      Semi@32..33 ";"
                    RBrace@33..34 "}""#]],
        ),
    }
}
