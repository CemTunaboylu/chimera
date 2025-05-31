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
                LParen => self.parse_paren_expr(syntax),
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
        only_parenthesis: ("()",
            expect![[
                "Root@0..2\n  ParenExpr@0..2\n    LParen@0..1 \"(\"\n    RParen@1..2 \")\""
            ]]
        ),

        missing_closing_parenthesis_does_not_panic: ("(",
            expect![[
                "Root@0..1\n  ParenExpr@0..1\n    LParen@0..1 \"(\""
            ]]
        ),
        parenthesised_sub_precedes_mul: ("(3+1)*4",
            expect![[
                "Root@0..7\n  InfixBinOp@0..7\n    ParenExpr@0..5\n      LParen@0..1 \"(\"\n      InfixBinOp@1..4\n        Literal@1..2\n          Int@1..2 \"3\"\n        Plus@2..3 \"+\"\n        Literal@3..4\n          Int@3..4 \"1\"\n      RParen@4..5 \")\"\n    Star@5..6 \"*\"\n    Literal@6..7\n      Int@6..7 \"4\""
            ]]
        ),
        parenthesised_var_def: ("let z = (3+1);",
            expect![[r#"
                Root@0..14
                  VarDef@0..14
                    KwLet@0..3 "let"
                    Whitespace@3..4 " "
                    InfixBinOp@4..13
                      VarRef@4..6
                        Ident@4..5 "z"
                        Whitespace@5..6 " "
                      Eq@6..7 "="
                      Whitespace@7..8 " "
                      ParenExpr@8..13
                        LParen@8..9 "("
                        InfixBinOp@9..12
                          Literal@9..10
                            Int@9..10 "3"
                          Plus@10..11 "+"
                          Literal@11..12
                            Int@11..12 "1"
                        RParen@12..13 ")"
                    Semi@13..14 ";""#]]
        ),

        misplaced_parenthesised_var_def: ("let z = (3;1);",
            expect![[r#"
                Root@0..13
                  VarDef@0..12
                    KwLet@0..3 "let"
                    Whitespace@3..4 " "
                    InfixBinOp@4..11
                      VarRef@4..6
                        Ident@4..5 "z"
                        Whitespace@5..6 " "
                      Eq@6..7 "="
                      Whitespace@7..8 " "
                      ParenExpr@8..11
                        LParen@8..9 "("
                        Literal@9..10
                          Int@9..10 "3"
                        Recovered@10..11
                          Semi@10..11 ";"
                    Recovered@11..12
                      Int@11..12 "1"
                  Recovered@12..13
                    RParen@12..13 ")""#]]
        ),

        misplaced_parenthesised_var_def_with_fn_call: ("let z = (something(););",
            expect![[r#"
                Root@0..23
                  Semi@0..23
                    VarDef@0..22
                      KwLet@0..3 "let"
                      Whitespace@3..4 " "
                      InfixBinOp@4..21
                        VarRef@4..6
                          Ident@4..5 "z"
                          Whitespace@5..6 " "
                        Eq@6..7 "="
                        Whitespace@7..8 " "
                        ParenExpr@8..21
                          LParen@8..9 "("
                          Call@9..20
                            Ident@9..18 "something"
                            LParen@18..19 "("
                            RParen@19..20 ")"
                          Recovered@20..21
                            Semi@20..21 ";"
                      Recovered@21..22
                        RParen@21..22 ")"
                    Semi@22..23 ";""#]]
        ),

        assignment_during_var_def: ("let z = (self.x += 1) + 2;",
            expect![[r#"
                Root@0..26
                  VarDef@0..21
                    KwLet@0..3 "let"
                    Whitespace@3..4 " "
                    InfixBinOp@4..20
                      VarRef@4..6
                        Ident@4..5 "z"
                        Whitespace@5..6 " "
                      Eq@6..7 "="
                      Whitespace@7..8 " "
                      ParenExpr@8..20
                        LParen@8..9 "("
                        InfixBinOp@9..18
                          SelfRef@9..13
                            Kwself@9..13 "self"
                          Dot@13..14 "."
                          VarRef@14..16
                            Ident@14..15 "x"
                            Whitespace@15..16 " "
                          Recovered@16..18
                            PlusEq@16..18 "+="
                        Whitespace@18..19 " "
                        Recovered@19..20
                          Int@19..20 "1"
                    Recovered@20..21
                      RParen@20..21 ")"
                  Whitespace@21..22 " "
                  Recovered@22..23
                    Plus@22..23 "+"
                  Whitespace@23..24 " "
                  Semi@24..26
                    Literal@24..25
                      Int@24..25 "2"
                    Semi@25..26 ";""#]]
        ),


        parenthesised_pi: ("((314))",
            expect![[
                "Root@0..7\n  ParenExpr@0..7\n    LParen@0..1 \"(\"\n    ParenExpr@1..6\n      LParen@1..2 \"(\"\n      Literal@2..5\n        Int@2..5 \"314\"\n      RParen@5..6 \")\"\n    RParen@6..7 \")\""
            ]]
        ),
        parenthesised_pi_with_semicolon: ("((314));",
            expect![[r#"
                Root@0..8
                  Semi@0..8
                    ParenExpr@0..7
                      LParen@0..1 "("
                      ParenExpr@1..6
                        LParen@1..2 "("
                        Literal@2..5
                          Int@2..5 "314"
                        RParen@5..6 ")"
                      RParen@6..7 ")"
                    Semi@7..8 ";""#]]
        ),
        parenthesised_pi_with_semicolon_inside: ("((314;));",
            expect![[r#"
                Root@0..8
                  ParenExpr@0..7
                    LParen@0..1 "("
                    ParenExpr@1..6
                      LParen@1..2 "("
                      Literal@2..5
                        Int@2..5 "314"
                      Recovered@5..6
                        Semi@5..6 ";"
                    RParen@6..7 ")"
                  Recovered@7..8
                    RParen@7..8 ")""#]]
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
