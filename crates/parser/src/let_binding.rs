use crate::{
    operator::starting_precedence,
    parse::Finished,
    parser::{IsNext, Parser},
};

use syntax::syntax_kind::SyntaxKind::*;

impl Parser<'_> {
    #[allow(unused_variables)]
    pub fn parse_let_binding(&self) -> Option<Finished> {
        let marker = self.start();
        self.expect_and_bump(KwLet);

        let rollback_when_dropped = self.impose_context_for_parsing(LetBinding);

        if IsNext::Yes == self.is_next_strict(LParen) {
            self.expect_in_ctx([Tuple, LetBinding].as_ref());
        }

        if self
            .parse_expression_until_binding_power(starting_precedence())
            .is_none()
        {
            let got = self.peek();
            self.recover_with_msg("expected a valid assignment", got);
        }
        self.expect_and_bump(Semi);
        Some(self.complete_marker_with(marker, LetBinding))
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
        malformed_var_defs: ("let a = let b = let c = 5;",
            expect![[r#"
                Root@0..26
                  LetBinding@0..8
                    KwLet@0..3 "let"
                    Whitespace@3..4 " "
                    InfixBinOp@4..8
                      VarRef@4..6
                        Ident@4..5 "a"
                        Whitespace@5..6 " "
                      Eq@6..7 "="
                      Whitespace@7..8 " "
                  LetBinding@8..16
                    KwLet@8..11 "let"
                    Whitespace@11..12 " "
                    InfixBinOp@12..16
                      VarRef@12..14
                        Ident@12..13 "b"
                        Whitespace@13..14 " "
                      Eq@14..15 "="
                      Whitespace@15..16 " "
                  LetBinding@16..26
                    KwLet@16..19 "let"
                    Whitespace@19..20 " "
                    InfixBinOp@20..25
                      VarRef@20..22
                        Ident@20..21 "c"
                        Whitespace@21..22 " "
                      Eq@22..23 "="
                      Whitespace@23..24 " "
                      Literal@24..25
                        Int@24..25 "5"
                    Semi@25..26 ";""#]]
        ),

        moving_var_def:    ("let foo = bar",
            expect![[r#"
                Root@0..13
                  LetBinding@0..13
                    KwLet@0..3 "let"
                    Whitespace@3..4 " "
                    InfixBinOp@4..13
                      VarRef@4..8
                        Ident@4..7 "foo"
                        Whitespace@7..8 " "
                      Eq@8..9 "="
                      Whitespace@9..10 " "
                      VarRef@10..13
                        Ident@10..13 "bar""#]]),
        type_hinted_var_def:    ("let foo : Important = bar",
            expect![[r#"
                Root@0..25
                  LetBinding@0..25
                    KwLet@0..3 "let"
                    Whitespace@3..4 " "
                    InfixBinOp@4..25
                      VarRef@4..19
                        Ident@4..7 "foo"
                        Whitespace@7..8 " "
                        Colon@8..9 ":"
                        TypeHint@9..19
                          Whitespace@9..10 " "
                          StructAsType@10..19 "Important"
                      Whitespace@19..20 " "
                      Eq@20..21 "="
                      Whitespace@21..22 " "
                      VarRef@22..25
                        Ident@22..25 "bar""#]]),
        fn_type_hinted_var_def:    ("let foo : fn(i32)->i32 = bar",
            expect![[r#"
                Root@0..28
                  LetBinding@0..28
                    KwLet@0..3 "let"
                    Whitespace@3..4 " "
                    InfixBinOp@4..28
                      VarRef@4..22
                        Ident@4..7 "foo"
                        Whitespace@7..8 " "
                        Colon@8..9 ":"
                        TypeHint@9..22
                          Whitespace@9..10 " "
                          TyFn@10..22
                            KwFn@10..12 "fn"
                            LParen@12..13 "("
                            ParamDecl@13..16
                              TyI32@13..16 "i32"
                            RParen@16..17 ")"
                            RetType@17..22
                              RArrow@17..19 "->"
                              TyI32@19..22 "i32"
                      Whitespace@22..23 " "
                      Eq@23..24 "="
                      Whitespace@24..25 " "
                      VarRef@25..28
                        Ident@25..28 "bar""#]]),

        mut_moving_var_def:    ("let mut foo : i32 = bar",
            expect![[r#"
                Root@0..23
                  LetBinding@0..23
                    KwLet@0..3 "let"
                    Whitespace@3..4 " "
                    Mut@4..23
                      KwMut@4..7 "mut"
                      Whitespace@7..8 " "
                      InfixBinOp@8..23
                        VarRef@8..17
                          Ident@8..11 "foo"
                          Whitespace@11..12 " "
                          Colon@12..13 ":"
                          TypeHint@13..17
                            Whitespace@13..14 " "
                            TyI32@14..17 "i32"
                        Whitespace@17..18 " "
                        Eq@18..19 "="
                        Whitespace@19..20 " "
                        VarRef@20..23
                          Ident@20..23 "bar""#]]),
        do_not_recover_on_let_token: (
            "let a =\nlet b = a;",
            expect![[r#"
                Root@0..18
                  LetBinding@0..8
                    KwLet@0..3 "let"
                    Whitespace@3..4 " "
                    InfixBinOp@4..8
                      VarRef@4..6
                        Ident@4..5 "a"
                        Whitespace@5..6 " "
                      Eq@6..7 "="
                      Whitespace@7..8 "\n"
                  LetBinding@8..18
                    KwLet@8..11 "let"
                    Whitespace@11..12 " "
                    InfixBinOp@12..17
                      VarRef@12..14
                        Ident@12..13 "b"
                        Whitespace@13..14 " "
                      Eq@14..15 "="
                      Whitespace@15..16 " "
                      VarRef@16..17
                        Ident@16..17 "a"
                    Semi@17..18 ";""#]]),

        do_not_recover_on_let_token_with_semicolon: (
            "let a =;let b = a;",
            expect![[r#"
                Root@0..18
                  LetBinding@0..8
                    KwLet@0..3 "let"
                    Whitespace@3..4 " "
                    InfixBinOp@4..7
                      VarRef@4..6
                        Ident@4..5 "a"
                        Whitespace@5..6 " "
                      Eq@6..7 "="
                    Semi@7..8 ";"
                  LetBinding@8..18
                    KwLet@8..11 "let"
                    Whitespace@11..12 " "
                    InfixBinOp@12..17
                      VarRef@12..14
                        Ident@12..13 "b"
                        Whitespace@13..14 " "
                      Eq@14..15 "="
                      Whitespace@15..16 " "
                      VarRef@16..17
                        Ident@16..17 "a"
                    Semi@17..18 ";""#]]
        ),
        do_not_recover_on_let_token_in_block: (
            "{let a =\n{let b =} 10 }",
            expect![[r#"
                Root@0..23
                  Block@0..23
                    LBrace@0..1 "{"
                    LetBinding@1..21
                      KwLet@1..4 "let"
                      Whitespace@4..5 " "
                      InfixBinOp@5..19
                        VarRef@5..7
                          Ident@5..6 "a"
                          Whitespace@6..7 " "
                        Eq@7..8 "="
                        Whitespace@8..9 "\n"
                        Block@9..18
                          LBrace@9..10 "{"
                          LetBinding@10..17
                            KwLet@10..13 "let"
                            Whitespace@13..14 " "
                            InfixBinOp@14..17
                              VarRef@14..16
                                Ident@14..15 "b"
                                Whitespace@15..16 " "
                              Eq@16..17 "="
                          RBrace@17..18 "}"
                        Whitespace@18..19 " "
                      Recovered@19..21
                        Int@19..21 "10"
                    Whitespace@21..22 " "
                    RBrace@22..23 "}""#]],
        ),
        do_not_recover_on_let_token_in_block_with_semicolon: (
            "{let a =\n{let b =}; 10 }",
            expect![[r#"
                Root@0..24
                  Block@0..24
                    LBrace@0..1 "{"
                    LetBinding@1..19
                      KwLet@1..4 "let"
                      Whitespace@4..5 " "
                      InfixBinOp@5..18
                        VarRef@5..7
                          Ident@5..6 "a"
                          Whitespace@6..7 " "
                        Eq@7..8 "="
                        Whitespace@8..9 "\n"
                        Block@9..18
                          LBrace@9..10 "{"
                          LetBinding@10..17
                            KwLet@10..13 "let"
                            Whitespace@13..14 " "
                            InfixBinOp@14..17
                              VarRef@14..16
                                Ident@14..15 "b"
                                Whitespace@15..16 " "
                              Eq@16..17 "="
                          RBrace@17..18 "}"
                      Semi@18..19 ";"
                    Whitespace@19..20 " "
                    Literal@20..22
                      Int@20..22 "10"
                    Whitespace@22..23 " "
                    RBrace@23..24 "}""#]],
        ),
    }
}
