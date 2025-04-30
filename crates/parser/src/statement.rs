use crate::{operator::starting_precedence, parse::Finished, parser::Parser};

use syntax::{
    anchor::RollingBackAnchor,
    bitset::SyntaxKindBitSet,
    non_assigning_operators,
    syntax_kind::SyntaxKind::{self, *},
};

impl Parser<'_> {
    pub fn parse_statement(&self) -> Option<Finished> {
        let mut syntax = self.peek()?;
        while let Err(err) = syntax {
            self.recover_from_err(err);
            syntax = self.peek()?;
        }
        let marker = match self.peek()? {
            Ok(syntax) if syntax.is_of_kind(KwFn) => self.parse_function_def(),
            Ok(syntax) if syntax.is_of_kind(KwFor) => self.parse_for_loop(),
            Ok(syntax) if syntax.is_of_kind(KwIf) => self.parse_conditionals(),
            Ok(syntax) if syntax.is_of_kind(KwImpl) => self.parse_impl_block(),
            Ok(syntax) if syntax.is_of_kind(KwLet) => self.parse_variable_def(),
            Ok(syntax) if syntax.is_of_kind(KwReturn) => self.parse_return(),
            Ok(syntax) if syntax.is_of_kind(KwStruct) => self.parse_struct_definition(),
            Ok(syntax) if syntax.is_of_kind(KwWhile) => self.parse_while_loop(),
            // An expression produces a result (result of evalution), but if there is a ; at the end,
            // it becomes a statement, thus check that here and wrap it with Semi
            _ => self.parse_expression_until_binding_power(starting_precedence()),
        };
        if self.is_next(Semi) {
            if let Some(m) = marker {
                let semi = self.precede_marker_with(&m);
                self.bump();
                return Some(self.complete_marker_with(semi, Semi));
            } else {
                self.ignore();
            }
        }
        marker
    }

    fn impose_var_def_restrictions(&self) -> RollingBackAnchor {
        let rollback_when_dropped = self.roll_back_context_after_drop();
        self.expect_in_ctx([Ident, Semi, VarDef, Eq].as_ref());
        let ctx = self.context.borrow();
        ctx.forbid_all();
        let non_assignments: SyntaxKindBitSet = non_assigning_operators();
        ctx.allow(non_assignments);
        ctx.allow(SyntaxKind::can_be_parameter().as_ref());
        ctx.allow(SyntaxKind::opening_delimiters().as_ref());
        ctx.allow([Eq, Ident, Semi, VarDef, VarRef, SelfRef, StructLit].as_ref());
        rollback_when_dropped
    }

    #[allow(unused_variables)]
    pub fn parse_variable_def(&self) -> Option<Finished> {
        let marker = self.start();
        self.expect_and_bump(KwLet);

        let rollback_when_dropped = self.impose_var_def_restrictions();

        if self
            .parse_expression_until_binding_power(starting_precedence())
            .is_none()
        {
            let got = self.peek();
            self.recover_with_msg("expected a valid assignment", got);
        }
        self.expect_and_bump(Semi);
        Some(self.complete_marker_with(marker, VarDef))
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
        return_stmt: ("return 3+14/100;", expect![[r#"
            Root@0..16
              Return@0..16
                KwReturn@0..6 "return"
                Whitespace@6..7 " "
                InfixBinOp@7..15
                  Literal@7..8
                    Int@7..8 "3"
                  Plus@8..9 "+"
                  InfixBinOp@9..15
                    Literal@9..11
                      Int@9..11 "14"
                    Slash@11..12 "/"
                    Literal@12..15
                      Int@12..15 "100"
                Semi@15..16 ";""#]]),
        malformed_var_defs: ("let a = let b = let c = 5;",
            expect![[r#"
                Root@0..26
                  VarDef@0..8
                    KwLet@0..3 "let"
                    Whitespace@3..4 " "
                    InfixBinOp@4..8
                      VarRef@4..6
                        Ident@4..5 "a"
                        Whitespace@5..6 " "
                      Eq@6..7 "="
                      Whitespace@7..8 " "
                  VarDef@8..16
                    KwLet@8..11 "let"
                    Whitespace@11..12 " "
                    InfixBinOp@12..16
                      VarRef@12..14
                        Ident@12..13 "b"
                        Whitespace@13..14 " "
                      Eq@14..15 "="
                      Whitespace@15..16 " "
                  VarDef@16..26
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
                  VarDef@0..13
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
                  VarDef@0..25
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
                  VarDef@0..28
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
                  VarDef@0..23
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
        recover_on_let_token: (
            "let a =\nlet b = a;",
            expect![[r#"
                Root@0..18
                  VarDef@0..8
                    KwLet@0..3 "let"
                    Whitespace@3..4 " "
                    InfixBinOp@4..8
                      VarRef@4..6
                        Ident@4..5 "a"
                        Whitespace@5..6 " "
                      Eq@6..7 "="
                      Whitespace@7..8 "\n"
                  VarDef@8..18
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

        recover_on_let_token_with_semicolon: (
            "let a =;let b = a;",
            expect![[r#"
                Root@0..18
                  VarDef@0..8
                    KwLet@0..3 "let"
                    Whitespace@3..4 " "
                    InfixBinOp@4..7
                      VarRef@4..6
                        Ident@4..5 "a"
                        Whitespace@5..6 " "
                      Eq@6..7 "="
                    Semi@7..8 ";"
                  VarDef@8..18
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
        parse_multiple_statements: (
            "let a = 1;\na",
            expect![[r#"
                Root@0..12
                  VarDef@0..10
                    KwLet@0..3 "let"
                    Whitespace@3..4 " "
                    InfixBinOp@4..9
                      VarRef@4..6
                        Ident@4..5 "a"
                        Whitespace@5..6 " "
                      Eq@6..7 "="
                      Whitespace@7..8 " "
                      Literal@8..9
                        Int@8..9 "1"
                    Semi@9..10 ";"
                  Whitespace@10..11 "\n"
                  VarRef@11..12
                    Ident@11..12 "a""#]],
        ),
        parse_multiple_statements_with_semicolon: (
            "let a = 0; a",
            expect![[r#"
                Root@0..12
                  VarDef@0..10
                    KwLet@0..3 "let"
                    Whitespace@3..4 " "
                    InfixBinOp@4..9
                      VarRef@4..6
                        Ident@4..5 "a"
                        Whitespace@5..6 " "
                      Eq@6..7 "="
                      Whitespace@7..8 " "
                      Literal@8..9
                        Int@8..9 "0"
                    Semi@9..10 ";"
                  Whitespace@10..11 " "
                  VarRef@11..12
                    Ident@11..12 "a""#]],
        ),
        recover_on_let_token_in_block: (
            "{let a =\n{let b =} 10 }",
            expect![[r#"
                Root@0..23
                  Block@0..23
                    LBrace@0..1 "{"
                    VarDef@1..21
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
                          VarDef@10..17
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
        recover_on_let_token_in_block_with_semicolon: (
            "{let a =\n{let b =}; 10 }",
            expect![[r#"
                Root@0..24
                  Block@0..24
                    LBrace@0..1 "{"
                    VarDef@1..19
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
                          VarDef@10..17
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
