use crate::{parse::Finished, parser::Parser};

use syntax::{anchor::RollingBackAnchor, syntax_kind::SyntaxKind::*};

#[allow(unused_variables)]
impl<'input> Parser<'input> {
    #[allow(unused_variables)]
    pub fn parse_conditionals(&self) -> Option<Finished> {
        let marker = self.start();
        self.expect_and_bump(KwIf);

        {
            let anchor = self.impose_condition_parsing_restrictions_for_control_flow();
            if self.parse_condition().is_none() {
                self.recover_with_msg("if expects a condition", "");
            }
        }

        self.parse_block();

        while self.is_next(KwElif) {
            self.expect_and_bump(KwElif);
            {
                let anchor = self.impose_condition_parsing_restrictions_for_control_flow();
                if self.parse_condition().is_none() {
                    self.recover_with_msg("elif expects a condition", "");
                }
            }
            self.parse_block();
        }

        if self.is_next(KwElse) {
            self.expect_and_bump(KwElse);
            self.parse_block();
        }

        let finished_as_expr = self.complete_marker_with(marker, ControlFlow);
        // if self.is_next(Semi) {
        //     let semi_marker = self.precede_marker_with(&finished_as_expr);
        //     self.expect_and_bump(Semi);
        //     Some(self.complete_marker_with(semi_marker, Semi))
        // } else {
        //     Some(finished_as_expr)
        // }
        Some(finished_as_expr)
    }

    pub fn impose_condition_parsing_restrictions_for_control_flow(&self) -> RollingBackAnchor {
        let rollback_after_drop = self.roll_back_context_after_drop();
        let ctx = self.context.borrow();
        ctx.allow([KwTrue, KwFalse, LParen].as_ref());
        ctx.forbid(LBrace);
        ctx.disallow_recovery_of([LBrace, KwElif, KwElse].as_ref());
        rollback_after_drop
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

      for_loop_iterated: ("for elm in arr { print(elm) }",
          expect![[r#"
                Root@0..29
                  ForLoop@0..29
                    KwFor@0..3 "for"
                    Whitespace@3..4 " "
                    ForIdent@4..8
                      Ident@4..7 "elm"
                      Whitespace@7..8 " "
                    KwIn@8..10 "in"
                    Whitespace@10..11 " "
                    VarRef@11..15
                      Ident@11..14 "arr"
                      Whitespace@14..15 " "
                    Block@15..29
                      LBrace@15..16 "{"
                      Whitespace@16..17 " "
                      FnCall@17..27
                        Ident@17..22 "print"
                        LParen@22..23 "("
                        FnArg@23..26
                          VarRef@23..26
                            Ident@23..26 "elm"
                        RParen@26..27 ")"
                      Whitespace@27..28 " "
                      RBrace@28..29 "}""#]],
      ),

      for_loop_with_slicing: ("for elm in arr[0_arr.len()]{ print(elm) }",
          expect![[r#"
                Root@0..41
                  ForLoop@0..41
                    KwFor@0..3 "for"
                    Whitespace@3..4 " "
                    ForIdent@4..8
                      Ident@4..7 "elm"
                      Whitespace@7..8 " "
                    KwIn@8..10 "in"
                    Whitespace@10..11 " "
                    ContainerRef@11..27
                      Ident@11..14 "arr"
                      LBrack@14..15 "["
                      InfixBinOp@15..26
                        Literal@15..16
                          Int@15..16 "0"
                        Under@16..17 "_"
                        InfixBinOp@17..26
                          VarRef@17..20
                            Ident@17..20 "arr"
                          Dot@20..21 "."
                          FnCall@21..26
                            Ident@21..24 "len"
                            LParen@24..25 "("
                            RParen@25..26 ")"
                      RBrack@26..27 "]"
                    Block@27..41
                      LBrace@27..28 "{"
                      Whitespace@28..29 " "
                      FnCall@29..39
                        Ident@29..34 "print"
                        LParen@34..35 "("
                        FnArg@35..38
                          VarRef@35..38
                            Ident@35..38 "elm"
                        RParen@38..39 ")"
                      Whitespace@39..40 " "
                      RBrace@40..41 "}""#]],
      ),
      for_loop_with_csv_identifiers: ("for ix,elm in arr[0_arr.len()].enumerate() { print(ix, elm) }",
          expect![[r#"
                Root@0..59
                  ForLoop@0..59
                    KwFor@0..3 "for"
                    Whitespace@3..4 " "
                    ForIdent@4..6
                      Ident@4..6 "ix"
                    ForIdent@6..10
                      Ident@6..9 "elm"
                      Whitespace@9..10 " "
                    KwIn@10..12 "in"
                    Whitespace@12..13 " "
                    InfixBinOp@13..42
                      ContainerRef@13..29
                        Ident@13..16 "arr"
                        LBrack@16..17 "["
                        InfixBinOp@17..28
                          Literal@17..18
                            Int@17..18 "0"
                          Under@18..19 "_"
                          InfixBinOp@19..28
                            VarRef@19..22
                              Ident@19..22 "arr"
                            Dot@22..23 "."
                            FnCall@23..28
                              Ident@23..26 "len"
                              LParen@26..27 "("
                              RParen@27..28 ")"
                        RBrack@28..29 "]"
                      Dot@29..30 "."
                      FnCall@30..41
                        Ident@30..39 "enumerate"
                        LParen@39..40 "("
                        RParen@40..41 ")"
                      Whitespace@41..42 " "
                    Block@42..59
                      LBrace@42..43 "{"
                      Whitespace@43..44 " "
                      FnCall@44..57
                        Ident@44..49 "print"
                        LParen@49..50 "("
                        FnArg@50..52
                          VarRef@50..52
                            Ident@50..52 "ix"
                        Whitespace@52..53 " "
                        FnArg@53..56
                          VarRef@53..56
                            Ident@53..56 "elm"
                        RParen@56..57 ")"
                      Whitespace@57..58 " "
                      RBrace@58..59 "}""#]],
      ),
        for_loop_with_range: ("for ix in 0_9{ print(arr[ix]) }",
            expect![[r#"
                Root@0..31
                  ForLoop@0..31
                    KwFor@0..3 "for"
                    Whitespace@3..4 " "
                    ForIdent@4..7
                      Ident@4..6 "ix"
                      Whitespace@6..7 " "
                    KwIn@7..9 "in"
                    Whitespace@9..10 " "
                    InfixBinOp@10..13
                      Literal@10..11
                        Int@10..11 "0"
                      Under@11..12 "_"
                      Literal@12..13
                        Int@12..13 "9"
                    Block@13..31
                      LBrace@13..14 "{"
                      Whitespace@14..15 " "
                      FnCall@15..29
                        Ident@15..20 "print"
                        LParen@20..21 "("
                        FnArg@21..28
                          ContainerRef@21..28
                            Ident@21..24 "arr"
                            LBrack@24..25 "["
                            VarRef@25..27
                              Ident@25..27 "ix"
                            RBrack@27..28 "]"
                        RParen@28..29 ")"
                      Whitespace@29..30 " "
                      RBrace@30..31 "}""#]],
        ),
                while_loop: ("while !stack.is_empty() { print(stack.pop()) }",
            expect![[r#"
                Root@0..46
                  WhileLoop@0..46
                    KwWhile@0..5 "while"
                    Whitespace@5..6 " "
                    Condition@6..24
                      PrefixUnaryOp@6..24
                        Excl@6..7 "!"
                        InfixBinOp@7..24
                          VarRef@7..12
                            Ident@7..12 "stack"
                          Dot@12..13 "."
                          FnCall@13..23
                            Ident@13..21 "is_empty"
                            LParen@21..22 "("
                            RParen@22..23 ")"
                          Whitespace@23..24 " "
                    Block@24..46
                      LBrace@24..25 "{"
                      Whitespace@25..26 " "
                      FnCall@26..44
                        Ident@26..31 "print"
                        LParen@31..32 "("
                        FnArg@32..43
                          InfixBinOp@32..43
                            VarRef@32..37
                              Ident@32..37 "stack"
                            Dot@37..38 "."
                            FnCall@38..43
                              Ident@38..41 "pop"
                              LParen@41..42 "("
                              RParen@42..43 ")"
                        RParen@43..44 ")"
                      Whitespace@44..45 " "
                      RBrace@45..46 "}""#]],
        ),

        while_loop_with_break: ("while true { break; }",
            expect![[r#"
                Root@0..21
                  WhileLoop@0..21
                    KwWhile@0..5 "while"
                    Whitespace@5..6 " "
                    Condition@6..10
                      KwTrue@6..10 "true"
                    Block@10..21
                      Whitespace@10..11 " "
                      LBrace@11..12 "{"
                      Whitespace@12..13 " "
                      Semi@13..19
                        Jump@13..18
                          KwBreak@13..18 "break"
                        Semi@18..19 ";"
                      Whitespace@19..20 " "
                      RBrace@20..21 "}""#]],
        ),

        while_loop_with_complex_cond_and_break: ("while human.age < 18{ break; }",
            expect![[r#"
                Root@0..30
                  WhileLoop@0..30
                    KwWhile@0..5 "while"
                    Whitespace@5..6 " "
                    Condition@6..20
                      InfixBinOp@6..20
                        InfixBinOp@6..16
                          VarRef@6..11
                            Ident@6..11 "human"
                          Dot@11..12 "."
                          VarRef@12..16
                            Ident@12..15 "age"
                            Whitespace@15..16 " "
                        Lt@16..17 "<"
                        Whitespace@17..18 " "
                        Literal@18..20
                          Int@18..20 "18"
                    Block@20..30
                      LBrace@20..21 "{"
                      Whitespace@21..22 " "
                      Semi@22..28
                        Jump@22..27
                          KwBreak@22..27 "break"
                        Semi@27..28 ";"
                      Whitespace@28..29 " "
                      RBrace@29..30 "}""#]],
        ),


        while_loop_with_continue: ("while true { continue; }",
            expect![[r#"
                Root@0..24
                  WhileLoop@0..24
                    KwWhile@0..5 "while"
                    Whitespace@5..6 " "
                    Condition@6..10
                      KwTrue@6..10 "true"
                    Block@10..24
                      Whitespace@10..11 " "
                      LBrace@11..12 "{"
                      Whitespace@12..13 " "
                      Jump@13..22
                        KwContinue@13..21 "continue"
                        Semi@21..22 ";"
                      Whitespace@22..23 " "
                      RBrace@23..24 "}""#]],
        ),

        while_loop_to_be_recovered: ("while { break; }",
            expect![[r#"
                Root@0..16
                  WhileLoop@0..16
                    KwWhile@0..5 "while"
                    Whitespace@5..6 " "
                    Condition@6..6
                    Block@6..16
                      LBrace@6..7 "{"
                      Whitespace@7..8 " "
                      Semi@8..14
                        Jump@8..13
                          KwBreak@8..13 "break"
                        Semi@13..14 ";"
                      Whitespace@14..15 " "
                      RBrace@15..16 "}""#]],
        ),

        conditionals_only_if_non_semi: ("if is_ok { break; }",
            expect![[r#"
                Root@0..19
                  ControlFlow@0..19
                    KwIf@0..2 "if"
                    Whitespace@2..3 " "
                    Condition@3..9
                      VarRef@3..9
                        Ident@3..8 "is_ok"
                        Whitespace@8..9 " "
                    Block@9..19
                      LBrace@9..10 "{"
                      Whitespace@10..11 " "
                      Jump@11..17
                        KwBreak@11..16 "break"
                        Semi@16..17 ";"
                      Whitespace@17..18 " "
                      RBrace@18..19 "}""#]],
        ),


        conditionals_only_if_non_semi_complex_cond: ("if human.age >= 18 { break; }",
            expect![[r#"
                Root@0..29
                  ControlFlow@0..29
                    KwIf@0..2 "if"
                    Whitespace@2..3 " "
                    Condition@3..19
                      InfixBinOp@3..19
                        InfixBinOp@3..13
                          VarRef@3..8
                            Ident@3..8 "human"
                          Dot@8..9 "."
                          VarRef@9..13
                            Ident@9..12 "age"
                            Whitespace@12..13 " "
                        Ge@13..15 ">="
                        Whitespace@15..16 " "
                        Literal@16..18
                          Int@16..18 "18"
                        Whitespace@18..19 " "
                    Block@19..29
                      LBrace@19..20 "{"
                      Whitespace@20..21 " "
                      Jump@21..27
                        KwBreak@21..26 "break"
                        Semi@26..27 ";"
                      Whitespace@27..28 " "
                      RBrace@28..29 "}""#]],
        ),
                conditionals_if_else_semi: ("let can_pass = if human.age >= 18 { true } else { false };",
            expect![[r#"
                Root@0..58
                  VarDef@0..58
                    KwLet@0..3 "let"
                    Whitespace@3..4 " "
                    InfixBinOp@4..57
                      VarRef@4..13
                        Ident@4..12 "can_pass"
                        Whitespace@12..13 " "
                      Eq@13..14 "="
                      Whitespace@14..15 " "
                      ControlFlow@15..57
                        KwIf@15..17 "if"
                        Whitespace@17..18 " "
                        Condition@18..34
                          InfixBinOp@18..34
                            InfixBinOp@18..28
                              VarRef@18..23
                                Ident@18..23 "human"
                              Dot@23..24 "."
                              VarRef@24..28
                                Ident@24..27 "age"
                                Whitespace@27..28 " "
                            Ge@28..30 ">="
                            Whitespace@30..31 " "
                            Literal@31..33
                              Int@31..33 "18"
                            Whitespace@33..34 " "
                        Block@34..42
                          LBrace@34..35 "{"
                          Whitespace@35..36 " "
                          Literal@36..40
                            KwTrue@36..40 "true"
                          Whitespace@40..41 " "
                          RBrace@41..42 "}"
                        Whitespace@42..43 " "
                        KwElse@43..47 "else"
                        Block@47..57
                          Whitespace@47..48 " "
                          LBrace@48..49 "{"
                          Whitespace@49..50 " "
                          Literal@50..55
                            KwFalse@50..55 "false"
                          Whitespace@55..56 " "
                          RBrace@56..57 "}"
                    Semi@57..58 ";""#]],
        ),
             conditionals_if_elif_else_semi: ("if color == RED { stop(); } elif color==YELLOW {prepare_to_stop(); } else { pass()}",
            expect![[r#"
                Root@0..81
                  ControlFlow@0..81
                    KwIf@0..2 "if"
                    Whitespace@2..3 " "
                    Condition@3..16
                      InfixBinOp@3..16
                        VarRef@3..9
                          Ident@3..8 "color"
                          Whitespace@8..9 " "
                        EqEq@9..11 "=="
                        Whitespace@11..12 " "
                        VarRef@12..16
                          Ident@12..15 "RED"
                          Whitespace@15..16 " "
                    Block@16..26
                      LBrace@16..17 "{"
                      Whitespace@17..18 " "
                      FnCall@18..24
                        Ident@18..22 "stop"
                        LParen@22..23 "("
                        RParen@23..24 ")"
                      Whitespace@24..25 " "
                      RBrace@25..26 "}"
                    Whitespace@26..27 " "
                    KwElif@27..31 "elif"
                    Whitespace@31..32 " "
                    Condition@32..46
                      InfixBinOp@32..46
                        VarRef@32..37
                          Ident@32..37 "color"
                        EqEq@37..39 "=="
                        VarRef@39..46
                          Ident@39..45 "YELLOW"
                          Whitespace@45..46 " "
                    Block@46..66
                      LBrace@46..47 "{"
                      FnCall@47..64
                        Ident@47..62 "prepare_to_stop"
                        LParen@62..63 "("
                        RParen@63..64 ")"
                      Whitespace@64..65 " "
                      RBrace@65..66 "}"
                    Whitespace@66..67 " "
                    KwElse@67..71 "else"
                    Block@71..81
                      Whitespace@71..72 " "
                      LBrace@72..73 "{"
                      Whitespace@73..74 " "
                      FnCall@74..80
                        Ident@74..78 "pass"
                        LParen@78..79 "("
                        RParen@79..80 ")"
                      RBrace@80..81 "}""#]],
        ),
        conditionals_if_returning_break: ("if color == RED { break stop(); }",
            expect![[r#"
                Root@0..33
                  ControlFlow@0..33
                    KwIf@0..2 "if"
                    Whitespace@2..3 " "
                    Condition@3..16
                      InfixBinOp@3..16
                        VarRef@3..9
                          Ident@3..8 "color"
                          Whitespace@8..9 " "
                        EqEq@9..11 "=="
                        Whitespace@11..12 " "
                        VarRef@12..16
                          Ident@12..15 "RED"
                          Whitespace@15..16 " "
                    Block@16..33
                      LBrace@16..17 "{"
                      Whitespace@17..18 " "
                      Jump@18..31
                        KwBreak@18..23 "break"
                        Whitespace@23..24 " "
                        FnCall@24..30
                          Ident@24..28 "stop"
                          LParen@28..29 "("
                          RParen@29..30 ")"
                        Semi@30..31 ";"
                      Whitespace@31..32 " "
                      RBrace@32..33 "}""#]],
        ),

    }
}
