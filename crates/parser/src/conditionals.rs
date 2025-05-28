use crate::{
    parse::Finished,
    parser::{IsNext, Parser},
};

use syntax::syntax_kind::SyntaxKind::*;

#[allow(unused_variables)]
impl Parser<'_> {
    #[allow(unused_variables)]
    pub fn parse_conditionals(&self) -> Option<Finished> {
        let marker = self.start();
        let cond_marker = self.start();
        self.expect_and_bump(KwIf);
        {
            let anchor = self.impose_restrictions_of_currently_parsing_on_context(Condition);
            if self.parse_condition().is_none() {
                self.recover_with_msg("if expects a condition", "");
            }
        }

        self.parse_block();
        self.complete_marker_with(cond_marker, Conditional);

        while IsNext::Yes == self.is_next_strict(KwElif) {
            let cond_marker = self.start();
            self.expect_and_bump(KwElif);
            {
                let anchor = self.impose_restrictions_of_currently_parsing_on_context(Condition);
                if self.parse_condition().is_none() {
                    self.recover_with_msg("elif expects a condition", "");
                }
            }
            self.parse_block();
            self.complete_marker_with(cond_marker, Conditional);
        }

        if self.is_next(KwElse) {
            let cond_marker = self.start();
            self.expect_and_bump(KwElse);
            self.parse_block();
            self.complete_marker_with(cond_marker, Conditional);
        }

        let finished_as_expr = self.complete_marker_with(marker, ControlFlow);
        Some(finished_as_expr)
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
        conditionals_only_if_non_semi: ("if is_ok { break; }",
            expect![[r#"
                Root@0..19
                  ControlFlow@0..19
                    Conditional@0..19
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
                    Conditional@0..29
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
                        Conditional@15..42
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
                        Conditional@43..57
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
                Root@0..83
                  ControlFlow@0..83
                    Conditional@0..27
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
                      Block@16..27
                        LBrace@16..17 "{"
                        Whitespace@17..18 " "
                        Semi@18..25
                          Call@18..24
                            Ident@18..22 "stop"
                            LParen@22..23 "("
                            RParen@23..24 ")"
                          Semi@24..25 ";"
                        Whitespace@25..26 " "
                        RBrace@26..27 "}"
                    Whitespace@27..28 " "
                    Conditional@28..68
                      KwElif@28..32 "elif"
                      Whitespace@32..33 " "
                      Condition@33..47
                        InfixBinOp@33..47
                          VarRef@33..38
                            Ident@33..38 "color"
                          EqEq@38..40 "=="
                          VarRef@40..47
                            Ident@40..46 "YELLOW"
                            Whitespace@46..47 " "
                      Block@47..68
                        LBrace@47..48 "{"
                        Semi@48..66
                          Call@48..65
                            Ident@48..63 "prepare_to_stop"
                            LParen@63..64 "("
                            RParen@64..65 ")"
                          Semi@65..66 ";"
                        Whitespace@66..67 " "
                        RBrace@67..68 "}"
                    Whitespace@68..69 " "
                    Conditional@69..83
                      KwElse@69..73 "else"
                      Block@73..83
                        Whitespace@73..74 " "
                        LBrace@74..75 "{"
                        Whitespace@75..76 " "
                        Call@76..82
                          Ident@76..80 "pass"
                          LParen@80..81 "("
                          RParen@81..82 ")"
                        RBrace@82..83 "}""#]],
        ),
        conditionals_if_returning_break: ("if color == RED { break stop(); }",
            expect![[r#"
                Root@0..33
                  ControlFlow@0..33
                    Conditional@0..33
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
                          Call@24..30
                            Ident@24..28 "stop"
                            LParen@28..29 "("
                            RParen@29..30 ")"
                          Semi@30..31 ";"
                        Whitespace@31..32 " "
                        RBrace@32..33 "}""#]],
        ),

    }
}
