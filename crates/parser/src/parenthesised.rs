use crate::{
    operator::starting_precedence,
    parse::{Element, Finished},
    parser::{IsNext, Parser},
};

use syntax::{
    anchor::RollingBackAnchor,
    bitset::SyntaxKindBitSet,
    syntax_kind::SyntaxKind::{self, *},
};

#[allow(unused_variables)]
impl Parser<'_> {
    pub fn tuple_pattern_expectation(&self) -> impl Into<SyntaxKindBitSet> {
        [Tuple, LetBinding].as_ref()
    }
    pub fn is_parsing_tuple_pattern(&self) -> bool {
        self.is_expected([Tuple, LetBinding].as_ref())
    }
    pub fn mark_tuple_pattern_as_parsed(&self) {
        self.mark_expectation_as_satisfied_in_ctx([Tuple, LetBinding].as_ref());
    }

    fn impose_tuple_pattern_context(&self) -> RollingBackAnchor {
        let rollback_when_dropped = self.roll_back_context_after_drop();
        let ctx = self.context.borrow();
        let closing_delimiters: SyntaxKindBitSet = SyntaxKind::closing_delimiters().as_ref().into();
        ctx.disallow_recovery_of(closing_delimiters + [Comma].as_ref().into());
        let allowed: SyntaxKindBitSet = [And, KwMut, Ident, Mut, SelfRef].as_ref().into();
        let opening_delimiters = SyntaxKind::opening_delimiters().as_ref().into();
        ctx.allow_only(allowed + opening_delimiters);
        rollback_when_dropped
    }

    // Parses tuples in let bindings, e.g. let (a,b,c) = ... a pattern expression as in Rust
    #[allow(unused_variables)]
    pub fn parse_tuple_pattern(&self) -> Option<Finished> {
        let rollback_when_dropped = self.impose_tuple_pattern_context();
        let marker = self.start();
        self.expect_and_bump(LParen);
        self.parse_tuple();
        self.expect_and_bump(RParen);
        let finished = self.complete_marker_with(marker, SyntaxKind::Tuple);
        Some(finished)
    }

    fn parse_tuple(&self) {
        let elements = &[Element::ParseExprWith(starting_precedence())];
        self.bump_separated_by(elements, Comma, RParen);
    }

    #[allow(unused_variables)]
    pub fn parse_parenthesised(&self) -> Option<Finished> {
        if self.is_parsing_tuple_pattern() {
            return self.parse_tuple_pattern();
        }
        let rollback_when_dropped = self.impose_context_for_parsing(ParenExpr);
        let marker = self.start();
        self.expect_and_bump(LParen);

        let kind = if IsNext::No == self.is_next_strict(RParen) {
            self.parse_expression_until_binding_power(starting_precedence());
            match self.is_next_strict(Comma) {
                IsNext::Yes => {
                    let rollback_when_dropped = self.impose_context_for_parsing(Tuple);
                    self.bump_if(Comma);
                    self.parse_tuple();
                    Tuple
                }
                IsNext::No => ParenExpr,
                _ => return None,
            }
        } else {
            Unit // an empty () is the unit tuple now
        };

        self.expect_and_bump(RParen);
        let finished = self.complete_marker_with(marker, kind);
        Some(finished)
    }
}

#[cfg(test)]
mod tests {
    use crate::parse::tests::check;
    use expect_test::expect;
    use parameterized_test::create;

    #[test]
    fn test_erroneous_tuple_patterns() {
        check(
            "let (a + b,) = 1;",
            expect![[r#"
                Root@0..17
                  LetBinding@0..17
                    KwLet@0..3 "let"
                    Whitespace@3..4 " "
                    InfixBinOp@4..16
                      Tuple@4..12
                        LParen@4..5 "("
                        VarRef@5..7
                          Ident@5..6 "a"
                          Whitespace@6..7 " "
                        Recovered@7..8
                          Plus@7..8 "+"
                        Whitespace@8..9 " "
                        VarRef@9..10
                          Ident@9..10 "b"
                        Comma@10..11 ","
                        RParen@11..12 ")"
                      Whitespace@12..13 " "
                      Eq@13..14 "="
                      Whitespace@14..15 " "
                      Literal@15..16
                        Int@15..16 "1"
                    Semi@16..17 ";""#]],
        )
    }

    create! {
        create_parser_test,
        (prog, expect), {
            check(prog, expect);
        }
    }

    create_parser_test! {
        only_parenthesis: ("()",
            expect![[r#"
                Root@0..2
                  Unit@0..2
                    LParen@0..1 "("
                    RParen@1..2 ")""#]]
        ),
        single_element_parenthesised_expr: ("(x)",
            expect![[r#"
                Root@0..3
                  ParenExpr@0..3
                    LParen@0..1 "("
                    VarRef@1..2
                      Ident@1..2 "x"
                    RParen@2..3 ")""#]]
        ),
        single_element_tuple_with_trailing_comma: ("(x,)",
            expect![[r#"
                Root@0..4
                  Tuple@0..4
                    LParen@0..1 "("
                    VarRef@1..2
                      Ident@1..2 "x"
                    Comma@2..3 ","
                    RParen@3..4 ")""#]]
        ),
        single_element_tuple_with_mut: ("(mut x,)",
            expect![[r#"
                Root@0..8
                  Tuple@0..8
                    LParen@0..1 "("
                    Mut@1..6
                      KwMut@1..4 "mut"
                      Whitespace@4..5 " "
                      VarRef@5..6
                        Ident@5..6 "x"
                    Comma@6..7 ","
                    RParen@7..8 ")""#]]
        ),
        single_element_tuple_with_ref_mut: ("(& mut x,)",
            expect![[r#"
                Root@0..10
                  Tuple@0..10
                    LParen@0..1 "("
                    PrefixUnaryOp@1..8
                      And@1..2 "&"
                      Whitespace@2..3 " "
                      Mut@3..8
                        KwMut@3..6 "mut"
                        Whitespace@6..7 " "
                        VarRef@7..8
                          Ident@7..8 "x"
                    Comma@8..9 ","
                    RParen@9..10 ")""#]]
        ),
        element_grouping_parenthesised_expr: ("(x+y)",
            expect![[r#"
                Root@0..5
                  ParenExpr@0..5
                    LParen@0..1 "("
                    InfixBinOp@1..4
                      VarRef@1..2
                        Ident@1..2 "x"
                      Plus@2..3 "+"
                      VarRef@3..4
                        Ident@3..4 "y"
                    RParen@4..5 ")""#]]
        ),
        tuple_with_grouped_elements: ("(x+y,)",
            expect![[r#"
                Root@0..6
                  Tuple@0..6
                    LParen@0..1 "("
                    InfixBinOp@1..4
                      VarRef@1..2
                        Ident@1..2 "x"
                      Plus@2..3 "+"
                      VarRef@3..4
                        Ident@3..4 "y"
                    Comma@4..5 ","
                    RParen@5..6 ")""#]]
        ),
        nested_tuples: ("((x,y),z)",
            expect![[r#"
                Root@0..9
                  Tuple@0..9
                    LParen@0..1 "("
                    Tuple@1..6
                      LParen@1..2 "("
                      VarRef@2..3
                        Ident@2..3 "x"
                      Comma@3..4 ","
                      VarRef@4..5
                        Ident@4..5 "y"
                      RParen@5..6 ")"
                    Comma@6..7 ","
                    VarRef@7..8
                      Ident@7..8 "z"
                    RParen@8..9 ")""#]]
        ),
        for_loop_with_tuple_pattern: ("for ((mut x, & mut y), z) in vec_of_pairs {}",
            expect![[r#"
                Root@0..44
                  ForLoop@0..44
                    KwFor@0..3 "for"
                    Whitespace@3..4 " "
                    ForIdent@4..25
                      Tuple@4..25
                        LParen@4..5 "("
                        Tuple@5..21
                          LParen@5..6 "("
                          Mut@6..11
                            KwMut@6..9 "mut"
                            Whitespace@9..10 " "
                            VarRef@10..11
                              Ident@10..11 "x"
                          Comma@11..12 ","
                          Whitespace@12..13 " "
                          PrefixUnaryOp@13..20
                            And@13..14 "&"
                            Whitespace@14..15 " "
                            Mut@15..20
                              KwMut@15..18 "mut"
                              Whitespace@18..19 " "
                              VarRef@19..20
                                Ident@19..20 "y"
                          RParen@20..21 ")"
                        Comma@21..22 ","
                        Whitespace@22..23 " "
                        VarRef@23..24
                          Ident@23..24 "z"
                        RParen@24..25 ")"
                    In@25..42
                      Whitespace@25..26 " "
                      KwIn@26..28 "in"
                      Whitespace@28..29 " "
                      VarRef@29..42
                        Ident@29..41 "vec_of_pairs"
                        Whitespace@41..42 " "
                    Block@42..44
                      LBrace@42..43 "{"
                      RBrace@43..44 "}""#]]
        ),
        missing_closing_parenthesis_does_not_panic: ("(",
            expect![[
                "Root@0..1\n  Unit@0..1\n    LParen@0..1 \"(\""
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
                  LetBinding@0..14
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
                  LetBinding@0..12
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
                    LetBinding@0..22
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
                  LetBinding@0..21
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
    }
}
