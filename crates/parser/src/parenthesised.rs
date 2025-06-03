use crate::{
    operator::starting_precedence,
    parse::{Element, Finished},
    parser::Parser,
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
    pub fn is_parsing_tuple_type(&self) -> bool {
        self.is_expected([TypeHint].as_ref())
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
        let types: SyntaxKindBitSet = SyntaxKind::types().as_ref().into();
        let opening_delimiters = SyntaxKind::opening_delimiters().as_ref().into();
        ctx.allow_only(allowed + opening_delimiters + types);
        rollback_when_dropped
    }
    fn impose_tuple_type_context(&self) -> RollingBackAnchor {
        let rollback_when_dropped = self.by_forbidding_all();
        self.dont_recover_in_ctx(RParen);
        let types: SyntaxKindBitSet = SyntaxKind::types().as_ref().into();
        let allowed: SyntaxKindBitSet = [And, Comma, Ident, KwMut, LParen, SelfRef].as_ref().into();
        self.allow_in_ctx(allowed + types);
        rollback_when_dropped
    }
    pub fn parse_tuple_type(&self) {
        let marker = self.start();
        self.expect_and_bump(LParen);
        let elements = &[Element::ParseExprWith(starting_precedence())];
        self.bump_separated_by(elements, Comma, RParen);
        self.expect_and_bump(RParen);
        self.complete_marker_with(marker, Tuple);
    }
    fn parse_tuple(&self) {
        let elements = if self.is_parsing_tuple_pattern() {
            &[Element::LeftHandSide]
        } else {
            &[Element::ParseExprWith(starting_precedence())]
        };
        self.bump_separated_by(elements, Comma, RParen);
    }
    #[allow(unused_variables)]
    fn parse_parenthesised_or_tuple_pattern(&self) -> (SyntaxKind, Option<Finished>) {
        let marker = self.start();
        self.expect_and_bump(LParen);
        let mut kind = Unit; // an empty () is the unit tuple now
        if self.bump_if(RParen) {
            return (kind, Some(self.complete_marker_with(marker, kind)));
        }
        kind = ParenExpr;
        if self
            .parse_expression_until_binding_power(starting_precedence())
            .is_none()
        {
            self.bump_until([Comma, RParen].as_ref());
        };

        if self.bump_if(RParen) {
            return (kind, Some(self.complete_marker_with(marker, kind)));
        } else if self.bump_if(Comma) {
            kind = Tuple;
            let rollback_when_dropped = self.impose_context_for_parsing(Tuple);
            self.parse_tuple();
        } else {
            self.bump_until(RParen);
            kind = Recovered;
        }

        self.expect_and_bump(RParen);
        let finished = self.complete_marker_with(marker, kind);
        (kind, Some(finished))
    }

    // Parses tuples in let bindings, e.g. let (a,b,c) = ... a pattern expression as in Rust
    #[allow(unused_variables)]
    pub fn parse_tuple_pattern(&self) -> Option<Finished> {
        let rollback_when_dropped = self.impose_tuple_pattern_context();
        let (kind, finished) = self.parse_parenthesised_or_tuple_pattern();
        if kind != Tuple {
            self.emit_error_event("expected tuple pattern");
        }
        finished
    }

    #[allow(unused_variables)]
    /// Main entry point for parsing tuples and parenthesised expressions
    pub fn parse_parenthesised(&self) -> Option<Finished> {
        if self.is_parsing_tuple_pattern() {
            let marker = self.parse_tuple_pattern();
            // we now need to revert expectation to allow upcoming
            // tuples or parenthesised expressions parse
            self.mark_tuple_pattern_as_parsed();
            return marker;
        } else if self.is_parsing_tuple_type() {
            let rollback_when_dropped = self.impose_tuple_type_context();
            self.parse_tuple_type();
            return None;
        }
        let rollback_when_dropped = self.impose_context_for_parsing(ParenExpr);
        let (kind, finished) = self.parse_parenthesised_or_tuple_pattern();
        if kind == Recovered {
            self.emit_error_event("expected parenthesised expression");
        }
        finished
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

    #[test]
    fn test_last_erroneous_tuple_patterns() {
        check(
            "let (x, y, a + b,) = 1;",
            expect![[r#"
                Root@0..23
                  LetBinding@0..23
                    KwLet@0..3 "let"
                    Whitespace@3..4 " "
                    InfixBinOp@4..22
                      Tuple@4..18
                        LParen@4..5 "("
                        VarRef@5..6
                          Ident@5..6 "x"
                        Comma@6..7 ","
                        Whitespace@7..8 " "
                        VarRef@8..9
                          Ident@8..9 "y"
                        Comma@9..10 ","
                        Whitespace@10..11 " "
                        VarRef@11..13
                          Ident@11..12 "a"
                          Whitespace@12..13 " "
                        Recovered@13..14
                          Plus@13..14 "+"
                        Whitespace@14..15 " "
                        VarRef@15..16
                          Ident@15..16 "b"
                        Comma@16..17 ","
                        RParen@17..18 ")"
                      Whitespace@18..19 " "
                      Eq@19..20 "="
                      Whitespace@20..21 " "
                      Literal@21..22
                        Int@21..22 "1"
                    Semi@22..23 ";""#]],
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
        just_comma_in_parenthesis: ("(,)",
            expect![[r#"
                Root@0..3
                  Tuple@0..3
                    LParen@0..1 "("
                    Comma@1..2 ","
                    RParen@2..3 ")""#]]
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
        return_single_element_tuple_with_trailing_comma: ("return (x,)",
            expect![[r#"
                Root@0..11
                  Return@0..11
                    KwReturn@0..6 "return"
                    Whitespace@6..7 " "
                    Tuple@7..11
                      LParen@7..8 "("
                      VarRef@8..9
                        Ident@8..9 "x"
                      Comma@9..10 ","
                      RParen@10..11 ")""#]]
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
        two_element_tuple_with_double_commas: ("(x,,y)",
            expect![[r#"
                Root@0..6
                  Tuple@0..6
                    LParen@0..1 "("
                    VarRef@1..2
                      Ident@1..2 "x"
                    Comma@2..3 ","
                    Comma@3..4 ","
                    VarRef@4..5
                      Ident@4..5 "y"
                    RParen@5..6 ")""#]]
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
            expect![[r#"
                Root@0..1
                  Recovered@0..1
                    LParen@0..1 "(""#]]
        ),
        parenthesised_sub_precedes_mul: ("(3+1)*4",
            expect![[
                "Root@0..7\n  InfixBinOp@0..7\n    ParenExpr@0..5\n      LParen@0..1 \"(\"\n      InfixBinOp@1..4\n        Literal@1..2\n          Int@1..2 \"3\"\n        Plus@2..3 \"+\"\n        Literal@3..4\n          Int@3..4 \"1\"\n      RParen@4..5 \")\"\n    Star@5..6 \"*\"\n    Literal@6..7\n      Int@6..7 \"4\""
            ]]
        ),
        parenthesised_let_binding: ("let z = (3+1);",
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
        let_binding_with_valid_tuple_pattern: ("let (mut x:i32, y: &mut Coordinate ) = (0,Coordinate(0));",
            expect![[r#"
                Root@0..57
                  LetBinding@0..57
                    KwLet@0..3 "let"
                    Whitespace@3..4 " "
                    InfixBinOp@4..56
                      Tuple@4..36
                        LParen@4..5 "("
                        Mut@5..14
                          KwMut@5..8 "mut"
                          Whitespace@8..9 " "
                          VarRef@9..14
                            Ident@9..10 "x"
                            Colon@10..11 ":"
                            TypeHint@11..14
                              TyI32@11..14 "i32"
                        Comma@14..15 ","
                        Whitespace@15..16 " "
                        VarRef@16..35
                          Ident@16..17 "y"
                          Colon@17..18 ":"
                          TypeHint@18..35
                            Whitespace@18..19 " "
                            PrefixUnaryOp@19..35
                              And@19..20 "&"
                              Mut@20..34
                                KwMut@20..23 "mut"
                                Whitespace@23..24 " "
                                StructAsType@24..34 "Coordinate"
                              Whitespace@34..35 " "
                        RParen@35..36 ")"
                      Whitespace@36..37 " "
                      Eq@37..38 "="
                      Whitespace@38..39 " "
                      Tuple@39..56
                        LParen@39..40 "("
                        Literal@40..41
                          Int@40..41 "0"
                        Comma@41..42 ","
                        Call@42..55
                          Ident@42..52 "Coordinate"
                          LParen@52..53 "("
                          FnArg@53..54
                            Literal@53..54
                              Int@53..54 "0"
                          RParen@54..55 ")"
                        RParen@55..56 ")"
                    Semi@56..57 ";""#]]
        ),
        let_binding_with_two_commas_pattern: ("let (x,,y) = (3,1);",
            expect![[r#"
                Root@0..19
                  LetBinding@0..19
                    KwLet@0..3 "let"
                    Whitespace@3..4 " "
                    InfixBinOp@4..18
                      Tuple@4..10
                        LParen@4..5 "("
                        VarRef@5..6
                          Ident@5..6 "x"
                        Comma@6..7 ","
                        Comma@7..8 ","
                        VarRef@8..9
                          Ident@8..9 "y"
                        RParen@9..10 ")"
                      Whitespace@10..11 " "
                      Eq@11..12 "="
                      Whitespace@12..13 " "
                      Tuple@13..18
                        LParen@13..14 "("
                        Literal@14..15
                          Int@14..15 "3"
                        Comma@15..16 ","
                        Literal@16..17
                          Int@16..17 "1"
                        RParen@17..18 ")"
                    Semi@18..19 ";""#]]
        ),
        misplaced_parenthesised_let_binding: ("let z = (3;1);",
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
                      Recovered@8..13
                        LParen@8..9 "("
                        Literal@9..10
                          Int@9..10 "3"
                        Semi@10..11 ";"
                        Int@11..12 "1"
                        RParen@12..13 ")"
                    Semi@13..14 ";""#]]
        ),
        misplaced_parenthesised_let_binding_with_fn_call: ("let z = (something(););",
            expect![[r#"
                Root@0..23
                  LetBinding@0..23
                    KwLet@0..3 "let"
                    Whitespace@3..4 " "
                    InfixBinOp@4..22
                      VarRef@4..6
                        Ident@4..5 "z"
                        Whitespace@5..6 " "
                      Eq@6..7 "="
                      Whitespace@7..8 " "
                      Recovered@8..22
                        LParen@8..9 "("
                        Call@9..20
                          Ident@9..18 "something"
                          LParen@18..19 "("
                          RParen@19..20 ")"
                        Semi@20..21 ";"
                        RParen@21..22 ")"
                    Semi@22..23 ";""#]]
        ),
        assignment_during_let_binding: ("let z = (self.x += 1) + 2;",
            expect![[r#"
                Root@0..26
                  LetBinding@0..26
                    KwLet@0..3 "let"
                    Whitespace@3..4 " "
                    InfixBinOp@4..25
                      VarRef@4..6
                        Ident@4..5 "z"
                        Whitespace@5..6 " "
                      Eq@6..7 "="
                      Whitespace@7..8 " "
                      InfixBinOp@8..25
                        Recovered@8..21
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
                          Int@19..20 "1"
                          RParen@20..21 ")"
                        Whitespace@21..22 " "
                        Plus@22..23 "+"
                        Whitespace@23..24 " "
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
                Root@0..9
                  Semi@0..9
                    ParenExpr@0..8
                      LParen@0..1 "("
                      Recovered@1..7
                        LParen@1..2 "("
                        Literal@2..5
                          Int@2..5 "314"
                        Semi@5..6 ";"
                        RParen@6..7 ")"
                      RParen@7..8 ")"
                    Semi@8..9 ";""#]]
        ),
    }
}
