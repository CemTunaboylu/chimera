use crate::{
    operator::starting_precedence,
    parse::{Element, Finished},
    parser::Parser,
};

use syntax::syntax_kind::SyntaxKind::*;

use thin_vec::thin_vec;

#[allow(unused_variables)]
impl Parser<'_> {
    #[allow(unused_variables)]
    pub fn parse_while_loop(&self) -> Option<Finished> {
        let marker = self.start();
        self.expect_and_bump(KwWhile);

        {
            let rollback_when_dropped =
                self.impose_restrictions_of_currently_parsing_on_context(WhileLoop);
            if self.parse_condition().is_none() {
                self.recover_with_msg("while loop expects a condition", "");
            }
        }
        self.parse_block();
        Some(self.complete_marker_with(marker, WhileLoop))
    }

    #[allow(unused_variables)]
    pub fn parse_for_loop(&self) -> Option<Finished> {
        let marker = self.start();
        self.expect_and_bump(KwFor);

        self.parse_loop_identifiers();
        {
            let rollback_when_dropped =
                self.impose_restrictions_of_currently_parsing_on_context(In);
            let in_marker = self.start();
            self.expect_and_bump(KwIn);
            // this can be 0_10, <iterable>, <iterable>.<method>,
            self.parse_expression_until_binding_power(starting_precedence());
            self.complete_marker_with(in_marker, In);
        }
        self.parse_block();
        Some(self.complete_marker_with(marker, ForLoop))
    }

    // this can be (<i1>, (<i2>, <i3>)) thus needs its own parsing
    #[allow(unused_variables)]
    pub fn parse_loop_identifiers(&self) {
        if self.is_next(LParen) {
            self.parse_left_hand_side();
        } else {
            // comma separated identifiers
            let rollback_when_dropped = self.roll_back_context_after_drop();
            self.context.borrow().allow(KwIn);

            use Element::*;

            let idents = thin_vec![Kind(Ident)];
            self.parse_separated_by(&idents, ForIdent, Comma, KwIn);
        }
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
                  In@8..15
                    KwIn@8..10 "in"
                    Whitespace@10..11 " "
                    VarRef@11..15
                      Ident@11..14 "arr"
                      Whitespace@14..15 " "
                  Block@15..29
                    LBrace@15..16 "{"
                    Whitespace@16..17 " "
                    Call@17..27
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
                  In@8..27
                    KwIn@8..10 "in"
                    Whitespace@10..11 " "
                    ContainerRef@11..27
                      Ident@11..14 "arr"
                      Indexing@14..27
                        LBrack@14..15 "["
                        InfixBinOp@15..26
                          Literal@15..16
                            Int@15..16 "0"
                          Under@16..17 "_"
                          InfixBinOp@17..26
                            VarRef@17..20
                              Ident@17..20 "arr"
                            Dot@20..21 "."
                            Call@21..26
                              Ident@21..24 "len"
                              LParen@24..25 "("
                              RParen@25..26 ")"
                        RBrack@26..27 "]"
                  Block@27..41
                    LBrace@27..28 "{"
                    Whitespace@28..29 " "
                    Call@29..39
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
              Root@0..61
                ForLoop@0..61
                  KwFor@0..3 "for"
                  Whitespace@3..4 " "
                  ForIdent@4..7
                    Ident@4..6 "ix"
                    Comma@6..7 ","
                  ForIdent@7..11
                    Ident@7..10 "elm"
                    Whitespace@10..11 " "
                  In@11..43
                    KwIn@11..13 "in"
                    Whitespace@13..14 " "
                    InfixBinOp@14..43
                      ContainerRef@14..30
                        Ident@14..17 "arr"
                        Indexing@17..30
                          LBrack@17..18 "["
                          InfixBinOp@18..29
                            Literal@18..19
                              Int@18..19 "0"
                            Under@19..20 "_"
                            InfixBinOp@20..29
                              VarRef@20..23
                                Ident@20..23 "arr"
                              Dot@23..24 "."
                              Call@24..29
                                Ident@24..27 "len"
                                LParen@27..28 "("
                                RParen@28..29 ")"
                          RBrack@29..30 "]"
                      Dot@30..31 "."
                      Call@31..42
                        Ident@31..40 "enumerate"
                        LParen@40..41 "("
                        RParen@41..42 ")"
                      Whitespace@42..43 " "
                  Block@43..61
                    LBrace@43..44 "{"
                    Whitespace@44..45 " "
                    Call@45..59
                      Ident@45..50 "print"
                      LParen@50..51 "("
                      FnArg@51..54
                        VarRef@51..53
                          Ident@51..53 "ix"
                        Comma@53..54 ","
                      Whitespace@54..55 " "
                      FnArg@55..58
                        VarRef@55..58
                          Ident@55..58 "elm"
                      RParen@58..59 ")"
                    Whitespace@59..60 " "
                    RBrace@60..61 "}""#]],
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
                    In@7..13
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
                      Call@15..29
                        Ident@15..20 "print"
                        LParen@20..21 "("
                        FnArg@21..28
                          ContainerRef@21..28
                            Ident@21..24 "arr"
                            Indexing@24..28
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
                          Call@13..23
                            Ident@13..21 "is_empty"
                            LParen@21..22 "("
                            RParen@22..23 ")"
                          Whitespace@23..24 " "
                    Block@24..46
                      LBrace@24..25 "{"
                      Whitespace@25..26 " "
                      Call@26..44
                        Ident@26..31 "print"
                        LParen@31..32 "("
                        FnArg@32..43
                          InfixBinOp@32..43
                            VarRef@32..37
                              Ident@32..37 "stack"
                            Dot@37..38 "."
                            Call@38..43
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
                      Jump@13..19
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
                      Jump@22..28
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
                      Jump@8..14
                        KwBreak@8..13 "break"
                        Semi@13..14 ";"
                      Whitespace@14..15 " "
                      RBrace@15..16 "}""#]],
        ),
    }
}
