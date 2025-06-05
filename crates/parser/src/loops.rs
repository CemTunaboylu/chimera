use crate::{operator::starting_precedence, parse::Finished, parser::Parser};

use syntax::syntax_kind::SyntaxKind::*;

#[allow(unused_variables)]
impl Parser<'_> {
    #[allow(unused_variables)]
    pub fn parse_while_loop(&self) -> Option<Finished> {
        let marker = self.start();
        self.expect_and_bump(KwWhile);

        {
            let rollback_when_dropped = self.impose_context_for_parsing(WhileLoop);
            if self.parse_condition().is_none() {
                self.recover_with_msg("while loop expects a condition", "");
            }
        }
        self.parse_block();
        Some(self.complete_marker_with(marker, WhileLoop))
    }

    #[allow(unused_variables)]
    pub fn parse_for_loop(&self) -> Option<Finished> {
        let rollback_when_dropped = self.parsing(ForLoop);
        let marker = self.start();
        self.expect_and_bump(KwFor);
        self.parse_loop_identifiers();
        {
            let rollback_when_dropped = self.impose_context_for_parsing(In);
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
        let marker = if self.is_next(LParen) {
            let rollback_when_dropped = self.by_expecting(self.tuple_pattern_expectation());
            let marker = self.parse_tuple_pattern();
            if let Some(completed) = marker {
                self.precede_marker_with(&completed)
            } else {
                return;
            }
        } else {
            let marker = self.start();
            self.expect_and_bump(Ident);
            // if there is more than one identifier, we need to recover until KwIn
            self.recover_until(KwIn);
            marker
        };
        self.complete_marker_with(marker, ForIdent);
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
      for_loop_with_tuple_pattern: ("for (ix,elm) in arr[0_arr.len()].enumerate() { print(ix, elm) }",
          expect![[r#"
              Root@0..63
                ForLoop@0..63
                  KwFor@0..3 "for"
                  Whitespace@3..4 " "
                  ForIdent@4..12
                    Tuple@4..12
                      LParen@4..5 "("
                      VarRef@5..7
                        Ident@5..7 "ix"
                      Comma@7..8 ","
                      VarRef@8..11
                        Ident@8..11 "elm"
                      RParen@11..12 ")"
                  In@12..45
                    Whitespace@12..13 " "
                    KwIn@13..15 "in"
                    Whitespace@15..16 " "
                    InfixBinOp@16..45
                      ContainerRef@16..32
                        Ident@16..19 "arr"
                        Indexing@19..32
                          LBrack@19..20 "["
                          InfixBinOp@20..31
                            Literal@20..21
                              Int@20..21 "0"
                            Under@21..22 "_"
                            InfixBinOp@22..31
                              VarRef@22..25
                                Ident@22..25 "arr"
                              Dot@25..26 "."
                              Call@26..31
                                Ident@26..29 "len"
                                LParen@29..30 "("
                                RParen@30..31 ")"
                          RBrack@31..32 "]"
                      Dot@32..33 "."
                      Call@33..44
                        Ident@33..42 "enumerate"
                        LParen@42..43 "("
                        RParen@43..44 ")"
                      Whitespace@44..45 " "
                  Block@45..63
                    LBrace@45..46 "{"
                    Whitespace@46..47 " "
                    Call@47..61
                      Ident@47..52 "print"
                      LParen@52..53 "("
                      FnArg@53..56
                        VarRef@53..55
                          Ident@53..55 "ix"
                        Comma@55..56 ","
                      Whitespace@56..57 " "
                      FnArg@57..60
                        VarRef@57..60
                          Ident@57..60 "elm"
                      RParen@60..61 ")"
                    Whitespace@61..62 " "
                    RBrace@62..63 "}""#]],
      ),
      for_loop_with_nested_tuple_pattern: ("for ((ix,elm), (ix2,elm2)) in arr[0_arr.len()].enumerate() { print(ix, elm, ix2, elm2) }",
          expect![[r#"
              Root@0..88
                ForLoop@0..88
                  KwFor@0..3 "for"
                  Whitespace@3..4 " "
                  ForIdent@4..26
                    Tuple@4..26
                      LParen@4..5 "("
                      Tuple@5..13
                        LParen@5..6 "("
                        VarRef@6..8
                          Ident@6..8 "ix"
                        Comma@8..9 ","
                        VarRef@9..12
                          Ident@9..12 "elm"
                        RParen@12..13 ")"
                      Comma@13..14 ","
                      Whitespace@14..15 " "
                      Tuple@15..25
                        LParen@15..16 "("
                        VarRef@16..19
                          Ident@16..19 "ix2"
                        Comma@19..20 ","
                        VarRef@20..24
                          Ident@20..24 "elm2"
                        RParen@24..25 ")"
                      RParen@25..26 ")"
                  In@26..59
                    Whitespace@26..27 " "
                    KwIn@27..29 "in"
                    Whitespace@29..30 " "
                    InfixBinOp@30..59
                      ContainerRef@30..46
                        Ident@30..33 "arr"
                        Indexing@33..46
                          LBrack@33..34 "["
                          InfixBinOp@34..45
                            Literal@34..35
                              Int@34..35 "0"
                            Under@35..36 "_"
                            InfixBinOp@36..45
                              VarRef@36..39
                                Ident@36..39 "arr"
                              Dot@39..40 "."
                              Call@40..45
                                Ident@40..43 "len"
                                LParen@43..44 "("
                                RParen@44..45 ")"
                          RBrack@45..46 "]"
                      Dot@46..47 "."
                      Call@47..58
                        Ident@47..56 "enumerate"
                        LParen@56..57 "("
                        RParen@57..58 ")"
                      Whitespace@58..59 " "
                  Block@59..88
                    LBrace@59..60 "{"
                    Whitespace@60..61 " "
                    Call@61..86
                      Ident@61..66 "print"
                      LParen@66..67 "("
                      FnArg@67..70
                        VarRef@67..69
                          Ident@67..69 "ix"
                        Comma@69..70 ","
                      Whitespace@70..71 " "
                      FnArg@71..75
                        VarRef@71..74
                          Ident@71..74 "elm"
                        Comma@74..75 ","
                      Whitespace@75..76 " "
                      FnArg@76..80
                        VarRef@76..79
                          Ident@76..79 "ix2"
                        Comma@79..80 ","
                      Whitespace@80..81 " "
                      FnArg@81..85
                        VarRef@81..85
                          Ident@81..85 "elm2"
                      RParen@85..86 ")"
                    Whitespace@86..87 " "
                    RBrace@87..88 "}""#]],
      ),
      for_loop_with_nested_and_mut_tuple_pattern: ("for ((ix,mut elm), (mut ix2, mut elm2,),) in arr[0_arr.len()].enumerate() { print(ix, elm, ix2, elm2) }",
          expect![[r#"
              Root@0..103
                ForLoop@0..103
                  KwFor@0..3 "for"
                  Whitespace@3..4 " "
                  ForIdent@4..41
                    Tuple@4..41
                      LParen@4..5 "("
                      Tuple@5..17
                        LParen@5..6 "("
                        VarRef@6..8
                          Ident@6..8 "ix"
                        Comma@8..9 ","
                        Mut@9..16
                          KwMut@9..12 "mut"
                          VarRef@12..16
                            Whitespace@12..13 " "
                            Ident@13..16 "elm"
                        RParen@16..17 ")"
                      Comma@17..18 ","
                      Whitespace@18..19 " "
                      Tuple@19..39
                        LParen@19..20 "("
                        Mut@20..27
                          KwMut@20..23 "mut"
                          VarRef@23..27
                            Whitespace@23..24 " "
                            Ident@24..27 "ix2"
                        Comma@27..28 ","
                        Whitespace@28..29 " "
                        Mut@29..37
                          KwMut@29..32 "mut"
                          VarRef@32..37
                            Whitespace@32..33 " "
                            Ident@33..37 "elm2"
                        Comma@37..38 ","
                        RParen@38..39 ")"
                      Comma@39..40 ","
                      RParen@40..41 ")"
                  In@41..74
                    Whitespace@41..42 " "
                    KwIn@42..44 "in"
                    Whitespace@44..45 " "
                    InfixBinOp@45..74
                      ContainerRef@45..61
                        Ident@45..48 "arr"
                        Indexing@48..61
                          LBrack@48..49 "["
                          InfixBinOp@49..60
                            Literal@49..50
                              Int@49..50 "0"
                            Under@50..51 "_"
                            InfixBinOp@51..60
                              VarRef@51..54
                                Ident@51..54 "arr"
                              Dot@54..55 "."
                              Call@55..60
                                Ident@55..58 "len"
                                LParen@58..59 "("
                                RParen@59..60 ")"
                          RBrack@60..61 "]"
                      Dot@61..62 "."
                      Call@62..73
                        Ident@62..71 "enumerate"
                        LParen@71..72 "("
                        RParen@72..73 ")"
                      Whitespace@73..74 " "
                  Block@74..103
                    LBrace@74..75 "{"
                    Whitespace@75..76 " "
                    Call@76..101
                      Ident@76..81 "print"
                      LParen@81..82 "("
                      FnArg@82..85
                        VarRef@82..84
                          Ident@82..84 "ix"
                        Comma@84..85 ","
                      Whitespace@85..86 " "
                      FnArg@86..90
                        VarRef@86..89
                          Ident@86..89 "elm"
                        Comma@89..90 ","
                      Whitespace@90..91 " "
                      FnArg@91..95
                        VarRef@91..94
                          Ident@91..94 "ix2"
                        Comma@94..95 ","
                      Whitespace@95..96 " "
                      FnArg@96..100
                        VarRef@96..100
                          Ident@96..100 "elm2"
                      RParen@100..101 ")"
                    Whitespace@101..102 " "
                    RBrace@102..103 "}""#]],
      ),
      for_loop_with_csv_identifiers_errors: ("for ix,elm in arr[0_arr.len()].enumerate() { print(ix, elm) }",
          expect![[r#"
              Root@0..61
                ForLoop@0..61
                  KwFor@0..3 "for"
                  Whitespace@3..4 " "
                  ForIdent@4..11
                    Ident@4..6 "ix"
                    Recovered@6..7
                      Comma@6..7 ","
                    Recovered@7..10
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
