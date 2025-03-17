use crate::{operator::starting_precedence, parser::Parser};

use syntax::{
    anchor::RollingBackAnchor,
    syntax_kind::SyntaxKind::{self, *},
};

#[allow(unused_variables)]
impl<'input> Parser<'input> {
    // the identifier is already bumped at this point because we ended up here
    // from parsing an identifier and saw a LBrack as well.
    #[allow(unused_variables)]
    pub fn parse_container_indexing(&self) {
        while self.is_next(LBrack) {
            let marker = self.start();
            {
                let rollback_when_dropped = self.disallow_recovery_of_for_indexing(RBrack);
                self.expect_and_bump(LBrack);
                self.parse_expression_until_binding_power(starting_precedence());
            }
            {
                let rollback_when_dropped = self.disallow_recovery_of_for_indexing(LBrack);
                self.expect_and_bump(RBrack);
            }
            self.complete_marker_with(marker, Indexing);
        }
    }
    fn disallow_recovery_of_for_indexing(&self, kind: SyntaxKind) -> RollingBackAnchor {
        let rollback_when_dropped = self.roll_back_context_after_drop();
        let ctx = self.context.borrow();
        ctx.disallow_recovery_of(kind);
        rollback_when_dropped
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
            empty_array_index: ("[]",
            expect![[r#"
                Root@0..2
                  Indexing@0..2
                    LBrack@0..1 "["
                    RBrack@1..2 "]""#]]
        ),

        missing_closing_bracket_does_not_panic: ("[",
            expect![[r#"
                Root@0..1
                  Indexing@0..1
                    LBrack@0..1 "[""#]]
        ),

        arr_indexing: ("arr[arr.len() - 1]",
            expect![[r#"
                Root@0..18
                  ContainerRef@0..18
                    Ident@0..3 "arr"
                    Indexing@3..18
                      LBrack@3..4 "["
                      InfixBinOp@4..17
                        InfixBinOp@4..14
                          VarRef@4..7
                            Ident@4..7 "arr"
                          Dot@7..8 "."
                          FnCall@8..13
                            Ident@8..11 "len"
                            LParen@11..12 "("
                            RParen@12..13 ")"
                          Whitespace@13..14 " "
                        Minus@14..15 "-"
                        Whitespace@15..16 " "
                        Literal@16..17
                          Int@16..17 "1"
                      RBrack@17..18 "]""#]],
        ),
        tensor_indexing: ("tensor[tensor[0].len() - 1][0]",
            expect![[r#"
                Root@0..30
                  ContainerRef@0..30
                    Ident@0..6 "tensor"
                    Indexing@6..27
                      LBrack@6..7 "["
                      InfixBinOp@7..26
                        InfixBinOp@7..23
                          ContainerRef@7..16
                            Ident@7..13 "tensor"
                            Indexing@13..16
                              LBrack@13..14 "["
                              Literal@14..15
                                Int@14..15 "0"
                              RBrack@15..16 "]"
                          Dot@16..17 "."
                          FnCall@17..22
                            Ident@17..20 "len"
                            LParen@20..21 "("
                            RParen@21..22 ")"
                          Whitespace@22..23 " "
                        Minus@23..24 "-"
                        Whitespace@24..25 " "
                        Literal@25..26
                          Int@25..26 "1"
                      RBrack@26..27 "]"
                    Indexing@27..30
                      LBrack@27..28 "["
                      Literal@28..29
                        Int@28..29 "0"
                      RBrack@29..30 "]""#]]
        ),
        var_def_from_arr: ("let z = arr[me.z - she.z];",
            expect![[r#"
                Root@0..26
                  VarDef@0..26
                    KwLet@0..3 "let"
                    Whitespace@3..4 " "
                    InfixBinOp@4..25
                      VarRef@4..6
                        Ident@4..5 "z"
                        Whitespace@5..6 " "
                      Eq@6..7 "="
                      Whitespace@7..8 " "
                      ContainerRef@8..25
                        Ident@8..11 "arr"
                        Indexing@11..25
                          LBrack@11..12 "["
                          InfixBinOp@12..24
                            InfixBinOp@12..17
                              VarRef@12..14
                                Ident@12..14 "me"
                              Dot@14..15 "."
                              VarRef@15..17
                                Ident@15..16 "z"
                                Whitespace@16..17 " "
                            Minus@17..18 "-"
                            Whitespace@18..19 " "
                            InfixBinOp@19..24
                              VarRef@19..22
                                Ident@19..22 "she"
                              Dot@22..23 "."
                              VarRef@23..24
                                Ident@23..24 "z"
                          RBrack@24..25 "]"
                    Semi@25..26 ";""#]]
        ),
    }
}
