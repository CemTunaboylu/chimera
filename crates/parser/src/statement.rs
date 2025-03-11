use crate::{operator::starting_precedence, parse::Finished, parser::Parser};

use syntax::syntax_kind::SyntaxKind::*;

impl<'input> Parser<'input> {
    pub fn parse_statement(&self) -> Option<Finished> {
        let mut syntax = self.peek()?;
        while let Err(err) = syntax {
            self.recover_from_err(err);
            syntax = self.peek()?;
        }
        // TODO: deal with semi: loops can return if not ; delimited
        let marker = match self.peek()? {
            Ok(syntax) if syntax.is_of_kind(KwFn) => self.parse_function_def(),
            Ok(syntax) if syntax.is_of_kind(KwFor) => self.parse_for_loop(),
            Ok(syntax) if syntax.is_of_kind(KwIf) => self.parse_conditionals(),
            Ok(syntax) if syntax.is_of_kind(KwImpl) => self.parse_impl_block(),
            Ok(syntax) if syntax.is_of_kind(KwLet) => self.parse_variable_def(),
            Ok(syntax) if syntax.is_of_kind(KwStruct) => self.parse_struct_definition(),
            Ok(syntax) if syntax.is_of_kind(KwWhile) => self.parse_while_loop(),
            // An expression produces a result (result of evalution), but if there is a ; at the end,
            // it becomes a statemet, thus check that here and wrap it with Semi
            _ => self.parse_expression_until_binding_power(starting_precedence()),
        };
        if self.is_next(Semi) {
            if marker.is_some() {
                // let semi = marker?.precede(self);
                // self.bump();
                self.ignore();
                // Some(semi.complete(&mut self.event_holder, Semi))
            } else {
                self.ignore();
                // self.recover();
            }
        }
        marker
    }

    #[allow(unused_variables)]
    pub fn parse_variable_def(&self) -> Option<Finished> {
        let marker = self.start();
        self.expect_and_bump(KwLet);
        let rollback_when_dropped = self.roll_back_context_after_drop();
        self.context.borrow().expect([Ident, Semi, VarDef].as_ref());
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
        malformed_var_defs: ("let a = let b = let c = 5",
            expect![[r#"
                Root@0..25
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
                  VarDef@16..25
                    KwLet@16..19 "let"
                    Whitespace@19..20 " "
                    InfixBinOp@20..25
                      VarRef@20..22
                        Ident@20..21 "c"
                        Whitespace@21..22 " "
                      Eq@22..23 "="
                      Whitespace@23..24 " "
                      Literal@24..25
                        Int@24..25 "5""#]]
        ),

    }

    #[test]
    fn parse_variable_definition() {
        check(
            "let foo = bar",
            expect![[r#"
                Root@0..10
                  VariableDef@0..10
                    LetKw@0..3 "let"
                    InfixBinaryOp@3..10
                      VariableRef@3..6
                        Identifier@3..6 "foo"
                      Eq@6..7 "="
                      VariableRef@7..10
                        Identifier@7..10 "bar""#]],
        );
    }

    #[test]
    fn recover_on_let_token() {
        check(
            "let a =\nlet b = a;",
            expect![[r#"
                Root@0..11
                  VariableDef@0..5
                    LetKw@0..3 "let"
                    InfixBinaryOp@3..5
                      VariableRef@3..4
                        Identifier@3..4 "a"
                      Eq@4..5 "="
                  VariableDef@5..11
                    LetKw@5..8 "let"
                    InfixBinaryOp@8..11
                      VariableRef@8..9
                        Identifier@8..9 "b"
                      Eq@9..10 "="
                      VariableRef@10..11
                        Identifier@10..11 "a""#]],
        );
    }
    #[test]
    fn recover_on_let_token_with_semicolon() {
        check(
            "let a =;let b = a;",
            expect![[r#"
                Root@0..11
                  VariableDef@0..5
                    LetKw@0..3 "let"
                    InfixBinaryOp@3..5
                      VariableRef@3..4
                        Identifier@3..4 "a"
                      Eq@4..5 "="
                  VariableDef@5..11
                    LetKw@5..8 "let"
                    InfixBinaryOp@8..11
                      VariableRef@8..9
                        Identifier@8..9 "b"
                      Eq@9..10 "="
                      VariableRef@10..11
                        Identifier@10..11 "a""#]],
        );
    }

    #[test]
    fn parse_multiple_statements() {
        check(
            "let a = 1;\na",
            expect![[r#"
                Root@0..7
                  VariableDef@0..7
                    LetKw@0..3 "let"
                    InfixBinaryOp@3..7
                      VariableRef@3..4
                        Identifier@3..4 "a"
                      Eq@4..5 "="
                      Literal@5..6
                        Number@5..6 "1"
                      VariableRef@6..7
                        Identifier@6..7 "a""#]],
        );
    }
    #[test]
    fn parse_multiple_statements_with_semicolon() {
        check(
            "let a = 0; a",
            expect![[r#"
                Root@0..7
                  VariableDef@0..6
                    LetKw@0..3 "let"
                    InfixBinaryOp@3..6
                      VariableRef@3..4
                        Identifier@3..4 "a"
                      Eq@4..5 "="
                      Literal@5..6
                        Number@5..6 "0"
                  VariableRef@6..7
                    Identifier@6..7 "a""#]],
        );
    }

    #[test]
    fn recover_on_let_token_in_block() {
        check(
            "{let a =\n{let b =} 10 }",
            expect![[r#"
                Root@0..16
                  Block@0..16
                    LBrace@0..1 "{"
                    VariableDef@1..15
                      LetKw@1..4 "let"
                      InfixBinaryOp@4..15
                        VariableRef@4..5
                          Identifier@4..5 "a"
                        Eq@5..6 "="
                        Block@6..13
                          LBrace@6..7 "{"
                          VariableDef@7..12
                            LetKw@7..10 "let"
                            InfixBinaryOp@10..12
                              VariableRef@10..11
                                Identifier@10..11 "b"
                              Eq@11..12 "="
                          RBrace@12..13 "}"
                        Recovered@13..15
                          Number@13..15 "10"
                    RBrace@15..16 "}""#]],
        );
    }

    #[test]
    fn recover_on_let_token_in_block_with_semicolon() {
        check(
            "{let a =\n{let b =}; 10 }",
            expect![[r#"
                Root@0..16
                  Block@0..16
                    LBrace@0..1 "{"
                    VariableDef@1..15
                      LetKw@1..4 "let"
                      InfixBinaryOp@4..15
                        VariableRef@4..5
                          Identifier@4..5 "a"
                        Eq@5..6 "="
                        Block@6..13
                          LBrace@6..7 "{"
                          VariableDef@7..12
                            LetKw@7..10 "let"
                            InfixBinaryOp@10..12
                              VariableRef@10..11
                                Identifier@10..11 "b"
                              Eq@11..12 "="
                          RBrace@12..13 "}"
                        Literal@13..15
                          Number@13..15 "10"
                    RBrace@15..16 "}""#]],
        );
    }
}
