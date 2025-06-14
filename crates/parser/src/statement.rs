use crate::{operator::starting_precedence, parse::Finished, parser::Parser};

use syntax::syntax_kind::SyntaxKind::*;

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
            Ok(syntax) if syntax.is_of_kind(KwLet) => self.parse_let_binding(),
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
        parse_multiple_statements: (
            "let a = 1;\na",
            expect![[r#"
                Root@0..12
                  LetBinding@0..10
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
                  LetBinding@0..10
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
    }
}
