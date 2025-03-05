use lexer::token_type::TokenType;
use syntax::{Syntax, syntax_kind::SyntaxKind};

use crate::{
    cst::ConcreteSyntaxTree,
    marker::{Complete, Incomplete, Marker},
    operator::{AssocBinOp, AssocUnOp, Bound, Op, starting_precedence},
    parser::Parser,
    sink::Sink,
};
pub type Finished = Marker<Complete>;
pub type Started = Marker<Incomplete>;

use SyntaxKind::*;

impl<'input> Parser<'input> {
    pub fn parse(mut self) -> ConcreteSyntaxTree {
        let root = self.start();
        while !self.is_at_the_end() {
            self.parse_statement();
        }
        self.complete_marker_with(root, Root);

        let sink = Sink::new(self.event_holder.into(), self.lexer.source());
        ConcreteSyntaxTree::from(sink)
    }
    fn parse_statement(&mut self) -> Option<Finished> {
        let mut syntax = self.peek()?;
        while let Err(err) = syntax {
            self.recover_from_err(err);
            syntax = self.peek()?;
        }
        let marker = match self.peek()? {
            Ok(syntax) if syntax.is_of_kind(KwLet) => self.parse_variable_def(),
            Ok(syntax) if syntax.is_of_kind(KwFn) => self.parse_function_def(),
            _ => self.parse_expression_until_binding_power(starting_precedence()),
        };
        if self.is_next(Semi) {
            if marker.is_some() {
                // let semi = marker?.precede(self);
                // self.bump();
                self.lexer.next();
                // Some(semi.complete(&mut self.event_holder, Semi))
            } else {
                self.recover();
            }
        }
        marker
    }

    fn parse_variable_def(&mut self) -> Option<Finished> {
        self.clean_buffer();
        let marker = self.start();
        self.expect_and_bump(KwLet);
        self.context.expect(Ident);
        if self
            .parse_expression_until_binding_power(starting_precedence())
            .is_none()
        {
            let got = self.peek();
            self.recover_with_msg("expected a valid assignment", got);
        }
        // if self.context.get_expectations().has_any() {
        //     self.recover_unmet_expectation();
        //     self.context.del_expectation(Ident);
        // }
        Some(self.complete_marker_with(marker, VarDef))
    }

    pub fn parse_expression_until_binding_power(
        &mut self,
        min_precedence: Bound,
    ) -> Option<Finished> {
        let mut lhs_marker = self.parse_left_hand_side()?;
        // let checkpoint = self.context.clone();
        // TODO: implement a type constraint as well
        // self.context.forbid(&[]);
        while let Some(peeked) = self.peek() {
            match peeked {
                // note: recovery is handled within specialized parse methods
                // this is just for debugging
                Err(parse_err) => {
                    panic!("{:?}", format!("{:?}", parse_err));
                }
                Ok(syntax) => {
                    let kind = syntax.get_kind();
                    self.clean_buffer();
                    // TODO: cannot let stuff slide here
                    if kind.is_literal_value() || syntax.is_of_type(TokenType::Keyword) {
                        self.recover();
                        return None;
                    }

                    if kind.is_posfix_unary_operator() {
                        if let Some(marker) =
                            self.parse_postfix_unary_operation(kind, &min_precedence, &lhs_marker)
                        {
                            lhs_marker = marker;
                            continue;
                        }
                    }
                    if kind.is_binary_operator() {
                        if let Some(marker) =
                            self.parse_binary_operation(syntax, &min_precedence, &lhs_marker)
                        {
                            lhs_marker = marker;
                            continue;
                        }
                    }
                }
            }
            break;
        }
        Some(lhs_marker)
    }

    fn parse_left_hand_side(&mut self) -> Option<Marker<Complete>> {
        use SyntaxKind::*;
        // note: after with a bump, expectations will be cleared
        let syntax = match self.peek()? {
            Ok(syntax) => syntax,
            Err(err) => {
                self.recover_from_err(err);
                return None;
            }
        };
        self.clean_buffer();
        let kind = syntax.get_kind();
        let marker = match kind {
            Ident => self.parse_variable_ref_or_fn_call(),
            Int | Float | StrLit | CharLit => self.parse_literal(kind),
            LParen => self.parse_paren_expr(syntax),
            LBrace => self.parse_block(),
            Minus | Excl => self.parse_prefix_unary_operation(kind),
            delimiter if delimiter.is_closing_delimiter() => {
                if !self.context.is_allowed(delimiter) {
                    println!("delimiter not allowed: {:?}", delimiter);
                    self.recover_restricted(delimiter);
                }
                None
            }
            operator
                if operator.is_binary_operator()
                    || operator.is_posfix_unary_operator()
                    || operator.is_prefix_unary_operator() =>
            {
                if !self.context.is_expected(operator) {
                    self.recover_unmet_expectation();
                } else if !self.context.is_allowed(operator) {
                    self.recover_restricted(operator);
                }
                None
            }
            _ => None,
        };
        self.clean_buffer();
        marker
    }

    fn parse_variable_ref_or_fn_call(&mut self) -> Option<Finished> {
        self.context.expect(Ident);
        let marker = self.start();
        self.bump();

        // function call
        let finished = if self.is_next(LParen) {
            self.expect_and_bump(LParen);
            self.parse_comma_separated_arguments_until(|syntax: Syntax| !syntax.is_of_kind(RParen));
            self.expect_and_bump(RParen);
            self.complete_marker_with(marker, FnCall)
        } else if self.is_next(LBrack) {
            self.expect_and_bump(LBrack);
            self.parse_expression_until_binding_power(starting_precedence());
            self.expect_and_bump(RBrack);
            self.complete_marker_with(marker, ArrRef)
        } else {
            let var_ref_marker = self.complete_marker_with(marker, VarRef);
            var_ref_marker
        };

        // above is_next puts further whitespaces in the buffer to look ahead,
        // thus after we decide that it is a VarRef, we should consume them to not contaminate
        // preceding parses.
        self.clean_buffer();
        Some(finished)
    }

    fn parse_literal(&mut self, kind: SyntaxKind) -> Option<Finished> {
        self.expect_and_bump_with_marker(kind, SyntaxKind::Literal)
    }

    // TODO: wrap this with a parse_delimited method
    fn parse_paren_expr(&mut self, syntax: Syntax) -> Option<Finished> {
        // TODO: LParen should inject RParen expectations
        use SyntaxKind::*;
        let marker = self.start();
        self.expect_and_bump(LParen);
        let checkpoint = self.context.clone();
        // restricts ], } (recovers them by erroring), expects )
        self.impose_restrictions_of(syntax);
        _ = self.parse_expression_until_binding_power(starting_precedence());
        self.expect_and_bump(RParen);
        self.context = checkpoint;
        let finished = self.complete_marker_with(marker, SyntaxKind::ParenExpr);
        self.clean_buffer();
        Some(finished)
    }

    pub fn parse_block(&mut self) -> Option<Finished> {
        let marker = self.start();
        self.expect_and_bump(LBrace);
        let checkpoint = self.context.clone();
        self.context.disallow_recovery_of(RBrace);
        while self.parse_statement().is_some() {}
        self.expect_and_bump(RBrace);
        self.context = checkpoint;
        Some(self.complete_marker_with(marker, Block))
    }

    fn parse_prefix_unary_operation(&mut self, kind: SyntaxKind) -> Option<Finished> {
        // TODO: I can expect a SyntaxKind::{PrefixUnaryOp or PostFixUnaryOp}
        let prefix_unary_op = AssocUnOp::from_syntax_kind(kind)?;
        let marker = self.start();
        self.expect_and_bump(kind);
        let bounded_precedence = Bound::from_op(prefix_unary_op.clone());
        _ = self.parse_expression_until_binding_power(bounded_precedence);
        Some(self.complete_marker_with(marker, prefix_unary_op.into()))
    }

    fn parse_postfix_unary_operation(
        &mut self,
        kind: SyntaxKind,
        min_precedence: &Bound,
        marker: &Marker<Complete>,
    ) -> Option<Finished> {
        // TODO: I can expect a SyntaxKind::{PrefixUnaryOp or PostFixUnaryOp}
        let postfix_unary_op = AssocUnOp::from_syntax_kind(kind)?;
        let precedence = postfix_unary_op.precedence();
        // this should never be the case since it has the highest precedence
        if min_precedence.gt(&precedence) {
            return None;
        }
        let marker = self.precede_marker_with(marker);
        self.expect_and_bump(kind);
        Some(self.complete_marker_with(marker, postfix_unary_op.into()))
    }

    fn parse_binary_operation(
        &mut self,
        syntax: Syntax,
        min_precedence: &Bound,
        marker: &Marker<Complete>,
    ) -> Option<Finished> {
        let kind = syntax.get_kind();
        let binary_op = AssocBinOp::from_syntax_kind(kind)?;
        let precedence = binary_op.precedence();
        if min_precedence.gt(&precedence) {
            return None;
        }
        let marker = self.precede_marker_with(marker);
        self.expect_and_bump(kind);
        let bounded_precedence = Bound::from_op(binary_op.clone());
        let checkpoint = self.context.clone();
        self.impose_restrictions_of(syntax);
        self.parse_expression_until_binding_power(bounded_precedence);
        self.context = checkpoint;
        Some(self.complete_marker_with(marker, binary_op.into()))
    }
}

#[cfg(test)]
mod tests {
    use crate::errors::ParseError;

    use super::*;
    use expect_test::{Expect, expect};
    use miette::Report;
    use parameterized_test::create;
    use std::ops::Range;
    use thin_vec::{ThinVec, thin_vec};

    pub fn check(prog: &str, expect: Expect) {
        let parse = Parser::new(prog).parse();
        let debug_tree = parse.debug_tree();
        expect.assert_eq(&debug_tree);
    }

    fn check_err(
        input: &str,
        expected: ThinVec<SyntaxKind>,
        found: Option<SyntaxKind>,
        range: Range<usize>,
        output: &str,
    ) {
        let error = ParseError::new(range, expected, found);
        let mut report: Report = error.into();
        report = report.with_source_code(input.to_string());
        assert_eq!(format!("{:?}", report), output);
    }

    #[test]
    fn one_expected_did_find() {
        check_err(
            "let a = ",
            thin_vec![SyntaxKind::Literal],
            None,
            8..9,
            "  \u{1b}[31m×\u{1b}[0m Parsing error\n   ╭────\n \u{1b}[2m1\u{1b}[0m │ let a = \n   ╰────\n\u{1b}[36m  help: \u{1b}[0mexpected Literal, but got ''\n",
        );
    }

    create! {
        create_parser_test,
        (prog, expect), {
            check(prog, expect);
        }
    }

    create_parser_test! {
        nothing: ("", expect![["Root@0..0"]]),
        single_digit: ("9", expect![["Root@0..1\n  Literal@0..1\n    Int@0..1 \"9\""]]),
        multiple_digit: ("314", expect![["Root@0..3\n  Literal@0..3\n    Int@0..3 \"314\""]]),
        identifier: ("ident", expect![["Root@0..5\n  VarRef@0..5\n    Ident@0..5 \"ident\""]]),
        identifier_with_semicolon: ("ident;", expect![["Root@0..5\n  VarRef@0..5\n    Ident@0..5 \"ident\""]]),
        ignored_whitespaces: ("   ", expect![["Root@0..0"]]),
        binary_add_two_numbers: ("3+14",
            expect![[
            "Root@0..4\n  InfixBinOp@0..4\n    Literal@0..1\n      Int@0..1 \"3\"\n    Plus@1..2 \"+\"\n    Literal@2..4\n      Int@2..4 \"14\""
            ]]),
        binary_add_two_numbers_with_semicolon: ("3+14;",
            expect![[
            "Root@0..4\n  InfixBinOp@0..4\n    Literal@0..1\n      Int@0..1 \"3\"\n    Plus@1..2 \"+\"\n    Literal@2..4\n      Int@2..4 \"14\""
            ]]),

        binary_dot_member: ("structure.member",
            expect![[
r#"Root@0..16
  InfixBinOp@0..16
    VarRef@0..9
      Ident@0..9 "structure"
    Dot@9..10 "."
    VarRef@10..16
      Ident@10..16 "member""#
            ]]),
        binary_dot_member_with_semicolon: ("structure.member;",
            expect![[
r#"Root@0..16
  InfixBinOp@0..16
    VarRef@0..9
      Ident@0..9 "structure"
    Dot@9..10 "."
    VarRef@10..16
      Ident@10..16 "member""#
            ]]),

        binary_dot_member_precedence: ("human.weight + 1 / 100",
            expect![[r#"
                Root@0..22
                  InfixBinOp@0..22
                    InfixBinOp@0..13
                      VarRef@0..5
                        Ident@0..5 "human"
                      Dot@5..6 "."
                      VarRef@6..12
                        Ident@6..12 "weight"
                      Whitespace@12..13 " "
                    Plus@13..14 "+"
                    Whitespace@14..15 " "
                    InfixBinOp@15..22
                      Literal@15..16
                        Int@15..16 "1"
                      Whitespace@16..17 " "
                      Slash@17..18 "/"
                      Whitespace@18..19 " "
                      Literal@19..22
                        Int@19..22 "100""#]]),

        binary_dot_member_precedence_with_semi_colon: ("human.weight + 1 /100;",
            expect![[r#"
                Root@0..21
                  InfixBinOp@0..21
                    InfixBinOp@0..13
                      VarRef@0..5
                        Ident@0..5 "human"
                      Dot@5..6 "."
                      VarRef@6..12
                        Ident@6..12 "weight"
                      Whitespace@12..13 " "
                    Plus@13..14 "+"
                    Whitespace@14..15 " "
                    InfixBinOp@15..21
                      Literal@15..16
                        Int@15..16 "1"
                      Whitespace@16..17 " "
                      Slash@17..18 "/"
                      Literal@18..21
                        Int@18..21 "100""#]]),

        binary_add_four_numbers: ("3+14+159+2653",
            expect![[
            "Root@0..13\n  InfixBinOp@0..13\n    InfixBinOp@0..8\n      InfixBinOp@0..4\n        Literal@0..1\n          Int@0..1 \"3\"\n        Plus@1..2 \"+\"\n        Literal@2..4\n          Int@2..4 \"14\"\n      Plus@4..5 \"+\"\n      Literal@5..8\n        Int@5..8 \"159\"\n    Plus@8..9 \"+\"\n    Literal@9..13\n      Int@9..13 \"2653\""
            ]]
        ),
        binary_add_four_numbers_with_semicolon: ("3+14*159-2653;",
            expect![[
            "Root@0..13\n  InfixBinOp@0..13\n    InfixBinOp@0..8\n      Literal@0..1\n        Int@0..1 \"3\"\n      Plus@1..2 \"+\"\n      InfixBinOp@2..8\n        Literal@2..4\n          Int@2..4 \"14\"\n        Star@4..5 \"*\"\n        Literal@5..8\n          Int@5..8 \"159\"\n    Minus@8..9 \"-\"\n    Literal@9..13\n      Int@9..13 \"2653\""
            ]]
        ),
        binary_add_mul_sub_four_numbers: ("3+14*159-2653",
            expect![[
            "Root@0..13\n  InfixBinOp@0..13\n    InfixBinOp@0..8\n      Literal@0..1\n        Int@0..1 \"3\"\n      Plus@1..2 \"+\"\n      InfixBinOp@2..8\n        Literal@2..4\n          Int@2..4 \"14\"\n        Star@4..5 \"*\"\n        Literal@5..8\n          Int@5..8 \"159\"\n    Minus@8..9 \"-\"\n    Literal@9..13\n      Int@9..13 \"2653\""
            ]]
        ),
        prefix_minus_digit: ("-9",
            expect![[
                "Root@0..2\n  PrefixUnaryOp@0..2\n    Minus@0..1 \"-\"\n    Literal@1..2\n      Int@1..2 \"9\""
            ]]
        ),
        prefix_minus_digit_with_semicolon: ("-9;",
            expect![[
                "Root@0..2\n  PrefixUnaryOp@0..2\n    Minus@0..1 \"-\"\n    Literal@1..2\n      Int@1..2 \"9\""
            ]]
        ),

        prefix_not_var_ref: ("!a",
            expect![[
r#"
Root@0..2
  PrefixUnaryOp@0..2
    Excl@0..1 "!"
    VarRef@1..2
      Ident@1..2 "a""#
            ]]
        ),

        prefix_not_var_ref_with_semicolon: ("!a;",
            expect![[
r#"
Root@0..2
  PrefixUnaryOp@0..2
    Excl@0..1 "!"
    VarRef@1..2
      Ident@1..2 "a""#
            ]]
        ),
        postfix_excl_prefix_neg_precedence: ("-9?",
            expect![[
r#"
Root@0..3
  PrefixUnaryOp@0..3
    Minus@0..1 "-"
    PostFixUnaryOp@1..3
      Literal@1..2
        Int@1..2 "9"
      QMark@2..3 "?""#
            ]]
        ),
        postfix_excl_prefix_neg_precedence_with_semicolon: ("-9?;",
            expect![[
r#"
Root@0..3
  PrefixUnaryOp@0..3
    Minus@0..1 "-"
    PostFixUnaryOp@1..3
      Literal@1..2
        Int@1..2 "9"
      QMark@2..3 "?""#
            ]]
        ),
        postfix_excl_infix_mul_precedence: ("1*9?",
            expect![[
r#"
Root@0..4
  InfixBinOp@0..4
    Literal@0..1
      Int@0..1 "1"
    Star@1..2 "*"
    PostFixUnaryOp@2..4
      Literal@2..3
        Int@2..3 "9"
      QMark@3..4 "?""#
            ]]
        ),
        prefix_minus_sum_precedence: ("-3+14",
            expect![[
                "Root@0..5\n  InfixBinOp@0..5\n    PrefixUnaryOp@0..2\n      Minus@0..1 \"-\"\n      Literal@1..2\n        Int@1..2 \"3\"\n    Plus@2..3 \"+\"\n    Literal@3..5\n      Int@3..5 \"14\""
            ]]
        ),
        only_parenthesis: ("()",
            expect![[
                "Root@0..2\n  ParenExpr@0..2\n    LParen@0..1 \"(\"\n    RParen@1..2 \")\""
            ]]
        ),

        missing_closing_parenthesis_does_not_panic: ("(",
            expect![[
                "Root@0..1\n  ParenExpr@0..1\n    LParen@0..1 \"(\""
            ]]
        ),

        parenthesised_pi: ("((314))",
            expect![[
                "Root@0..7\n  ParenExpr@0..7\n    LParen@0..1 \"(\"\n    ParenExpr@1..6\n      LParen@1..2 \"(\"\n      Literal@2..5\n        Int@2..5 \"314\"\n      RParen@5..6 \")\"\n    RParen@6..7 \")\""
            ]]
        ),
        parenthesised_pi_with_semicolon: ("((314));",
            expect![[r#"
                Root@0..7
                  ParenExpr@0..7
                    LParen@0..1 "("
                    ParenExpr@1..6
                      LParen@1..2 "("
                      Literal@2..5
                        Int@2..5 "314"
                      RParen@5..6 ")"
                    RParen@6..7 ")""#]]
        ),
        parenthesised_pi_with_semicolon_inside: ("((314;));",
            expect![[r#"
                Root@0..8
                  ParenExpr@0..5
                    LParen@0..1 "("
                    ParenExpr@1..5
                      LParen@1..2 "("
                      Literal@2..5
                        Int@2..5 "314"
                  Recovered@5..6
                    RParen@5..6 ")"
                  Recovered@6..7
                    RParen@6..7 ")"
                  Recovered@7..8
                    Semi@7..8 ";""#]]
        ),
        parenthesised_sub_precedes_mul: ("(3+1)*4",
            expect![[
                "Root@0..7\n  InfixBinOp@0..7\n    ParenExpr@0..5\n      LParen@0..1 \"(\"\n      InfixBinOp@1..4\n        Literal@1..2\n          Int@1..2 \"3\"\n        Plus@2..3 \"+\"\n        Literal@3..4\n          Int@3..4 \"1\"\n      RParen@4..5 \")\"\n    Star@5..6 \"*\"\n    Literal@6..7\n      Int@6..7 \"4\""
            ]]
        ),

        block_with_returning: ("{let a = 0; a}",
            expect![[r#"
                Root@0..13
                  Block@0..13
                    LBrace@0..1 "{"
                    VarDef@1..10
                      KwLet@1..4 "let"
                      Whitespace@4..5 " "
                      InfixBinOp@5..10
                        VarRef@5..6
                          Ident@5..6 "a"
                        Whitespace@6..7 " "
                        Eq@7..8 "="
                        Whitespace@8..9 " "
                        Literal@9..10
                          Int@9..10 "0"
                    Whitespace@10..11 " "
                    VarRef@11..12
                      Ident@11..12 "a"
                    RBrace@12..13 "}""#]],
        ),

        block_with_multiple_statements_and_expressions: ("{let a = 0;\n let b = 1; let c= 2; a+b+c}",
            expect![[r#"
                Root@0..37
                  Block@0..37
                    LBrace@0..1 "{"
                    VarDef@1..10
                      KwLet@1..4 "let"
                      Whitespace@4..5 " "
                      InfixBinOp@5..10
                        VarRef@5..6
                          Ident@5..6 "a"
                        Whitespace@6..7 " "
                        Eq@7..8 "="
                        Whitespace@8..9 " "
                        Literal@9..10
                          Int@9..10 "0"
                    Whitespace@10..11 "\n"
                    Whitespace@11..12 " "
                    VarDef@12..21
                      KwLet@12..15 "let"
                      Whitespace@15..16 " "
                      InfixBinOp@16..21
                        VarRef@16..17
                          Ident@16..17 "b"
                        Whitespace@17..18 " "
                        Eq@18..19 "="
                        Whitespace@19..20 " "
                        Literal@20..21
                          Int@20..21 "1"
                    Whitespace@21..22 " "
                    VarDef@22..30
                      KwLet@22..25 "let"
                      Whitespace@25..26 " "
                      InfixBinOp@26..30
                        VarRef@26..27
                          Ident@26..27 "c"
                        Eq@27..28 "="
                        Whitespace@28..29 " "
                        Literal@29..30
                          Int@29..30 "2"
                    Whitespace@30..31 " "
                    InfixBinOp@31..36
                      InfixBinOp@31..34
                        VarRef@31..32
                          Ident@31..32 "a"
                        Plus@32..33 "+"
                        VarRef@33..34
                          Ident@33..34 "b"
                      Plus@34..35 "+"
                      VarRef@35..36
                        Ident@35..36 "c"
                    RBrace@36..37 "}""#]],
        ),

        malformed_var_defs: ("let a = let b = let c = 5",
            expect![[r#"
                Root@0..25
                  VarDef@0..8
                    KwLet@0..3 "let"
                    Whitespace@3..4 " "
                    InfixBinOp@4..8
                      VarRef@4..5
                        Ident@4..5 "a"
                      Whitespace@5..6 " "
                      Eq@6..7 "="
                      Whitespace@7..8 " "
                  VarDef@8..16
                    KwLet@8..11 "let"
                    Whitespace@11..12 " "
                    InfixBinOp@12..16
                      VarRef@12..13
                        Ident@12..13 "b"
                      Whitespace@13..14 " "
                      Eq@14..15 "="
                      Whitespace@15..16 " "
                  VarDef@16..25
                    KwLet@16..19 "let"
                    Whitespace@19..20 " "
                    InfixBinOp@20..25
                      VarRef@20..21
                        Ident@20..21 "c"
                      Whitespace@21..22 " "
                      Eq@22..23 "="
                      Whitespace@23..24 " "
                      Literal@24..25
                        Int@24..25 "5""#]]
        ),


        function_def_with_no_parameters: ("fn empty() {}",
            expect![[r#"
                Root@0..13
                  VarDef@0..13
                    KwFn@0..2 "fn"
                    Whitespace@2..3 " "
                    Ident@3..8 "empty"
                    LParen@8..9 "("
                    RParen@9..10 ")"
                    Whitespace@10..11 " "
                    Block@11..13
                      LBrace@11..12 "{"
                      RBrace@12..13 "}""#]],
        ),

        function_def_with_single_parameter: ("fn empty(single:i32) {}",
            expect![[r#"
                Root@0..23
                  VarDef@0..23
                    KwFn@0..2 "fn"
                    Whitespace@2..3 " "
                    Ident@3..8 "empty"
                    LParen@8..9 "("
                    ParamDecl@9..19
                      Ident@9..15 "single"
                      Colon@15..16 ":"
                      TyI32@16..19 "i32"
                    RParen@19..20 ")"
                    Whitespace@20..21 " "
                    Block@21..23
                      LBrace@21..22 "{"
                      RBrace@22..23 "}""#]],
        ),

        function_def_with_multiple_parameters: ("fn empty(first:i32, second:char, third: Structure) {}",
            expect![[r#"
                Root@0..51
                  VarDef@0..51
                    KwFn@0..2 "fn"
                    Whitespace@2..3 " "
                    Ident@3..8 "empty"
                    LParen@8..9 "("
                    ParamDecl@9..18
                      Ident@9..14 "first"
                      Colon@14..15 ":"
                      TyI32@15..18 "i32"
                    Whitespace@18..19 " "
                    ParamDecl@19..30
                      Ident@19..25 "second"
                      Colon@25..26 ":"
                      TyChar@26..30 "char"
                    Whitespace@30..31 " "
                    ParamDecl@31..47
                      Ident@31..36 "third"
                      Colon@36..37 ":"
                      Whitespace@37..38 " "
                      Ident@38..47 "Structure"
                    RParen@47..48 ")"
                    Whitespace@48..49 " "
                    Block@49..51
                      LBrace@49..50 "{"
                      RBrace@50..51 "}""#]],
        ),

        function_def_with_multiple_parameters_with_actual_body: ("fn empty(first:i32, second:char) {let sum_1 = first + second; let sum_2 = second+first; first == check}",
            expect![[r#"
                Root@0..100
                  VarDef@0..100
                    KwFn@0..2 "fn"
                    Whitespace@2..3 " "
                    Ident@3..8 "empty"
                    LParen@8..9 "("
                    ParamDecl@9..18
                      Ident@9..14 "first"
                      Colon@14..15 ":"
                      TyI32@15..18 "i32"
                    Whitespace@18..19 " "
                    ParamDecl@19..30
                      Ident@19..25 "second"
                      Colon@25..26 ":"
                      TyChar@26..30 "char"
                    RParen@30..31 ")"
                    Whitespace@31..32 " "
                    Block@32..100
                      LBrace@32..33 "{"
                      VarDef@33..59
                        KwLet@33..36 "let"
                        Whitespace@36..37 " "
                        InfixBinOp@37..59
                          VarRef@37..42
                            Ident@37..42 "sum_1"
                          Whitespace@42..43 " "
                          Eq@43..44 "="
                          Whitespace@44..45 " "
                          InfixBinOp@45..59
                            VarRef@45..50
                              Ident@45..50 "first"
                            Whitespace@50..51 " "
                            Plus@51..52 "+"
                            Whitespace@52..53 " "
                            VarRef@53..59
                              Ident@53..59 "second"
                      Whitespace@59..60 " "
                      VarDef@60..84
                        KwLet@60..63 "let"
                        Whitespace@63..64 " "
                        InfixBinOp@64..84
                          VarRef@64..69
                            Ident@64..69 "sum_2"
                          Whitespace@69..70 " "
                          Eq@70..71 "="
                          Whitespace@71..72 " "
                          InfixBinOp@72..84
                            VarRef@72..78
                              Ident@72..78 "second"
                            Plus@78..79 "+"
                            VarRef@79..84
                              Ident@79..84 "first"
                      Whitespace@84..85 " "
                      InfixBinOp@85..99
                        VarRef@85..90
                          Ident@85..90 "first"
                        Whitespace@90..91 " "
                        EqEq@91..93 "=="
                        Whitespace@93..94 " "
                        VarRef@94..99
                          Ident@94..99 "check"
                      RBrace@99..100 "}""#]],
        ),

        function_call_with_single_parameter: ("empty(single)",
            expect![[r#"
                Root@0..13
                  FnCall@0..13
                    Ident@0..5 "empty"
                    LParen@5..6 "("
                    FnArg@6..12
                      VarRef@6..12
                        Ident@6..12 "single"
                    RParen@12..13 ")""#]],
        ),

        arr_indexing: ("arr[arr.len() - 1]",
            expect![[r#"
                Root@0..18
                  ArrRef@0..18
                    Ident@0..3 "arr"
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
    }

    #[test]
    fn several_valid_comma_separated_parameter_declarations() {
        use SyntaxKind::*;
        let params = "me:human, lang: Language, pet: Cat)";
        let mut parser = Parser::new(params);
        let root = parser.start();
        parser.parse_comma_separated_typed_declarations_until(|syntax: Syntax| {
            !syntax.is_of_kind(RParen)
        });
        parser.complete_marker_with(root, Root);
        let sink = Sink::new(parser.event_holder.into(), parser.lexer.source());
        let cst = ConcreteSyntaxTree::from(sink);

        let expect = expect![[r#"
            Root@0..32
              ParamDecl@0..8
                Ident@0..2 "me"
                Colon@2..3 ":"
                Ident@3..8 "human"
              Whitespace@8..9 " "
              ParamDecl@9..23
                Ident@9..13 "lang"
                Colon@13..14 ":"
                Whitespace@14..15 " "
                Ident@15..23 "Language"
              Whitespace@23..24 " "
              ParamDecl@24..32
                Ident@24..27 "pet"
                Colon@27..28 ":"
                Whitespace@28..29 " "
                Ident@29..32 "Cat""#]];

        let debug_tree = cst.debug_tree();
        expect.assert_eq(&debug_tree);
    }
}
