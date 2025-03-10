use lexer::token_type::TokenType;
use syntax::{Syntax, syntax_kind::SyntaxKind};
use thin_vec::ThinVec;

use crate::{
    cst::ConcreteSyntaxTree,
    marker::{Complete, Incomplete, Marker},
    operator::{AssocBinOp, AssocUnOp, Bound, Op, is_an_operator, starting_precedence},
    parser::Parser,
    sink::Sink,
};
pub type Finished = Marker<Complete>;
pub type Started = Marker<Incomplete>;

use SyntaxKind::*;

pub type CustomExpectationOnSyntax = fn(Syntax) -> bool;

pub enum SeparatedElement {
    Kind(SyntaxKind),
    Fn(CustomExpectationOnSyntax),
    ParseExprWith(Bound),
}

impl<'input> Parser<'input> {
    pub fn parse(self) -> ConcreteSyntaxTree {
        let root = self.start();
        while !self.is_at_the_end() {
            self.parse_statement();
        }
        self.complete_marker_with(root, Root);

        let lexer = self.lexer.borrow();
        let sink = Sink::new(self.event_holder.into_inner().into(), lexer.source());
        ConcreteSyntaxTree::from(sink)
    }

    /*
       Given expectation to bump and functions to apply,
           elements_in_order: [ Kind(Ident), Kind(Colon), Fn(ident_or_type) ]
        TODO: can take a bitset
    */
    pub fn parse_separated_by(
        &self,
        elements_in_order: ThinVec<SeparatedElement>,
        wrapping_kind_to_complete: SyntaxKind,
        separator: SyntaxKind,
        until: fn(Syntax) -> bool,
    ) -> Option<()> {
        use SeparatedElement::*;
        while until(self.peek()?.ok()?) {
            self.clean_buffer();
            let marker = self.start();

            for element in elements_in_order.iter() {
                match element {
                    Kind(exp_kind) => {
                        self.expect_and_bump(exp_kind.clone());
                    }
                    Fn(custom_exp_func) => {
                        self.expect_f_and_bump(*custom_exp_func);
                    }
                    ParseExprWith(bound) => {
                        self.parse_expression_until_binding_power(bound.clone());
                    }
                }
            }

            self.ignore_if(separator);
            self.complete_marker_with(marker, wrapping_kind_to_complete);
            self.clean_buffer();
        }
        Some(())
    }

    pub fn parse_condition(&self) -> Option<Finished> {
        let marker = if self.is_next_f(|syntax| matches!(syntax.get_kind(), KwFalse | KwTrue)) {
            self.clean_buffer();
            self.bump_with_marker(Condition)
        } else {
            let cond_marker = self.start();
            self.parse_expression_until_binding_power(starting_precedence());
            self.complete_marker_with(cond_marker, Condition)
        };
        Some(marker)
    }

    fn parse_statement(&self) -> Option<Finished> {
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

    fn parse_variable_def(&self) -> Option<Finished> {
        self.clean_buffer();
        let marker = self.start();
        self.expect_and_bump(KwLet);
        self.context.borrow().expect([Ident, Semi].as_ref());
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

    pub fn parse_expression_until_binding_power(&self, min_precedence: Bound) -> Option<Finished> {
        let mut lhs_marker = self.parse_left_hand_side()?;
        while let Some(peeked) = self.peek() {
            match peeked {
                // note: recovery is handled within specialized parse methods
                // this is just for debugging
                Err(parse_err) => {
                    self.recover_from_err(parse_err);
                    return None;
                }
                Ok(syntax) => {
                    let kind = syntax.get_kind();
                    self.clean_buffer();
                    // TODO: cannot let stuff slide here
                    if Semi == kind && !self.context.borrow().is_expected(kind) {
                        self.ignore();
                        break;
                    }
                    if !is_an_operator(&kind) {
                        break;
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
                    break;
                }
            }
            // break;
        }
        Some(lhs_marker)
    }

    pub fn parse_left_hand_side(&self) -> Option<Marker<Complete>> {
        use SyntaxKind::*;
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
            Ident => self.parse_starting_with_identifier(),
            Int | Float | StrLit | CharLit | KwTrue | KwFalse => self.parse_literal(kind),
            Minus | Excl => self.parse_prefix_unary_operation(kind),
            keyword if keyword.is_keyword() => self.parse_keyword_expression(syntax),
            delimiter if delimiter.is_delimiter() => self.parse_delimited(syntax),
            operator if is_an_operator(&operator) => {
                if !self.context.borrow().is_expected(operator) {
                    self.recover_unmet_expectation();
                } else if !self.context.borrow().is_allowed(operator) {
                    self.recover_restricted(operator);
                }
                None
            }
            _ => None,
        };

        self.clean_buffer();
        marker
    }

    // Possible options: a variable reference, function/method call or an iterable indexing
    fn parse_starting_with_identifier(&self) -> Option<Finished> {
        let marker = self.start();
        self.expect_and_bump(Ident);

        // function call
        let finished = if self.is_next(LParen) {
            self.parse_function_call(marker)
        } else if self.is_next(LBrack) {
            self.parse_container_indexing(marker)
        } else {
            self.complete_marker_with(marker, VarRef)
        };

        // above is_next puts further whitespaces in the buffer to look ahead,
        // thus after we decide that it is a VarRef, we should consume them to not contaminate
        // preceding parses.
        self.clean_buffer();
        Some(finished)
    }

    fn parse_literal(&self, kind: SyntaxKind) -> Option<Finished> {
        self.expect_and_bump_with_marker(kind, Literal)
    }

    #[allow(unused_variables)]
    fn parse_break(&self) -> Option<Finished> {
        let marker = self.start();
        self.expect_and_bump(KwBreak);

        let rollback_after_drop = self.roll_back_context_after_drop();
        self.context.borrow().expect(Semi);

        self.parse_expression_until_binding_power(starting_precedence());

        let finished_as_expr = self.complete_marker_with(marker, Jump);
        if self.is_next(Semi) {
            let semi_marker = self.precede_marker_with(&finished_as_expr);
            self.expect_and_bump(Semi);
            Some(self.complete_marker_with(semi_marker, Semi))
        } else {
            Some(finished_as_expr)
        }
    }

    #[allow(unused_variables)]
    fn parse_return(&self) -> Option<Finished> {
        let marker = self.start();
        self.expect_and_bump(KwReturn);

        let rollback_after_drop = self.roll_back_context_after_drop();
        self.context.borrow().expect(Semi);

        self.parse_expression_until_binding_power(starting_precedence());

        let finished_as_expr = self.complete_marker_with(marker, Jump);
        let semi_marker = self.precede_marker_with(&finished_as_expr);
        self.expect_and_bump(Semi);
        Some(self.complete_marker_with(semi_marker, Semi))
    }
    fn parse_continue(&self) -> Option<Finished> {
        let marker = self.start();
        self.expect_and_bump(KwContinue);
        self.expect_and_bump(Semi);
        Some(self.complete_marker_with(marker, Jump))
    }

    pub fn parse_keyword_expression(&self, keyword: Syntax) -> Option<Finished> {
        match keyword.get_kind() {
            KwBreak => self.parse_break(),
            KwContinue => self.parse_continue(),
            // A variable definition is a statement, thus will be recovered
            KwIf => self.parse_conditionals(),
            KwLet => {
                self.recover();
                None
            }
            KwStruct => self.parse_struct_definition(),
            KwReturn => self.parse_return(),
            nope => {
                println!("{:?} is not implemented yet", nope);
                todo!()
            }
        }
    }

    #[allow(unused_variables)] // for rollback anchor
    pub fn parse_delimited(&self, syntax: Syntax) -> Option<Finished> {
        let kind = syntax.get_kind();
        if !self.context.borrow().is_allowed(kind) {
            self.recover_restricted(kind);
            return None;
        }
        use TokenType::*;
        match syntax.get_token_type() {
            OpeningDelimiter(expected_token_kind) => match kind {
                LParen => self.parse_paren_expr(syntax),
                LBrace => self.parse_block(),
                _ => unreachable!(),
            },
            ClosingDelimiter(expected_token_kind) => {
                return None;
            }
            _ => unreachable!(),
        }
    }

    #[allow(unused_variables)] // for rollback anchor
    pub fn parse_paren_expr(&self, syntax: Syntax) -> Option<Finished> {
        use SyntaxKind::*;
        let marker = self.start();
        self.expect_and_bump(LParen);
        let rollback_when_dropped = self.roll_back_context_after_drop();
        // restricts ']', '}' (recovers them by erroring), expects and allows ')'
        self.impose_restrictions_of(syntax);
        _ = self.parse_expression_until_binding_power(starting_precedence());
        self.expect_and_bump(RParen);
        let finished = self.complete_marker_with(marker, SyntaxKind::ParenExpr);
        self.clean_buffer();
        Some(finished)
    }

    #[allow(unused_variables)] // for rollback anchor
    pub fn parse_block(&self) -> Option<Finished> {
        let marker = self.start();
        self.expect_and_bump(LBrace);

        // TODO: put in a function impose_block_restrictions or inject_block_context
        {
            let rollback_when_dropped = self.roll_back_context_after_drop();
            let ctx = self.context.borrow();
            ctx.forbid(RBrace);
            ctx.disallow_recovery_of(RBrace);
            while !self.is_next(RBrace) && self.parse_statement().is_some() {}
        }

        self.expect_and_bump(RBrace);
        Some(self.complete_marker_with(marker, Block))
    }

    fn parse_prefix_unary_operation(&self, kind: SyntaxKind) -> Option<Finished> {
        let prefix_unary_op = AssocUnOp::from_syntax_kind(kind)?;
        let marker = self.start();
        self.expect_and_bump(kind);
        let bounded_precedence = Bound::from_op(prefix_unary_op.clone());
        _ = self.parse_expression_until_binding_power(bounded_precedence);
        Some(self.complete_marker_with(marker, prefix_unary_op.into()))
    }

    fn parse_postfix_unary_operation(
        &self,
        kind: SyntaxKind,
        min_precedence: &Bound,
        marker: &Marker<Complete>,
    ) -> Option<Finished> {
        let postfix_unary_op = AssocUnOp::from_syntax_kind(kind)?;
        let precedence = postfix_unary_op.precedence();
        // this should never be the case since it has the highest precedence among operators
        if min_precedence.gt(&precedence) {
            unreachable!();
        }
        let marker = self.precede_marker_with(marker);
        self.expect_and_bump(kind);
        Some(self.complete_marker_with(marker, postfix_unary_op.into()))
    }

    #[allow(unused_variables)] // for rollback anchor
    fn parse_binary_operation(
        &self,
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
        let rollback_when_dropped = self.roll_back_context_after_drop();
        self.impose_restrictions_of(syntax);
        self.parse_expression_until_binding_power(bounded_precedence);
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
            expect![[r#"
                Root@0..16
                  InfixBinOp@0..16
                    VarRef@0..9
                      Ident@0..9 "structure"
                    Dot@9..10 "."
                    VarRef@10..16
                      Ident@10..16 "member""#]]),
        binary_dot_member_with_semicolon: ("structure.member;",
            expect![[r#"
                Root@0..16
                  InfixBinOp@0..16
                    VarRef@0..9
                      Ident@0..9 "structure"
                    Dot@9..10 "."
                    VarRef@10..16
                      Ident@10..16 "member""#]]),

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
            expect![[r#"
                Root@0..2
                  PrefixUnaryOp@0..2
                    Excl@0..1 "!"
                    VarRef@1..2
                      Ident@1..2 "a""#]]
        ),

        prefix_not_var_ref_with_semicolon: ("!a;",
            expect![[r#"
                Root@0..2
                  PrefixUnaryOp@0..2
                    Excl@0..1 "!"
                    VarRef@1..2
                      Ident@1..2 "a""#]]
        ),
        postfix_excl_prefix_neg_precedence: ("-9?",
            expect![[r#"
                Root@0..3
                  PrefixUnaryOp@0..3
                    Minus@0..1 "-"
                    PostFixUnaryOp@1..3
                      Literal@1..2
                        Int@1..2 "9"
                      QMark@2..3 "?""#]]
        ),
        postfix_excl_prefix_neg_precedence_with_semicolon: ("-9?;",
            expect![[r#"
                Root@0..3
                  PrefixUnaryOp@0..3
                    Minus@0..1 "-"
                    PostFixUnaryOp@1..3
                      Literal@1..2
                        Int@1..2 "9"
                      QMark@2..3 "?""#]]
        ),
        postfix_excl_infix_mul_precedence: ("1*9?",
            expect![[r#"
                Root@0..4
                  InfixBinOp@0..4
                    Literal@0..1
                      Int@0..1 "1"
                    Star@1..2 "*"
                    PostFixUnaryOp@2..4
                      Literal@2..3
                        Int@2..3 "9"
                      QMark@3..4 "?""#]]
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
        parenthesised_sub_precedes_mul: ("(3+1)*4",
            expect![[
                "Root@0..7\n  InfixBinOp@0..7\n    ParenExpr@0..5\n      LParen@0..1 \"(\"\n      InfixBinOp@1..4\n        Literal@1..2\n          Int@1..2 \"3\"\n        Plus@2..3 \"+\"\n        Literal@3..4\n          Int@3..4 \"1\"\n      RParen@4..5 \")\"\n    Star@5..6 \"*\"\n    Literal@6..7\n      Int@6..7 \"4\""
            ]]
        ),

        block_with_returning: ("{let a = 0; a}",
            expect![[r#"
                Root@0..14
                  Block@0..14
                    LBrace@0..1 "{"
                    VarDef@1..11
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
                      Semi@10..11 ";"
                    Whitespace@11..12 " "
                    VarRef@12..13
                      Ident@12..13 "a"
                    RBrace@13..14 "}""#]],
        ),

        block_with_multiple_statements_and_expressions: ("{let a = 0;\n let b = 1; let c= 2; a+b+c}",
            expect![[r#"
                Root@0..40
                  Block@0..40
                    LBrace@0..1 "{"
                    VarDef@1..11
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
                      Semi@10..11 ";"
                    Whitespace@11..12 "\n"
                    Whitespace@12..13 " "
                    VarDef@13..23
                      KwLet@13..16 "let"
                      Whitespace@16..17 " "
                      InfixBinOp@17..22
                        VarRef@17..18
                          Ident@17..18 "b"
                        Whitespace@18..19 " "
                        Eq@19..20 "="
                        Whitespace@20..21 " "
                        Literal@21..22
                          Int@21..22 "1"
                      Semi@22..23 ";"
                    Whitespace@23..24 " "
                    VarDef@24..33
                      KwLet@24..27 "let"
                      Whitespace@27..28 " "
                      InfixBinOp@28..32
                        VarRef@28..29
                          Ident@28..29 "c"
                        Eq@29..30 "="
                        Whitespace@30..31 " "
                        Literal@31..32
                          Int@31..32 "2"
                      Semi@32..33 ";"
                    Whitespace@33..34 " "
                    InfixBinOp@34..39
                      InfixBinOp@34..37
                        VarRef@34..35
                          Ident@34..35 "a"
                        Plus@35..36 "+"
                        VarRef@36..37
                          Ident@36..37 "b"
                      Plus@37..38 "+"
                      VarRef@38..39
                        Ident@38..39 "c"
                    RBrace@39..40 "}""#]],
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
                  FnDef@0..13
                    KwFn@0..2 "fn"
                    Whitespace@2..3 " "
                    Ident@3..8 "empty"
                    LParen@8..9 "("
                    RParen@9..10 ")"
                    Block@10..13
                      Whitespace@10..11 " "
                      LBrace@11..12 "{"
                      RBrace@12..13 "}""#]],
        ),

        function_def_with_single_parameter: ("fn empty(single:i32) {}",
            expect![[r#"
                Root@0..23
                  FnDef@0..23
                    KwFn@0..2 "fn"
                    Whitespace@2..3 " "
                    Ident@3..8 "empty"
                    LParen@8..9 "("
                    ParamDecl@9..19
                      Ident@9..15 "single"
                      Colon@15..16 ":"
                      TyI32@16..19 "i32"
                    RParen@19..20 ")"
                    Block@20..23
                      Whitespace@20..21 " "
                      LBrace@21..22 "{"
                      RBrace@22..23 "}""#]],
        ),

        function_def_with_multiple_parameters: ("fn empty(first:i32, second:char, third: Structure) {}",
            expect![[r#"
                Root@0..51
                  FnDef@0..51
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
                    Block@48..51
                      Whitespace@48..49 " "
                      LBrace@49..50 "{"
                      RBrace@50..51 "}""#]],
        ),

        function_def_with_multiple_parameters_with_actual_body: ("fn empty(first:i32, second:char) -> bool {let sum_1 = first + second; let sum_2 = second+first; first == check}",
            expect![[r#"
                Root@0..110
                  FnDef@0..110
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
                    RetType@31..39
                      Whitespace@31..32 " "
                      RArrow@32..34 "->"
                      Whitespace@34..35 " "
                      TyBool@35..39 "bool"
                    Block@39..110
                      Whitespace@39..40 " "
                      LBrace@40..41 "{"
                      VarDef@41..68
                        KwLet@41..44 "let"
                        Whitespace@44..45 " "
                        InfixBinOp@45..67
                          VarRef@45..50
                            Ident@45..50 "sum_1"
                          Whitespace@50..51 " "
                          Eq@51..52 "="
                          Whitespace@52..53 " "
                          InfixBinOp@53..67
                            VarRef@53..58
                              Ident@53..58 "first"
                            Whitespace@58..59 " "
                            Plus@59..60 "+"
                            Whitespace@60..61 " "
                            VarRef@61..67
                              Ident@61..67 "second"
                        Semi@67..68 ";"
                      Whitespace@68..69 " "
                      VarDef@69..94
                        KwLet@69..72 "let"
                        Whitespace@72..73 " "
                        InfixBinOp@73..93
                          VarRef@73..78
                            Ident@73..78 "sum_2"
                          Whitespace@78..79 " "
                          Eq@79..80 "="
                          Whitespace@80..81 " "
                          InfixBinOp@81..93
                            VarRef@81..87
                              Ident@81..87 "second"
                            Plus@87..88 "+"
                            VarRef@88..93
                              Ident@88..93 "first"
                        Semi@93..94 ";"
                      Whitespace@94..95 " "
                      InfixBinOp@95..109
                        VarRef@95..100
                          Ident@95..100 "first"
                        Whitespace@100..101 " "
                        EqEq@101..103 "=="
                        Whitespace@103..104 " "
                        VarRef@104..109
                          Ident@104..109 "check"
                      RBrace@109..110 "}""#]],
        ),

        function_def_with_return: ("fn sum(a:i32, b:i32) -> i32 { return a+b; }",
            expect![[r#"
                Root@0..42
                  FnDef@0..42
                    KwFn@0..2 "fn"
                    Whitespace@2..3 " "
                    Ident@3..6 "sum"
                    LParen@6..7 "("
                    ParamDecl@7..12
                      Ident@7..8 "a"
                      Colon@8..9 ":"
                      TyI32@9..12 "i32"
                    Whitespace@12..13 " "
                    ParamDecl@13..18
                      Ident@13..14 "b"
                      Colon@14..15 ":"
                      TyI32@15..18 "i32"
                    RParen@18..19 ")"
                    RetType@19..26
                      Whitespace@19..20 " "
                      RArrow@20..22 "->"
                      Whitespace@22..23 " "
                      TyI32@23..26 "i32"
                    Block@26..42
                      Whitespace@26..27 " "
                      LBrace@27..28 "{"
                      Whitespace@28..29 " "
                      Semi@29..40
                        Jump@29..39
                          KwReturn@29..35 "return"
                          Whitespace@35..36 " "
                          InfixBinOp@36..39
                            VarRef@36..37
                              Ident@36..37 "a"
                            Plus@37..38 "+"
                            VarRef@38..39
                              Ident@38..39 "b"
                        Semi@39..40 ";"
                      Whitespace@40..41 " "
                      RBrace@41..42 "}""#]],
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
                  ContainerRef@0..18
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

        for_loop_iterated: ("for elm in arr { print(elm) }",
            expect![[r#"
                Root@0..29
                  ForLoop@0..29
                    KwFor@0..3 "for"
                    Whitespace@3..4 " "
                    ForIdent@4..7
                      Ident@4..7 "elm"
                    Whitespace@7..8 " "
                    KwIn@8..10 "in"
                    Whitespace@10..11 " "
                    VarRef@11..14
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
                    ForIdent@4..7
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
                    ForIdent@6..9
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
                    ForIdent@4..6
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
                    Condition@5..24
                      Whitespace@5..6 " "
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
                    Condition@5..20
                      Whitespace@5..6 " "
                      InfixBinOp@6..20
                        InfixBinOp@6..16
                          VarRef@6..11
                            Ident@6..11 "human"
                          Dot@11..12 "."
                          VarRef@12..15
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
                    Condition@5..6
                      Whitespace@5..6 " "
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
                    Condition@2..9
                      Whitespace@2..3 " "
                      VarRef@3..8
                        Ident@3..8 "is_ok"
                      Whitespace@8..9 " "
                    Block@9..19
                      LBrace@9..10 "{"
                      Whitespace@10..11 " "
                      Semi@11..17
                        Jump@11..16
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
                    Condition@2..19
                      Whitespace@2..3 " "
                      InfixBinOp@3..19
                        InfixBinOp@3..13
                          VarRef@3..8
                            Ident@3..8 "human"
                          Dot@8..9 "."
                          VarRef@9..12
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
                      Semi@21..27
                        Jump@21..26
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
                      VarRef@4..12
                        Ident@4..12 "can_pass"
                      Whitespace@12..13 " "
                      Eq@13..14 "="
                      Whitespace@14..15 " "
                      ControlFlow@15..57
                        KwIf@15..17 "if"
                        Condition@17..34
                          Whitespace@17..18 " "
                          InfixBinOp@18..34
                            InfixBinOp@18..28
                              VarRef@18..23
                                Ident@18..23 "human"
                              Dot@23..24 "."
                              VarRef@24..27
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
                    Condition@2..16
                      Whitespace@2..3 " "
                      InfixBinOp@3..16
                        VarRef@3..8
                          Ident@3..8 "color"
                        Whitespace@8..9 " "
                        EqEq@9..11 "=="
                        Whitespace@11..12 " "
                        VarRef@12..15
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
                    Condition@31..46
                      Whitespace@31..32 " "
                      InfixBinOp@32..46
                        VarRef@32..37
                          Ident@32..37 "color"
                        EqEq@37..39 "=="
                        VarRef@39..45
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

        basic_struct: ("struct Point { x: i32, y: i32, item: Item }",
            expect![[r#"
                Root@0..41
                  StructDef@0..41
                    KwStruct@0..6 "struct"
                    Whitespace@6..7 " "
                    Ident@7..12 "Point"
                    Whitespace@12..13 " "
                    LBrace@13..14 "{"
                    StructAttrs@14..40
                      Whitespace@14..15 " "
                      StructAttr@15..21
                        Ident@15..16 "x"
                        Colon@16..17 ":"
                        Whitespace@17..18 " "
                        TyI32@18..21 "i32"
                      Whitespace@21..22 " "
                      StructAttr@22..28
                        Ident@22..23 "y"
                        Colon@23..24 ":"
                        Whitespace@24..25 " "
                        TyI32@25..28 "i32"
                      Whitespace@28..29 " "
                      StructAttr@29..39
                        Ident@29..33 "item"
                        Colon@33..34 ":"
                        Whitespace@34..35 " "
                        Ident@35..39 "Item"
                      Whitespace@39..40 " "
                    RBrace@40..41 "}""#]],
        ),
    }

    #[test]
    fn several_valid_comma_separated_parameter_declarations() {
        use SyntaxKind::*;
        let params = "me:human, lang: Language, pet: Cat)";
        let parser = Parser::new(params);
        let root = parser.start();
        parser.parse_comma_separated_typed_declarations_until(|syntax: Syntax| {
            !syntax.is_of_kind(RParen)
        });
        parser.complete_marker_with(root, Root);
        let lexer = parser.lexer.borrow();
        let sink = Sink::new(parser.event_holder.take().into(), lexer.source());
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
