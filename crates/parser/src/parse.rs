use lexer::token_type::TokenType;
use syntax::{Syntax, is_a_type, is_an_assignment, is_an_operator, syntax_kind::SyntaxKind};
use thin_vec::ThinVec;

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

pub type CustomExpectationOnSyntax = fn(Syntax) -> bool;

#[derive(Debug)]
pub enum SeparatedElement {
    Kind(SyntaxKind),
    Fn(CustomExpectationOnSyntax),
    Branched(ThinVec<SeparatedElement>, ThinVec<SeparatedElement>),
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
        elements_in_order: &ThinVec<SeparatedElement>,
        wrapping_kind_to_complete: SyntaxKind,
        separator: SyntaxKind,
        until_false: fn(Syntax) -> bool,
    ) -> Option<()> {
        while until_false(self.peek()?.ok()?) {
            let marker = self.start();

            // println!("parse with before: {:?}", self.peek());
            self.parse_with(&elements_in_order);
            // println!("parse with after: {:?}", self.peek());

            self.ignore_if(separator);
            self.complete_marker_with(marker, wrapping_kind_to_complete);
        }
        Some(())
    }

    fn does_first_element_pass(&self, s: &SeparatedElement) -> bool {
        use SeparatedElement::*;
        match s {
            Kind(syntax_kind) => self.is_next(syntax_kind.clone()),
            Fn(f) => self.is_next_f(f.clone()),
            ParseExprWith(_) => unimplemented!(),
            Branched(_, _) => unimplemented!(),
        }
    }
    fn parse_with(&self, elements_in_order: &ThinVec<SeparatedElement>) {
        use SeparatedElement::*;
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
                Branched(a, b) => {
                    let to_iterate = if self.does_first_element_pass(a.get(0).unwrap()) {
                        a
                    } else if self.does_first_element_pass(b.get(0).unwrap()) {
                        b
                    } else {
                        continue;
                    };
                    println!(" will be using : {:?}", &to_iterate);
                    self.parse_with(to_iterate);
                }
            }
        }
    }

    pub fn parse_condition(&self) -> Option<Finished> {
        let marker = if self.is_next_f(|syntax| matches!(syntax.get_kind(), KwFalse | KwTrue)) {
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
    fn parse_variable_def(&self) -> Option<Finished> {
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
                            if is_an_assignment(&kind) && !self.context.borrow().is_expected(VarDef)
                            {
                                if self.is_next(Semi) {
                                    let semi_marker = self.precede_marker_with(&lhs_marker);
                                    self.expect_and_bump(Semi);
                                    lhs_marker = self.complete_marker_with(semi_marker, Semi);
                                    break;
                                }
                            }
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

    pub fn parse_left_hand_side(&self) -> Option<Finished> {
        use SyntaxKind::*;
        let syntax = match self.peek()? {
            Ok(syntax) => syntax,
            Err(err) => {
                self.recover_from_err(err);
                return None;
            }
        };
        let kind = syntax.get_kind();

        let marker = match kind {
            Ident => self.parse_starting_with_identifier(),
            Int | Float | StrLit | CharLit | KwTrue | KwFalse => self.parse_literal(kind),
            Minus | Excl => self.parse_prefix_unary_operation(kind),
            keyword if keyword.is_keyword() => self.parse_keyword_expression(syntax),
            delimiter if delimiter.is_delimiter() => self.parse_delimited(syntax),
            typing if is_a_type(&typing) => {
                self.bump();
                // should not continue with expression parsing, because
                // a type cannot be an operand.
                None
            }
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
            KwSelf | Kwself => Some(self.bump_with_marker(SelfRef)),
            KwMut => {
                // TODO: mut should not be here, it should wrap what's coming into its own node
                todo!()
            }
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

    pub fn parse_prefix_unary_operation(&self, kind: SyntaxKind) -> Option<Finished> {
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
pub(crate) mod tests {
    use crate::errors::ParseError;

    use super::*;
    use expect_test::{Expect, expect};
    use miette::Report;
    use parameterized_test::create;
    use std::ops::Range;
    use thin_vec::{ThinVec, thin_vec};

    pub(crate) fn check(prog: &str, expect: Expect) {
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
        ignored_whitespaces: ("   ", expect![[r#"
            Root@0..3
              Whitespace@0..3 "   ""#]]),
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
                      VarRef@6..13
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
                      VarRef@6..13
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
                        VarRef@5..7
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
                        VarRef@5..7
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
                        VarRef@17..19
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

        block_debug: ("{ self.x += by.x; self.y += by.y;}",
            expect![[r#"
                Root@0..34
                  Block@0..34
                    LBrace@0..1 "{"
                    Whitespace@1..2 " "
                    Semi@2..17
                      InfixBinOp@2..16
                        InfixBinOp@2..9
                          SelfRef@2..6
                            Kwself@2..6 "self"
                          Dot@6..7 "."
                          VarRef@7..9
                            Ident@7..8 "x"
                            Whitespace@8..9 " "
                        PlusEq@9..11 "+="
                        Whitespace@11..12 " "
                        InfixBinOp@12..16
                          VarRef@12..14
                            Ident@12..14 "by"
                          Dot@14..15 "."
                          VarRef@15..16
                            Ident@15..16 "x"
                      Semi@16..17 ";"
                    Whitespace@17..18 " "
                    Semi@18..33
                      InfixBinOp@18..32
                        InfixBinOp@18..25
                          SelfRef@18..22
                            Kwself@18..22 "self"
                          Dot@22..23 "."
                          VarRef@23..25
                            Ident@23..24 "y"
                            Whitespace@24..25 " "
                        PlusEq@25..27 "+="
                        Whitespace@27..28 " "
                        InfixBinOp@28..32
                          VarRef@28..30
                            Ident@28..30 "by"
                          Dot@30..31 "."
                          VarRef@31..32
                            Ident@31..32 "y"
                      Semi@32..33 ";"
                    RBrace@33..34 "}""#]],
        ),
    }
}
