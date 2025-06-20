use syntax::{Syntax, bitset::SyntaxKindBitSet, syntax_kind::SyntaxKind};
use thin_vec::ThinVec;

use crate::{
    cst::ConcreteSyntaxTree,
    marker::{Complete, Incomplete, Marker},
    operator::{AssocBinOp, AssocUnOp, Bound, Op, starting_precedence},
    parser::{IsNext, Parser},
    sink::Sink,
};
pub type Finished = Marker<Complete>;
pub type Started = Marker<Incomplete>;

use SyntaxKind::*;

pub type CustomExpectationOnSyntax = fn(&Syntax) -> bool;

#[derive(Clone, Debug)]
pub enum Element {
    Branched(ThinVec<Element>, ThinVec<Element>),
    Fn(CustomExpectationOnSyntax),
    InSet(SyntaxKindBitSet),
    LeftHandSide,
    Kind(SyntaxKind),
    KindAs(SyntaxKind, SyntaxKind),
    KindWithMarker(SyntaxKind, SyntaxKind),
    Optional(SyntaxKindBitSet),
    ParseExprWith(Bound),
}

impl Parser<'_> {
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

    pub fn bump_separated_by(
        &self,
        elements_in_order: &[Element],
        separator: SyntaxKind,
        unwanted: impl Into<SyntaxKindBitSet>,
    ) {
        let unwanted: SyntaxKindBitSet = unwanted.into();
        while let IsNext::No = self.is_next_in_strict(unwanted) {
            self.parse_with(elements_in_order);
            self.bump_if(separator);
        }
    }
    pub fn parse_separated_by(
        &self,
        elements_in_order: &[Element],
        wrapping_kind_to_complete: SyntaxKind,
        separator: SyntaxKind,
        unwanted: impl Into<SyntaxKindBitSet>,
    ) {
        let unwanted: SyntaxKindBitSet = unwanted.into();
        while let IsNext::No = self.is_next_in_strict(unwanted) {
            let marker = self.start();

            self.parse_with(elements_in_order);
            self.bump_if(separator);

            self.complete_marker_with(marker, wrapping_kind_to_complete);
        }
    }

    fn does_first_element_pass(&self, s: &Element) -> bool {
        use Element::*;
        match s {
            Branched(f, _) => self.does_first_element_pass(f.first().unwrap()),
            &Fn(f) => self.is_next_f(f),
            &InSet(syntax_kind_bit_set) => self.is_next_in(syntax_kind_bit_set),
            &Kind(syntax_kind) => self.is_next(syntax_kind),
            &KindAs(syntax_kind, _) => self.is_next(syntax_kind),
            &KindWithMarker(syntax_kind, _) => self.is_next(syntax_kind),
            &LeftHandSide => true,
            Optional(_) => true,
            ParseExprWith(_) => true,
        }
    }
    pub fn parse_with(&self, elements_in_order: &[Element]) {
        use Element::*;
        for element in elements_in_order.iter() {
            match element {
                Branched(a, b) => {
                    let to_iterate = if self.does_first_element_pass(a.first().unwrap()) {
                        a
                    } else {
                        b
                    };
                    self.parse_with(to_iterate);
                }
                &Fn(custom_exp_func) => {
                    self.expect_f_and_bump(custom_exp_func);
                }
                &InSet(set) => {
                    if self.is_next_in(set) {
                        self.bump();
                    }
                }
                &Kind(exp_kind) => {
                    self.expect_and_bump(exp_kind);
                }
                &KindAs(exp_kind, as_kind) => {
                    self.expect_and_bump_as(exp_kind, as_kind);
                }
                &KindWithMarker(exp_kind, bump_with) => {
                    self.expect_and_bump_with_marker(exp_kind, bump_with);
                }
                LeftHandSide => {
                    self.parse_left_hand_side();
                }
                &Optional(set) => {
                    if self.is_next_in(set) {
                        self.bump();
                    }
                }
                ParseExprWith(bound) => {
                    self.parse_expression_until_binding_power(bound.clone());
                }
            }
        }
    }

    #[allow(unused_variables)]
    pub fn parse_condition(&self) -> Option<Finished> {
        let marker = if self.is_next_f(|syntax| matches!(syntax.get_kind(), KwFalse | KwTrue)) {
            self.bump_with_marker(Condition)
        } else {
            let cond_marker = self.start();
            let rollback_when_dropped = self.impose_context_for_parsing(SyntaxKind::Condition);
            self.parse_expression_until_binding_power(starting_precedence());
            self.complete_marker_with(cond_marker, Condition)
        };
        Some(marker)
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
                    if !kind.is_operator() {
                        break;
                    }
                    if !self.is_allowed(kind) && !self.is_expected(kind) {
                        // note: returns None thus shortcircuits from here
                        self.recover_restricted(kind)?;
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
                            if kind.is_assignment()
                                && !self.is_in_the_middle_of_parsing(LetBinding)
                                && self.is_next(Semi)
                            {
                                let semi_marker = self.precede_marker_with(&lhs_marker);
                                self.expect_and_bump(Semi);
                                lhs_marker = self.complete_marker_with(semi_marker, Semi);
                                break;
                            }
                            continue;
                        }
                    }
                    break;
                }
            }
        }
        Some(lhs_marker)
    }

    #[allow(unused_variables)]
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

        match kind {
            Ident => self.parse_starting_with_identifier(),
            Int | Float | StrLit | CharLit | KwTrue | KwFalse => self.parse_literal(kind),
            LBrack => self.parse_buffer_literal(),
            // & (ref), ! (bool negate), - (negative), * (deref)
            And | Excl | Minus | Star => self.parse_prefix_unary_operation(kind),
            // Note: OrOr is a special case for a lambda definition with no parameters, and since OrOr is a binary infix operator,
            // we don't expect it to be in left-hand side expression, thus attempt to parse it as a lambda definition
            Or | OrOr => self.parse_lambda_def(),
            delimiter if delimiter.is_delimiter() => self.parse_delimited(syntax),
            container if matches!(container, KwBuffer | KwTensor) => self.parse_type(&syntax),
            typing if typing.is_type() => {
                let rollback_when_dropped = self.by_expecting(StructAsType);
                self.parse_type(&syntax)
            }
            keyword if keyword.is_keyword() => self.parse_keyword_expression(syntax),
            operator if operator.is_operator() => {
                self.recover_restricted(operator);
                None
            }
            _ => None,
        }
    }

    #[allow(unused_variables)]
    pub fn parse_type(&self, syntax: &Syntax) -> Option<Finished> {
        let kind = syntax.get_kind();
        if !self.is_allowed(kind) && !self.is_expected(kind) {
            // it returns None thus acts as an early return
            self.recover_restricted(kind)?;
        }
        match kind {
            KwTensor | KwBuffer => return self.parse_container_constructs(syntax),
            // note: a lambda is typed as a function when hinted or as a parameter as in Rust
            KwFn => return self.parse_function_as_type(),
            reference if reference == And => {
                self.parse_left_hand_side();
            }
            Ident => {
                self.parse_starting_with_identifier();
            }
            LParen => {
                self.parse_parenthesised();
            }
            t if t.is_type() => {
                self.bump();
            }
            _ => {}
        }
        // should not continue with expression parsing, because
        // a type cannot be an operand.
        None
    }

    // Possible options: a variable reference, function/method call or an iterable indexing
    #[allow(unused_variables)]
    pub fn parse_starting_with_identifier(&self) -> Option<Finished> {
        if self.is_expected(StructAsType) {
            self.expect_and_bump_as(Ident, StructAsType);
            return None;
        }

        let marker = self.start();
        self.expect_and_bump(Ident);

        let finished = if self.is_next(LParen) {
            self.parse_call(marker)
        } else if self.is_next(LBrace) && self.is_allowed(StructLit) {
            self.parse_struct_init_block();
            self.complete_marker_with(marker, StructLit)
        } else {
            self.complete_marker_with(marker, VarRef)
        };

        Some(finished)
    }

    pub fn parse_literal(&self, kind: SyntaxKind) -> Option<Finished> {
        self.expect_and_bump_with_marker(kind, Literal)
    }

    #[allow(unused_variables)]
    fn parse_break(&self) -> Option<Finished> {
        let marker = self.start();
        self.expect_and_bump(KwBreak);

        let rollback_after_drop = self.impose_context_for_parsing(Jump);

        self.parse_expression_until_binding_power(starting_precedence());
        self.expect_and_bump(Semi);

        Some(self.complete_marker_with(marker, Jump))
    }

    #[allow(unused_variables)]
    pub fn parse_return(&self) -> Option<Finished> {
        let marker = self.start();
        self.expect_and_bump(KwReturn);

        let rollback_after_drop = self.impose_context_for_parsing(Return);
        self.parse_expression_until_binding_power(starting_precedence());
        self.expect_and_bump(Semi);

        Some(self.complete_marker_with(marker, Return))
    }

    fn parse_continue(&self) -> Option<Finished> {
        let marker = self.start();
        self.expect_and_bump(KwContinue);
        self.expect_and_bump(Semi);
        Some(self.complete_marker_with(marker, Jump))
    }

    #[allow(unused_variables)]
    pub fn parse_mut(&self) -> Option<Finished> {
        let marker = self.start();
        self.expect_and_bump(KwMut);
        // * Note: either an identifier for binding, or a type declaration (&mut <type>)
        let rollback_when_dropped = self.impose_context_for_parsing(Mut);
        // * We are in the rhs of the : operator
        if self.is_expected(TypeHint) {
            if self.is_next(Ident) {
                self.expect_in_ctx(StructAsType);
                self.parse_starting_with_identifier();
            } else {
                self.parse_type(&self.peek().unwrap().unwrap());
            }
        // * We are in the lhs of the identifier, thus expect to parse an identifier
        } else {
            self.parse_left_hand_side();
        }
        Some(self.complete_marker_with(marker, Mut))
    }
    #[allow(unused_variables)]
    pub fn parse_keyword_expression(&self, keyword: Syntax) -> Option<Finished> {
        match keyword.get_kind() {
            KwBreak => self.parse_break(),
            KwContinue => self.parse_continue(),
            KwFn => self.parse_function_as_type(), // trying to parse it as a type
            // A variable definition is a statement, thus will be recovered
            KwIf => self.parse_conditionals(),
            KwLet => {
                self.recover_restricted(KwLet);
                None
            }
            KwMut if self.is_allowed(KwMut) => self.parse_mut(),
            KwReturn => self.parse_return(),
            KwStruct => self.parse_struct_definition(),
            KwSelf | Kwself => Some(self.bump_with_marker(SelfRef)),
            nope => {
                println!("{:?} is not implemented yet", nope);
                todo!()
            }
        }
    }

    #[allow(unused_variables)]
    pub fn parse_prefix_unary_operation(&self, kind: SyntaxKind) -> Option<Finished> {
        let prefix_unary_op = AssocUnOp::from_syntax_kind(kind)?;
        let marker = self.start();
        let rollback_after_drop = self.impose_context_for_parsing(PrefixUnaryOp);
        self.expect_and_bump(kind);
        let bounded_precedence = Bound::from_op(prefix_unary_op.clone());
        _ = self.parse_expression_until_binding_power(bounded_precedence);
        Some(self.complete_marker_with(marker, prefix_unary_op.into()))
    }

    #[allow(unused_variables)]
    pub fn parse_postfix_unary_operation(
        &self,
        kind: SyntaxKind,
        min_precedence: &Bound,
        marker: &Marker<Complete>,
    ) -> Option<Finished> {
        let postfix_unary_op = AssocUnOp::from_syntax_kind(kind)?;
        let precedence = postfix_unary_op.precedence();
        if min_precedence.gt(&precedence) {
            return None;
        }
        let marker = self.precede_marker_with(marker);
        self.expect_and_bump(kind);
        if kind == LBrack {
            let rollback_when_dropped = self.impose_restrictions_of_kind_on_context(kind);
            self.parse_expression_until_binding_power(starting_precedence());
            self.expect_and_bump(RBrack);
        }
        Some(self.complete_marker_with(marker, postfix_unary_op.into()))
    }

    #[allow(unused_variables)] // for rollback anchor
    pub fn parse_binary_operation(
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
        let rollback_when_dropped = self.impose_restrictions_of_kind_on_context(syntax.get_kind());
        self.parse_expression_until_binding_power(bounded_precedence);
        Some(self.complete_marker_with(marker, binary_op.into()))
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use expect_test::{Expect, expect};
    use parameterized_test::create;

    pub(crate) fn check(prog: &str, expect: Expect) {
        let parse = Parser::new(prog).parse();
        let debug_tree = parse.debug_tree();
        expect.assert_eq(&debug_tree);
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
        identifier_with_semicolon: ("ident;", expect![[r#"
            Root@0..6
              Semi@0..6
                VarRef@0..5
                  Ident@0..5 "ident"
                Semi@5..6 ";""#]]),
        ignored_whitespaces: ("   ", expect![[r#"
            Root@0..3
              Whitespace@0..3 "   ""#]]),
        binary_add_two_numbers: ("3+14",
            expect![[
            "Root@0..4\n  InfixBinOp@0..4\n    Literal@0..1\n      Int@0..1 \"3\"\n    Plus@1..2 \"+\"\n    Literal@2..4\n      Int@2..4 \"14\""
            ]]),
        binary_add_two_numbers_with_semicolon: ("3+14;",
            expect![[r#"
                Root@0..5
                  Semi@0..5
                    InfixBinOp@0..4
                      Literal@0..1
                        Int@0..1 "3"
                      Plus@1..2 "+"
                      Literal@2..4
                        Int@2..4 "14"
                    Semi@4..5 ";""#]]),
        redundant_semi: ("x+2;;",
            expect![[r#"
                Root@0..4
                  Semi@0..4
                    InfixBinOp@0..3
                      VarRef@0..1
                        Ident@0..1 "x"
                      Plus@1..2 "+"
                      Literal@2..3
                        Int@2..3 "2"
                    Semi@3..4 ";""#]]
        ),
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
                Root@0..17
                  Semi@0..17
                    InfixBinOp@0..16
                      VarRef@0..9
                        Ident@0..9 "structure"
                      Dot@9..10 "."
                      VarRef@10..16
                        Ident@10..16 "member"
                    Semi@16..17 ";""#]]),

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
                Root@0..22
                  Semi@0..22
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
                          Int@18..21 "100"
                    Semi@21..22 ";""#]]),

        binary_add_four_numbers: ("3+14+159+2653",
            expect![[
            "Root@0..13\n  InfixBinOp@0..13\n    InfixBinOp@0..8\n      InfixBinOp@0..4\n        Literal@0..1\n          Int@0..1 \"3\"\n        Plus@1..2 \"+\"\n        Literal@2..4\n          Int@2..4 \"14\"\n      Plus@4..5 \"+\"\n      Literal@5..8\n        Int@5..8 \"159\"\n    Plus@8..9 \"+\"\n    Literal@9..13\n      Int@9..13 \"2653\""
            ]]
        ),
        binary_add_four_numbers_with_semicolon: ("3+14*159-2653;",
            expect![[r#"
                Root@0..14
                  Semi@0..14
                    InfixBinOp@0..13
                      InfixBinOp@0..8
                        Literal@0..1
                          Int@0..1 "3"
                        Plus@1..2 "+"
                        InfixBinOp@2..8
                          Literal@2..4
                            Int@2..4 "14"
                          Star@4..5 "*"
                          Literal@5..8
                            Int@5..8 "159"
                      Minus@8..9 "-"
                      Literal@9..13
                        Int@9..13 "2653"
                    Semi@13..14 ";""#]]
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
            expect![[r#"
                Root@0..3
                  Semi@0..3
                    PrefixUnaryOp@0..2
                      Minus@0..1 "-"
                      Literal@1..2
                        Int@1..2 "9"
                    Semi@2..3 ";""#]]
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
                Root@0..3
                  Semi@0..3
                    PrefixUnaryOp@0..2
                      Excl@0..1 "!"
                      VarRef@1..2
                        Ident@1..2 "a"
                    Semi@2..3 ";""#]]
        ),
        postfix_qmark_prefix_neg_precedence: ("-9?",
            expect![[r#"
                Root@0..3
                  PrefixUnaryOp@0..3
                    Minus@0..1 "-"
                    PostfixUnaryOp@1..3
                      Literal@1..2
                        Int@1..2 "9"
                      QMark@2..3 "?""#]]
        ),
        postfix_qmark_prefix_neg_precedence_with_semicolon: ("-9?;",
            expect![[r#"
                Root@0..4
                  Semi@0..4
                    PrefixUnaryOp@0..3
                      Minus@0..1 "-"
                      PostfixUnaryOp@1..3
                        Literal@1..2
                          Int@1..2 "9"
                        QMark@2..3 "?"
                    Semi@3..4 ";""#]]
        ),
        postfix_qmark_prefix_and_precedence_with_semicolon: ("&9?;",
            expect![[r#"
                Root@0..4
                  Semi@0..4
                    PrefixUnaryOp@0..3
                      And@0..1 "&"
                      PostfixUnaryOp@1..3
                        Literal@1..2
                          Int@1..2 "9"
                        QMark@2..3 "?"
                    Semi@3..4 ";""#]]
        ),
        postfix_qmark_infix_mul_precedence: ("1*9?",
            expect![[r#"
                Root@0..4
                  InfixBinOp@0..4
                    Literal@0..1
                      Int@0..1 "1"
                    Star@1..2 "*"
                    PostfixUnaryOp@2..4
                      Literal@2..3
                        Int@2..3 "9"
                      QMark@3..4 "?""#]]
        ),
        prefix_minus_sum_precedence: ("-3+14",
            expect![[
                "Root@0..5\n  InfixBinOp@0..5\n    PrefixUnaryOp@0..2\n      Minus@0..1 \"-\"\n      Literal@1..2\n        Int@1..2 \"3\"\n    Plus@2..3 \"+\"\n    Literal@3..5\n      Int@3..5 \"14\""
            ]]
        ),

    }
}
