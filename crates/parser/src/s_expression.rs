use crate::{parse_behaviors::ASTBehavior, parser::Parser, statement::statement};
use syntax::syntax::SyntaxKind;

// Note: Into and From will produce different kinds since from Op to SyntaxKind,
// we transition to a composite SyntaxKind.
#[derive(Debug)]
pub(crate) enum OpType {
    Prefix(SyntaxKind),
    Infix(SyntaxKind),
    Postfix(SyntaxKind),
}
#[derive(Debug)]
pub(crate) enum Op {
    // prefix
    Neg,
    Not,
    // infix
    Add,
    Div,
    Dot,
    Eq,
    Mul,
    Sub,
    // postfix
    Factorial,
    None,
    End,
}

pub type BindingPower = u8;
pub const NO_FOR_POSTFIX: BindingPower = 0;
pub const NO_FOR_PREFIX: BindingPower = 99;
pub const STOPPER: BindingPower = 99;

impl Op {
    pub(crate) fn binding_power(&self) -> (BindingPower, BindingPower) {
        match self {
            // prefix
            Op::Neg => (NO_FOR_PREFIX, 9),
            Op::Not => (NO_FOR_PREFIX, 9),
            // like rust-analyzer, dummy token has a binding power pair
            Op::None => (0, 1),
            Op::End => (0, STOPPER),
            // left associative
            Op::Eq => (2, 1),
            Op::Add | Op::Sub => (5, 6),
            Op::Mul | Op::Div => (7, 8),
            // right associative
            Op::Dot => (14, 13),
            // postfix
            Op::Factorial => (11, NO_FOR_POSTFIX),
        }
    }
    pub(crate) fn prefix_operation_from(syntax_kind: &SyntaxKind) -> Op {
        match syntax_kind {
            SyntaxKind::Minus => Op::Neg,
            SyntaxKind::Not => Op::Not,
            SyntaxKind::SemiColon => Op::End,
            _ => Op::None,
        }
    }

    pub(crate) fn infix_operation_from(syntax_kind: &SyntaxKind) -> Op {
        match syntax_kind {
            SyntaxKind::Dot => Op::Dot,
            SyntaxKind::Eq => Op::Eq,
            SyntaxKind::Minus => Op::Sub,
            SyntaxKind::Plus => Op::Add,
            SyntaxKind::SemiColon => Op::End,
            SyntaxKind::Slash => Op::Div,
            SyntaxKind::Star => Op::Mul,
            _ => Op::None,
        }
    }
    pub(crate) fn postfix_operation_from(syntax_kind: &SyntaxKind) -> Op {
        match syntax_kind {
            SyntaxKind::Exclamation => Op::Factorial,
            SyntaxKind::SemiColon => Op::End,
            _ => Op::None,
        }
    }
}

impl From<&OpType> for Op {
    fn from(optype: &OpType) -> Self {
        match optype {
            OpType::Prefix(syntax_kind) => Self::prefix_operation_from(syntax_kind),
            OpType::Infix(syntax_kind) => Self::infix_operation_from(syntax_kind),
            OpType::Postfix(syntax_kind) => Self::postfix_operation_from(syntax_kind),
        }
    }
}

// we don't want the 'free' into, we want to implement the conversion explicitly because it is one to many
#[allow(clippy::from_over_into)]
impl Into<SyntaxKind> for Op {
    fn into(self) -> SyntaxKind {
        match self {
            Op::Add | Op::Sub | Op::Mul | Op::Div | Op::Dot | Op::Eq => SyntaxKind::InfixBinaryOp,
            Op::Neg | Op::Not => SyntaxKind::PrefixUnaryOp,
            Op::Factorial => SyntaxKind::PostFixUnaryOp,
            Op::None => todo!(),
            Op::End => todo!(),
        }
    }
}

pub fn pratt_parser<B: ASTBehavior>(parser: &mut Parser) {
    let root = parser.start();
    while !parser.is_at_the_end() {
        statement::<B>(parser);
    }
    root.complete(&mut parser.event_holder, SyntaxKind::Root);
}

#[cfg(test)]
pub(super) mod tests {

    use super::*;
    use crate::parse_behaviors::{IgnoreTrivia, NonIgnoring};
    use expect_test::{Expect, expect};
    use parameterized_test::create;

    pub fn check<B: ASTBehavior>(prog: &str, expect: Expect) {
        let parse = Parser::new(prog).parse::<B>();
        let debug_tree = parse.debug_tree();
        expect.assert_eq(&debug_tree);
    }

    create! {
        create_trivia_ignoring_parser_test,
        (prog, expect), {
            check::<IgnoreTrivia>(prog, expect);
        }
    }
    create! {
        create_non_ignoring_parser_test,
        (prog, expect), {
            check::<NonIgnoring>(prog, expect);
        }
    }

    create_trivia_ignoring_parser_test! {
        nothing: ("", expect![["Root@0..0"]]),
        single_digit: ("9", expect![["Root@0..1\n  Literal@0..1\n    Number@0..1 \"9\""]]),
        multiple_digit: ("314", expect![["Root@0..3\n  Literal@0..3\n    Number@0..3 \"314\""]]),
        identifier: ("ident", expect![["Root@0..5\n  VariableRef@0..5\n    Identifier@0..5 \"ident\""]]),
        identifier_with_semicolon: ("ident;", expect![["Root@0..5\n  VariableRef@0..5\n    Identifier@0..5 \"ident\""]]),
        ignored_whitespaces: ("   ", expect![["Root@0..0" ]]),
        binary_add_two_numbers: ("3+14",
            expect![[
            "Root@0..4\n  InfixBinaryOp@0..4\n    Literal@0..1\n      Number@0..1 \"3\"\n    Plus@1..2 \"+\"\n    Literal@2..4\n      Number@2..4 \"14\""
            ]]),
        binary_add_two_numbers_with_semicolon: ("3+14;",
            expect![[
            "Root@0..4\n  InfixBinaryOp@0..4\n    Literal@0..1\n      Number@0..1 \"3\"\n    Plus@1..2 \"+\"\n    Literal@2..4\n      Number@2..4 \"14\""
            ]]),

        binary_dot_member: ("structure.member",
            expect![[
r#"Root@0..16
  InfixBinaryOp@0..16
    VariableRef@0..9
      Identifier@0..9 "structure"
    Dot@9..10 "."
    VariableRef@10..16
      Identifier@10..16 "member""#
            ]]),
        binary_dot_member_with_semicolon: ("structure.member;",
            expect![[
r#"Root@0..16
  InfixBinaryOp@0..16
    VariableRef@0..9
      Identifier@0..9 "structure"
    Dot@9..10 "."
    VariableRef@10..16
      Identifier@10..16 "member""#
            ]]),

        binary_dot_member_precedence: ("human.weight + 1 /100",
            expect![[
r#"Root@0..18
  InfixBinaryOp@0..18
    InfixBinaryOp@0..12
      VariableRef@0..5
        Identifier@0..5 "human"
      Dot@5..6 "."
      VariableRef@6..12
        Identifier@6..12 "weight"
    Plus@12..13 "+"
    InfixBinaryOp@13..18
      Literal@13..14
        Number@13..14 "1"
      Slash@14..15 "/"
      Literal@15..18
        Number@15..18 "100""#
            ]]),
        binary_dot_member_precedence_with_semi_colon: ("human.weight + 1 /100;",
            expect![[
r#"Root@0..18
  InfixBinaryOp@0..18
    InfixBinaryOp@0..12
      VariableRef@0..5
        Identifier@0..5 "human"
      Dot@5..6 "."
      VariableRef@6..12
        Identifier@6..12 "weight"
    Plus@12..13 "+"
    InfixBinaryOp@13..18
      Literal@13..14
        Number@13..14 "1"
      Slash@14..15 "/"
      Literal@15..18
        Number@15..18 "100""#
            ]]),
        binary_add_four_numbers: ("3+14+159+2653",
            expect![[
            "Root@0..13\n  InfixBinaryOp@0..13\n    InfixBinaryOp@0..8\n      InfixBinaryOp@0..4\n        Literal@0..1\n          Number@0..1 \"3\"\n        Plus@1..2 \"+\"\n        Literal@2..4\n          Number@2..4 \"14\"\n      Plus@4..5 \"+\"\n      Literal@5..8\n        Number@5..8 \"159\"\n    Plus@8..9 \"+\"\n    Literal@9..13\n      Number@9..13 \"2653\""
            ]]
        ),
        binary_add_four_numbers_with_semicolon: ("3+14+159+2653;",
            expect![[
            "Root@0..13\n  InfixBinaryOp@0..13\n    InfixBinaryOp@0..8\n      InfixBinaryOp@0..4\n        Literal@0..1\n          Number@0..1 \"3\"\n        Plus@1..2 \"+\"\n        Literal@2..4\n          Number@2..4 \"14\"\n      Plus@4..5 \"+\"\n      Literal@5..8\n        Number@5..8 \"159\"\n    Plus@8..9 \"+\"\n    Literal@9..13\n      Number@9..13 \"2653\""
            ]]
        ),
        binary_add_mul_sub_four_numbers: ("3+14*159-2653",
            expect![[
            "Root@0..13\n  InfixBinaryOp@0..13\n    InfixBinaryOp@0..8\n      Literal@0..1\n        Number@0..1 \"3\"\n      Plus@1..2 \"+\"\n      InfixBinaryOp@2..8\n        Literal@2..4\n          Number@2..4 \"14\"\n        Star@4..5 \"*\"\n        Literal@5..8\n          Number@5..8 \"159\"\n    Minus@8..9 \"-\"\n    Literal@9..13\n      Number@9..13 \"2653\""
            ]]
        ),
        prefix_minus_digit: ("-9",
            expect![[
                "Root@0..2\n  PrefixUnaryOp@0..2\n    Minus@0..1 \"-\"\n    Literal@1..2\n      Number@1..2 \"9\""
            ]]
        ),
        prefix_minus_digit_with_semicolon: ("-9;",
            expect![[
                "Root@0..2\n  PrefixUnaryOp@0..2\n    Minus@0..1 \"-\"\n    Literal@1..2\n      Number@1..2 \"9\""
            ]]
        ),

        prefix_not_var_ref: ("~a",
            expect![[
r#"
Root@0..2
  PrefixUnaryOp@0..2
    Not@0..1 "~"
    VariableRef@1..2
      Identifier@1..2 "a""#
            ]]
        ),

        prefix_not_var_ref_with_semicolon: ("~a;",
            expect![[
r#"
Root@0..2
  PrefixUnaryOp@0..2
    Not@0..1 "~"
    VariableRef@1..2
      Identifier@1..2 "a""#
            ]]
        ),
        postfix_excl_prefix_neg_precedence: ("-9!",
            expect![[
r#"
Root@0..3
  PrefixUnaryOp@0..3
    Minus@0..1 "-"
    PostFixUnaryOp@1..3
      Literal@1..2
        Number@1..2 "9"
      Exclamation@2..3 "!""#
            ]]
        ),
        postfix_excl_prefix_neg_precedence_with_semicolon: ("-9!;",
            expect![[
r#"
Root@0..3
  PrefixUnaryOp@0..3
    Minus@0..1 "-"
    PostFixUnaryOp@1..3
      Literal@1..2
        Number@1..2 "9"
      Exclamation@2..3 "!""#
            ]]
        ),
        postfix_excl_infix_mul_precedence: ("1*9!",
            expect![[
r#"
Root@0..4
  InfixBinaryOp@0..4
    Literal@0..1
      Number@0..1 "1"
    Star@1..2 "*"
    PostFixUnaryOp@2..4
      Literal@2..3
        Number@2..3 "9"
      Exclamation@3..4 "!""#
            ]]
        ),
        prefix_minus_sum_precedence: ("-3+14",
            expect![[
                "Root@0..5\n  InfixBinaryOp@0..5\n    PrefixUnaryOp@0..2\n      Minus@0..1 \"-\"\n      Literal@1..2\n        Number@1..2 \"3\"\n    Plus@2..3 \"+\"\n    Literal@3..5\n      Number@3..5 \"14\""
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
                "Root@0..7\n  ParenExpr@0..7\n    LParen@0..1 \"(\"\n    ParenExpr@1..6\n      LParen@1..2 \"(\"\n      Literal@2..5\n        Number@2..5 \"314\"\n      RParen@5..6 \")\"\n    RParen@6..7 \")\""
            ]]
        ),
        parenthesised_pi_with_semicolon: ("((314));",
            expect![[
                "Root@0..7\n  ParenExpr@0..7\n    LParen@0..1 \"(\"\n    ParenExpr@1..6\n      LParen@1..2 \"(\"\n      Literal@2..5\n        Number@2..5 \"314\"\n      RParen@5..6 \")\"\n    RParen@6..7 \")\""
            ]]
        ),
        parenthesised_pi_with_semicolon_inside: ("((314;));",
            expect![[
                "Root@0..7\n  ParenExpr@0..7\n    LParen@0..1 \"(\"\n    ParenExpr@1..6\n      LParen@1..2 \"(\"\n      Literal@2..5\n        Number@2..5 \"314\"\n      RParen@5..6 \")\"\n    RParen@6..7 \")\""
            ]]
        ),
        parenthesised_sub_precedes_mul: ("(3+1)*4",
            expect![[
                "Root@0..7\n  InfixBinaryOp@0..7\n    ParenExpr@0..5\n      LParen@0..1 \"(\"\n      InfixBinaryOp@1..4\n        Literal@1..2\n          Number@1..2 \"3\"\n        Plus@2..3 \"+\"\n        Literal@3..4\n          Number@3..4 \"1\"\n      RParen@4..5 \")\"\n    Star@5..6 \"*\"\n    Literal@6..7\n      Number@6..7 \"4\""
            ]]
        ),
    }

    create_non_ignoring_parser_test! {
        nothing: ("", expect![["Root@0..0"]]),
        single_digit: ("9", expect![["Root@0..1\n  Literal@0..1\n    Number@0..1 \"9\""]]),
        multiple_digit: ("314", expect![["Root@0..3\n  Literal@0..3\n    Number@0..3 \"314\""]]),
        identifier: ("ident", expect![["Root@0..5\n  VariableRef@0..5\n    Identifier@0..5 \"ident\""]]),
        whitespaces: ("   ", expect![["Root@0..3\n  Whitespace@0..3 \"   \""]]),

    }
}
