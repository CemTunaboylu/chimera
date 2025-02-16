use crate::{
    lexer::*,
    marker::{Complete, Marker},
    parser::*,
};

use miette::Report;

// Note: Into and From will produce different kinds since from Op to SyntaxKind,
// we transition to a composite SyntaxKind.
enum OpType {
    Prefix(SyntaxKind),
    Infix(SyntaxKind),
    Postfix(SyntaxKind),
}
enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Neg,
    None,
}

pub type BindingPower = u8;
pub const NO: BindingPower = 0;

impl Op {
    fn binding_power(&self) -> (BindingPower, BindingPower) {
        match self {
            Op::Neg => (NO, 5),
            // left associative
            Op::Add | Op::Sub => (1, 2),
            Op::Mul | Op::Div => (3, 4),
            _ => (0, 0),
        }
    }
    fn prefix_operation_from(syntax_kind: SyntaxKind) -> Op {
        match syntax_kind {
            SyntaxKind::Minus => Op::Neg,
            _ => Op::None,
        }
    }

    fn infix_operation_from(syntax_kind: SyntaxKind) -> Op {
        match syntax_kind {
            SyntaxKind::Minus => Op::Sub,
            SyntaxKind::Plus => Op::Add,
            SyntaxKind::Slash => Op::Div,
            SyntaxKind::Star => Op::Mul,
            _ => Op::None,
        }
    }
}

impl From<OpType> for Op {
    fn from(optype: OpType) -> Self {
        match optype {
            OpType::Prefix(syntax_kind) => Self::prefix_operation_from(syntax_kind),
            OpType::Infix(syntax_kind) => Self::infix_operation_from(syntax_kind),
            OpType::Postfix(_syntax_kind) => todo!(),
        }
    }
}

impl Into<SyntaxKind> for Op {
    fn into(self) -> SyntaxKind {
        match self {
            Op::Add | Op::Sub | Op::Mul | Op::Div => SyntaxKind::InfixBinaryOp,
            Op::Neg => SyntaxKind::PrefixUnaryOp,
            Op::None => todo!(),
        }
    }
}

pub fn pratt_parser<B: ASTBehavior>(parser: &mut Parser) -> Option<Report> {
    parse_expression_until_binding_power::<B>(parser, 0)
}

fn parse_expression_until_binding_power<B: ASTBehavior>(
    parser: &mut Parser,
    min_binding_power: BindingPower,
) -> Option<Report> {
    let mut lhs_marker = if let Some(lhs) = left_hand_side::<B>(parser) {
        lhs
    } else {
        return None;
    };
    loop {
        match parser.peek::<B>() {
            None => return None,
            Some(Ok(token)) => match OpType::Infix(token.kind).into() {
                Op::None => {
                    return None;
                }
                op => {
                    let (left_binding_power, right_binding_power) = op.binding_power();
                    if left_binding_power < min_binding_power {
                        return None;
                    }
                    parser.bump();
                    let preceding_marker = lhs_marker.precede(parser);
                    parse_expression_until_binding_power::<B>(parser, right_binding_power);
                    lhs_marker = preceding_marker.complete(&mut parser.event_holder, op.into());
                }
            },
            Some(Err(err)) => return Some(err.clone().into()),
        }
    }
}

fn left_hand_side<B: ASTBehavior>(parser: &mut Parser) -> Option<Marker<Complete>> {
    let result = parser.peek::<B>()?;
    let lhs_marker = match result {
        Ok(token) => match token.kind {
            SyntaxKind::Number => literal(parser),
            SyntaxKind::Identifier => variable_ref(parser),
            SyntaxKind::Minus => prefix_expr::<B>(parser, SyntaxKind::Minus),
            SyntaxKind::LeftParen => paren_expr::<B>(parser),
            _ => todo!(),
        },
        Err(err) => todo!(),
    };
    Some(lhs_marker)
}

fn literal(parser: &mut Parser) -> Marker<Complete> {
    assert!(parser.is_next(SyntaxKind::Number));
    parser.bump_with_marker(SyntaxKind::Literal)
}

fn variable_ref(parser: &mut Parser) -> Marker<Complete> {
    assert!(parser.is_next(SyntaxKind::Identifier));
    parser.bump_with_marker(SyntaxKind::VariableRef)
}

fn prefix_expr<B: ASTBehavior>(parser: &mut Parser, kind: SyntaxKind) -> Marker<Complete> {
    assert!(parser.is_next(SyntaxKind::Minus));
    let marker = parser.start();
    parser.bump();
    let op: Op = OpType::Prefix(kind).into();
    let (_, right_binding_power) = op.binding_power();

    parse_expression_until_binding_power::<B>(parser, right_binding_power);
    marker.complete(&mut parser.event_holder, op.into())
}

fn paren_expr<B: ASTBehavior>(parser: &mut Parser) -> Marker<Complete> {
    assert!(parser.is_next(SyntaxKind::LeftParen));
    let marker = parser.start();
    parser.bump();
    parse_expression_until_binding_power::<B>(parser, 0);
    parser.bump_iff_or_panic(SyntaxKind::RightParen);
    marker.complete(&mut parser.event_holder, SyntaxKind::ParenExpr)
}

#[cfg(test)]
mod tests {

    use super::*;
    use expect_test::{expect, Expect};
    use parameterized_test::create;

    fn check<B: ASTBehavior>(prog: &str, expect: Expect) {
        let parse = Parser::new(prog).parse::<B>().expect("expected a parse");
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
        ignored_whitespaces: ("   ", expect![["Root@0..0" ]]),
        binary_add_two_numbers: ("3+14",
            expect![[
            "Root@0..4\n  InfixBinaryOp@0..4\n    Literal@0..1\n      Number@0..1 \"3\"\n    Plus@1..2 \"+\"\n    Literal@2..4\n      Number@2..4 \"14\""
            ]]),
        binary_add_four_numbers: ("3+14+159+2653",
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
        prefix_minus_precedence: ("-3+14",
            expect![[
                "Root@0..5\n  InfixBinaryOp@0..5\n    PrefixUnaryOp@0..2\n      Minus@0..1 \"-\"\n      Literal@1..2\n        Number@1..2 \"3\"\n    Plus@2..3 \"+\"\n    Literal@3..5\n      Number@3..5 \"14\""
            ]]
        ),

        parenthesised_pi: ("((314))",
            expect![[
                "Root@0..7\n  ParenExpr@0..7\n    LeftParen@0..1 \"(\"\n    ParenExpr@1..6\n      LeftParen@1..2 \"(\"\n      Literal@2..5\n        Number@2..5 \"314\"\n      RightParen@5..6 \")\"\n    RightParen@6..7 \")\""
            ]]
        ),

        parenthesised_sub_precedes_mul: ("(3+1)*4",
            expect![[
                "Root@0..7\n  InfixBinaryOp@0..7\n    ParenExpr@0..5\n      LeftParen@0..1 \"(\"\n      InfixBinaryOp@1..4\n        Literal@1..2\n          Number@1..2 \"3\"\n        Plus@2..3 \"+\"\n        Literal@3..4\n          Number@3..4 \"1\"\n      RightParen@4..5 \")\"\n    Star@5..6 \"*\"\n    Literal@6..7\n      Number@6..7 \"4\""
            ]]
        ),
    }

    create_non_ignoring_parser_test! {
        nothing: ("", expect![["Root@0..0"]]),
        single_digit: ("9", expect![["Root@0..1\n  Literal@0..1\n    Number@0..1 \"9\""]]),
        multiple_digit: ("314", expect![["Root@0..3\n  Literal@0..3\n    Number@0..3 \"314\""]]),
        identifier: ("ident", expect![["Root@0..5\n  VariableRef@0..5\n    Identifier@0..5 \"ident\""]]),
        whitespaces: ("   ", expect![["Root@0..3\n  Space@0..3 \"   \""]]),

    }
}
