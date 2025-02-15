use crate::{
    lexer::*,
    marker::{Complete, Marker},
    parser::*,
};

pub type BindingPower = u8;

// Note: Into and From will produce different kinds since from Op to SyntaxKind,
// we transition to a composite SyntaxKind.

enum Op {
    Pre,
    In,
    Post,
}
enum InfixOp {
    Add,
    Sub,
    Mul,
    Div,
    No,
}
impl InfixOp {
    fn binding_power(&self) -> (BindingPower, BindingPower) {
        match self {
            // left associative
            InfixOp::Add | InfixOp::Sub => (1, 2),
            InfixOp::Mul | InfixOp::Div => (3, 4),
            _ => (0, 0),
        }
    }
}

impl From<SyntaxKind> for InfixOp {
    fn from(tokenkind: SyntaxKind) -> Self {
        match tokenkind {
            SyntaxKind::And => todo!(),
            SyntaxKind::AndAnd => todo!(),
            SyntaxKind::AndEq => todo!(),
            SyntaxKind::Colon => todo!(),
            SyntaxKind::Dot => todo!(),
            SyntaxKind::DotDot => todo!(),
            SyntaxKind::DotDotDot => todo!(),
            SyntaxKind::Eq => todo!(),
            SyntaxKind::EqEq => todo!(),
            SyntaxKind::Exclamation => todo!(),
            SyntaxKind::GreaterThan => todo!(),
            SyntaxKind::KwIn => todo!(),
            SyntaxKind::LeftBrace => todo!(),
            SyntaxKind::LeftParen => todo!(),
            SyntaxKind::LeftShift => todo!(),
            SyntaxKind::LeftShiftEq => todo!(),
            SyntaxKind::LeftSquareBrac => todo!(),
            SyntaxKind::LessThan => todo!(),
            SyntaxKind::Minus => InfixOp::Sub,
            SyntaxKind::MinusEq => todo!(),
            SyntaxKind::Modulus => todo!(),
            SyntaxKind::ModulusEq => todo!(),
            SyntaxKind::NamespaceSep => todo!(),
            SyntaxKind::Not => todo!(),
            SyntaxKind::NotEq => todo!(),
            SyntaxKind::Or => todo!(),
            SyntaxKind::OrEq => todo!(),
            SyntaxKind::OrOr => todo!(),
            SyntaxKind::Plus => InfixOp::Add,
            SyntaxKind::PlusEq => todo!(),
            SyntaxKind::RightArrow => todo!(),
            SyntaxKind::RightBrace => todo!(),
            SyntaxKind::RightShift => todo!(),
            SyntaxKind::RightShiftEq => todo!(),
            SyntaxKind::RightSquareBrac => todo!(),
            SyntaxKind::Slash => InfixOp::Div,
            SyntaxKind::SlashEq => todo!(),
            SyntaxKind::Star => InfixOp::Mul,
            SyntaxKind::StarEq => todo!(),
            _ => InfixOp::No,
        }
    }
}

impl Into<SyntaxKind> for InfixOp {
    fn into(self) -> SyntaxKind {
        match self {
            InfixOp::Add | InfixOp::Sub | InfixOp::Mul | InfixOp::Div => SyntaxKind::InfixBinaryOp,
            InfixOp::No => todo!(),
        }
    }
}

enum PrefixOp {
    Neg,
    No,
}
impl PrefixOp {
    fn binding_power(&self) -> ((), BindingPower) {
        match self {
            PrefixOp::Neg => ((), 5),
            _ => ((), 0),
        }
    }
}

impl From<SyntaxKind> for PrefixOp {
    fn from(tokenkind: SyntaxKind) -> Self {
        match tokenkind {
            SyntaxKind::And => todo!(),
            SyntaxKind::Colon => todo!(),
            SyntaxKind::Exclamation => todo!(),
            SyntaxKind::LeftBrace => todo!(),
            SyntaxKind::LeftParen => todo!(),
            SyntaxKind::LeftSquareBrac => todo!(),
            SyntaxKind::Minus => PrefixOp::Neg,
            SyntaxKind::Not => todo!(),
            SyntaxKind::Star => todo!(),
            _ => PrefixOp::No,
        }
    }
}

impl Into<SyntaxKind> for PrefixOp {
    fn into(self) -> SyntaxKind {
        match self {
            PrefixOp::Neg => SyntaxKind::PrefixOp,
            PrefixOp::No => todo!(),
        }
    }
}

pub fn pratt_parser<B: ASTBehavior>(parser: &mut Parser) {
    parse_expression_until_binding_power::<B>(parser, 0);
}

fn parse_expression_until_binding_power<B: ASTBehavior>(
    parser: &mut Parser,
    min_binding_power: BindingPower,
) {
    let peek_with_behavior = |parser: &mut Parser| parser.peek::<B>();
    let parse_expression_until_binding_power_with_behavior =
        |parser: &mut Parser, min_binding_power: BindingPower| {
            parse_expression_until_binding_power::<B>(parser, min_binding_power)
        };

    let bump_with_marker = |parser: &mut Parser, kind: SyntaxKind| -> Marker<Complete> {
        let marker = parser.start();
        parser.bump();
        marker.complete(&mut parser.event_holder, kind)
    };

    let mut lhs_marker = match peek_with_behavior(parser) {
        Some(Result::Ok(SyntaxKind::Number)) | Some(Result::Ok(SyntaxKind::Identifier)) => {
            bump_with_marker(parser, SyntaxKind::Literal)
        }
        Some(Result::Ok(SyntaxKind::Minus)) => {
            let marker = parser.start();
            parser.bump();
            let op = PrefixOp::from(SyntaxKind::Minus);
            let (_, right_binding_power) = op.binding_power();

            parse_expression_until_binding_power_with_behavior(parser, right_binding_power);
            marker.complete(&mut parser.event_holder, op.into())
        }
        Some(Result::Ok(SyntaxKind::LeftParen)) => {
            let marker = parser.start();
            parser.bump();
            parse_expression_until_binding_power_with_behavior(parser, 0);
            assert_eq!(
                peek_with_behavior(parser),
                Some(Result::Ok(SyntaxKind::RightParen))
            );
            parser.bump();
            marker.complete(&mut parser.event_holder, SyntaxKind::ParenExpr)
        }
        None => return,
        _ => todo!(),
    };
    loop {
        match peek_with_behavior(parser) {
            None => return,
            Some(Result::Ok(kind)) => match InfixOp::from(kind) {
                InfixOp::No => {
                    return;
                }
                op => {
                    let (left_binding_power, right_binding_power) = op.binding_power();
                    if left_binding_power < min_binding_power {
                        return;
                    }
                    parser.bump();
                    let preceding_marker = lhs_marker.precede(parser);
                    parse_expression_until_binding_power_with_behavior(parser, right_binding_power);
                    lhs_marker = preceding_marker.complete(&mut parser.event_holder, op.into());
                }
            },
            _ => return,
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use expect_test::{expect, Expect};
    use parameterized_test::create;

    fn check<B: ASTBehavior>(prog: &str, expect: Expect) {
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
        identifier: ("ident", expect![["Root@0..5\n  Literal@0..5\n    Identifier@0..5 \"ident\""]]),
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
                "Root@0..2\n  PrefixOp@0..2\n    Minus@0..1 \"-\"\n    Literal@1..2\n      Number@1..2 \"9\""
            ]]
        ),
        prefix_minus_precedence: ("-3+14",
            expect![[
                "Root@0..5\n  InfixBinaryOp@0..5\n    PrefixOp@0..2\n      Minus@0..1 \"-\"\n      Literal@1..2\n        Number@1..2 \"3\"\n    Plus@2..3 \"+\"\n    Literal@3..5\n      Number@3..5 \"14\""
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
        identifier: ("ident", expect![["Root@0..5\n  Literal@0..5\n    Identifier@0..5 \"ident\""]]),
        whitespaces: ("   ", expect![["Root@0..3\n  Space@0..3 \"   \""]]),

    }
}
