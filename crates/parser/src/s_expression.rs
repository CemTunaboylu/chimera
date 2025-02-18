use crate::{
    parse_behaviors::ASTBehavior, parser::Parser, statement::statement, syntax::SyntaxKind,
};

// Note: Into and From will produce different kinds since from Op to SyntaxKind,
// we transition to a composite SyntaxKind.
pub(crate) enum OpType {
    Prefix(SyntaxKind),
    Infix(SyntaxKind),
    Postfix(SyntaxKind),
}
pub(crate) enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Neg,
    NotAnOp,
}

pub type BindingPower = u8;
pub const NO: BindingPower = 0;

impl Op {
    pub(crate) fn binding_power(&self) -> (BindingPower, BindingPower) {
        match self {
            Op::Neg => (NO, 5),
            // like rust-analyzer, dummy token has a binding power pair
            Op::NotAnOp => (0, 1),
            // left associative
            Op::Add | Op::Sub => (1, 2),
            Op::Mul | Op::Div => (3, 4),
            _ => (0, 0),
        }
    }
    pub(crate) fn prefix_operation_from(syntax_kind: SyntaxKind) -> Op {
        match syntax_kind {
            SyntaxKind::Minus => Op::Neg,
            _ => Op::NotAnOp,
        }
    }

    pub(crate) fn infix_operation_from(syntax_kind: SyntaxKind) -> Op {
        match syntax_kind {
            SyntaxKind::Minus => Op::Sub,
            SyntaxKind::Plus => Op::Add,
            SyntaxKind::Slash => Op::Div,
            SyntaxKind::Star => Op::Mul,
            _ => Op::NotAnOp,
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
            Op::NotAnOp => todo!(),
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
