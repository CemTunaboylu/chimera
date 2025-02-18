use crate::{
    marker::{Complete, Marker},
    parse_behaviors::ASTBehavior,
    parser::Parser,
    s_expression::{BindingPower, Op, OpType},
    syntax::SyntaxKind,
};

pub(crate) fn parse_expression_until_binding_power<B: ASTBehavior>(
    parser: &mut Parser,
    min_binding_power: BindingPower,
) -> Option<Marker<Complete>> {
    let mut lhs_marker = if let Some(lhs) = left_hand_side::<B>(parser) {
        lhs
    } else {
        return None;
    };
    loop {
        // parser.inject_expectations(&SyntaxKind::operators());
        parser.inject_expectations(&[SyntaxKind::InfixBinaryOp]);
        match parser.peek::<B>() {
            None => break,
            Some(Ok(token)) => match OpType::Infix(token.kind).into() {
                Op::NotAnOp => match token.kind {
                    SyntaxKind::Identifier => {
                        lhs_marker = variable_ref(parser);
                    }
                    SyntaxKind::RParen => {
                        return None;
                    }
                    _ => {
                        parser.recover();
                        return None;
                    }
                },
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
            Some(Err(err)) => todo!(),
        }
    }

    Some(lhs_marker)
}

const LHS_EXPECTATIONS: [SyntaxKind; 5] = [
    SyntaxKind::Number,
    SyntaxKind::Identifier,
    SyntaxKind::PrefixOp,
    SyntaxKind::PrefixUnaryOp,
    SyntaxKind::LParen,
];

fn left_hand_side<B: ASTBehavior>(parser: &mut Parser) -> Option<Marker<Complete>> {
    parser.inject_expectations(&LHS_EXPECTATIONS);
    let result = parser.peek::<B>()?;
    let lhs_marker = match result {
        // TODO: matches do not provide expected ones thus, should change it?
        // Or, I can add all of the options before the match and then match?
        Ok(token) => match token.kind {
            SyntaxKind::Number => literal(parser),
            SyntaxKind::Identifier => variable_ref(parser),
            SyntaxKind::Minus => prefix_expr::<B>(parser, SyntaxKind::Minus),
            SyntaxKind::LParen => paren_expr::<B>(parser),
            SyntaxKind::RParen => return None,
            _ => {
                // ')' hits here if we only provide "()"
                parser.recover();
                return None;
            }
        },
        Err(_err) => todo!(),
    };
    Some(lhs_marker)
}

fn literal(parser: &mut Parser) -> Marker<Complete> {
    assert!(parser.check_next_syntax(|token| matches!(token.kind, SyntaxKind::Number)));
    let marker = parser.bump_with_marker(SyntaxKind::Literal);
    // parser.check_next_syntax(Syntax::is_a_separator);
    marker
}

fn variable_ref(parser: &mut Parser) -> Marker<Complete> {
    assert!(parser.is_next(SyntaxKind::Identifier));
    let marker = parser.bump_with_marker(SyntaxKind::VariableRef);
    // parser.check_next_syntax(Syntax::is_a_separator);
    marker
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
    assert!(parser.is_next(SyntaxKind::LParen));
    let marker = parser.start();
    parser.bump();
    parse_expression_until_binding_power::<B>(parser, 0);
    parser.expect_and_bump::<B>(SyntaxKind::RParen);
    marker.complete(&mut parser.event_holder, SyntaxKind::ParenExpr)
}
