use crate::{
    marker::{Complete, Marker},
    parse_behaviors::ASTBehavior,
    parser::Parser,
    s_expression::{BindingPower, Op, OpType},
};

use syntax::syntax::SyntaxKind;

impl OpType {
    fn inject_expectations(&self, parser: &mut Parser) {
        let expectations = match self {
            OpType::Prefix(_) => return, // left_hand_side has its own function
            OpType::Infix(_) => &[SyntaxKind::InfixBinaryOp],
            OpType::Postfix(_) => &[SyntaxKind::PostFixUnaryOp],
        };

        parser.inject_expectations(expectations);
    }

    fn handle_no_op(&self, parser: &mut Parser) -> Option<Marker<Complete>> {
        match self {
            OpType::Prefix(_) => unreachable!(),
            OpType::Infix(syntax_kind) => match syntax_kind {
                SyntaxKind::Identifier => {
                    return Some(variable_ref(parser));
                }
                SyntaxKind::RParen => {}
                _ => {
                    parser.recover();
                }
            },
            OpType::Postfix(_) => {}
        }
        return None;
    }
    fn parse<B: ASTBehavior>(
        self,
        parser: &mut Parser,
        min_binding_power: BindingPower,
        marker: &Marker<Complete>,
    ) -> Option<Marker<Complete>> {
        // note: after with a bump, expectations will be cleared
        self.inject_expectations(parser);
        let op: Op = (&self).into();
        match op {
            Op::None => self.handle_no_op(parser),
            _ => {
                let (left_binding_power, right_binding_power) = op.binding_power();
                if left_binding_power < min_binding_power {
                    return None;
                }
                parser.bump();
                let preceding_marker = marker.precede(parser);
                parse_expression_until_binding_power::<B>(parser, right_binding_power);
                return Some(preceding_marker.complete(&mut parser.event_holder, op.into()));
            }
        }
    }
}

pub(crate) fn parse_expression_until_binding_power<B: ASTBehavior>(
    parser: &mut Parser,
    min_binding_power: BindingPower,
) -> Option<Marker<Complete>> {
    let mut lhs_marker = left_hand_side::<B>(parser)?;
    'parsing: loop {
        match parser.peek::<B>()? {
            Ok(syntax) => {
                let kind = syntax.kind;
                for op_type in [OpType::Postfix, OpType::Infix] {
                    if let Some(marker) =
                        op_type(kind).parse::<B>(parser, min_binding_power, &lhs_marker)
                    {
                        lhs_marker = marker;
                        continue 'parsing;
                    }
                }
            }
            Err(err) => {
                println!("{:?}", err);
                panic!("{:?}", format!("{:?}", err));
            }
        }
        break;
    }
    Some(lhs_marker)
}

const LHS_EXPECTATIONS: [SyntaxKind; 4] = [
    SyntaxKind::Number,
    SyntaxKind::Identifier,
    SyntaxKind::PrefixUnaryOp,
    SyntaxKind::LParen,
];

fn left_hand_side<B: ASTBehavior>(parser: &mut Parser) -> Option<Marker<Complete>> {
    // note: after with a bump, expectations will be cleared
    parser.inject_expectations(&LHS_EXPECTATIONS);
    let result = parser.peek::<B>()?;
    let lhs_marker = match result {
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
        Err(_err) => {
            println!("{:?}", _err);
            panic!("{:?}", _err);
        }
    };
    Some(lhs_marker)
}

fn literal(parser: &mut Parser) -> Marker<Complete> {
    assert!(parser.check_next_syntax(|token| matches!(token.kind, SyntaxKind::Number)));
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
    let op: Op = (&OpType::Prefix(kind)).into();
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
