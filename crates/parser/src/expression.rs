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
            OpType::Prefix(_) => &[SyntaxKind::PrefixUnaryOp], // left_hand_side has its own function
            OpType::Infix(_) => &[SyntaxKind::InfixBinaryOp],
            OpType::Postfix(_) => &[SyntaxKind::PostFixUnaryOp],
        };

        parser.inject_expectations(expectations);
    }

    fn should_recurse_with_recovery(&self, parser: &mut Parser) -> Option<()> {
        match self {
            OpType::Prefix(_) => {}
            OpType::Infix(syntax_kind) => match syntax_kind {
                SyntaxKind::Identifier | SyntaxKind::Literal => return Some(()),
                SyntaxKind::RParen => {}
                _ => {
                    parser.recover();
                }
            },
            OpType::Postfix(_) => {}
        }
        None
    }
    fn parse<B: ASTBehavior>(
        self,
        parser: &mut Parser,
        min_binding_power: BindingPower,
        marker: Option<&Marker<Complete>>,
    ) -> Option<Marker<Complete>> {
        // note: after with a bump, expectations will be cleared
        self.inject_expectations(parser);
        let op: Op = (&self).into();
        match op {
            Op::None => self
                .should_recurse_with_recovery(parser)
                .and_then(|_| parse_expression_until_binding_power::<B>(parser, min_binding_power)),
            _ => {
                let (left_binding_power, right_binding_power) = op.binding_power();
                if left_binding_power < min_binding_power {
                    return None;
                }
                let new_marker = if let Some(marker) = marker {
                    parser.bump();
                    marker.precede(parser)
                } else {
                    let marker = parser.start();
                    parser.bump();
                    marker
                };
                parse_expression_until_binding_power::<B>(parser, right_binding_power);
                Some(new_marker.complete(&mut parser.event_holder, op.into()))
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
                        op_type(kind).parse::<B>(parser, min_binding_power, Some(&lhs_marker))
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
        Ok(syntax) => match syntax.kind {
            SyntaxKind::Number | SyntaxKind::StringLiteral | SyntaxKind::CharLiteral => {
                literal(parser)
            }
            SyntaxKind::Identifier => variable_ref(parser),
            SyntaxKind::Minus | SyntaxKind::Not => {
                OpType::Prefix(syntax.kind).parse::<B>(parser, 0, None)?
            }
            SyntaxKind::LParen => paren_expr::<B>(parser),
            SyntaxKind::RParen | SyntaxKind::PostFixUnaryOp => return None,
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
    assert!(parser.check_next_syntax(|syntax| syntax.kind.is_literal_value()));
    parser.bump_with_marker(SyntaxKind::Literal)
}

fn variable_ref(parser: &mut Parser) -> Marker<Complete> {
    assert!(parser.is_next(SyntaxKind::Identifier));
    parser.bump_with_marker(SyntaxKind::VariableRef)
}

fn paren_expr<B: ASTBehavior>(parser: &mut Parser) -> Marker<Complete> {
    assert!(parser.is_next(SyntaxKind::LParen));
    let marker = parser.start();
    parser.bump();
    parse_expression_until_binding_power::<B>(parser, 0);
    parser.expect_and_bump::<B>(SyntaxKind::RParen);
    marker.complete(&mut parser.event_holder, SyntaxKind::ParenExpr)
}
