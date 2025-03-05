use crate::{
    marker::{Complete, Marker},
    operator::Precedence,
    parse_behaviors::SyntaxingBehavior,
    parser::Parser,
    statement::statement,
};

use syntax::syntax_kind::SyntaxKind;

impl OpType {
    fn inject_expectations(&self, parser: &mut Parser) {
        let expectations = match self {
            OpType::Prefix(_) => {
                // left_hand_side injected each possibility, now we refine it
                parser.pop_last_expectation();
                &[SyntaxKind::PrefixUnaryOp]
            }
            OpType::Infix(_) => &[SyntaxKind::InfixBinOp],
            OpType::Postfix(_) => return,
        };

        parser.inject_expectations(expectations);
    }

    fn recover<B: SyntaxingBehavior>(&self, parser: &mut Parser) {
        match self {
            OpType::Prefix(_) => {}
            OpType::Infix(_) => parser.recover::<B>(),
            OpType::Postfix(_) => {}
        }
    }

    fn parse<B: SyntaxingBehavior>(
        self,
        parser: &mut Parser,
        min_binding_power: BindingPower,
        marker: Option<&Marker<Complete>>,
    ) -> Option<Marker<Complete>> {
        // note: after with a bump, expectations will be cleared
        self.inject_expectations(parser);
        let op: Op = (&self).into();
        match op {
            Op::End => None,
            Op::None => {
                self.recover::<B>(parser);
                None
            }
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

pub(crate) fn parse_expression_until_binding_power<B: SyntaxingBehavior>(
    parser: &mut Parser,
    min_precedence: Precedence,
) -> Option<Marker<Complete>> {
    let mut lhs_marker = left_hand_side::<B>(parser)?;
    'parsing: loop {
        match parser.peek::<B>()? {
            Ok(syntax) => {
                let kind = syntax.get_kind();
                if let Some(marker) =
                    OpType::Postfix(kind).parse::<B>(parser, min_binding_power, Some(&lhs_marker))
                {
                    lhs_marker = marker;
                    continue 'parsing;
                }

                // no-op cases where we can parse parse more don't need to recover i.e. Identifier, Literal etc.
                match kind {
                    SyntaxKind::Ident => {
                        lhs_marker = variable_ref::<B>(parser);
                        continue 'parsing;
                    }
                    SyntaxKind::Literal => {
                        lhs_marker = literal::<B>(parser);
                        continue 'parsing;
                    }
                    SyntaxKind::RParen => return None,
                    SyntaxKind::RBrace => return None,
                    _ => {}
                }

                if let Some(marker) =
                    OpType::Infix(kind).parse::<B>(parser, min_binding_power, Some(&lhs_marker))
                {
                    lhs_marker = marker;
                    continue 'parsing;
                }
                break;
            }
            Err(err) => {
                println!("{:?}", err);
                panic!("{:?}", format!("{:?}", err));
            }
        }
    }
    Some(lhs_marker)
}

const LHS_EXPECTATIONS: [SyntaxKind; 7] = [
    SyntaxKind::CharLit,
    SyntaxKind::Float,
    SyntaxKind::Ident,
    SyntaxKind::Int,
    SyntaxKind::LBrace,
    SyntaxKind::LParen,
    SyntaxKind::StrLit,
];

fn left_hand_side<B: SyntaxingBehavior>(parser: &mut Parser) -> Option<Marker<Complete>> {
    // note: after with a bump, expectations will be cleared
    parser.inject_expectations(&LHS_EXPECTATIONS);
    let result = parser.peek::<B>()?;
    let lhs_marker = match result {
        Ok(syntax) => match syntax.get_kind() {
            SyntaxKind::Int | SyntaxKind::Float | SyntaxKind::StrLit | SyntaxKind::CharLit => {
                literal::<B>(parser)
            }
            SyntaxKind::Ident => variable_ref::<B>(parser),
            SyntaxKind::Minus | SyntaxKind::Not => {
                OpType::Prefix(syntax.get_kind()).parse::<B>(parser, 0, None)?
            }
            SyntaxKind::LParen => paren_expr::<B>(parser),
            SyntaxKind::LBrace => block::<B>(parser),
            // TODO: below should be clever, only allowing the latest left's
            // right should be let pass by
            SyntaxKind::RParen => return None,
            SyntaxKind::RBrace => return None,
            SyntaxKind::PostFixUnaryOp => return None,
            _ => {
                parser.recover::<B>();
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

fn literal<B: SyntaxingBehavior>(parser: &mut Parser) -> Marker<Complete> {
    assert!(parser.check_next_syntax::<B>(|syntax| syntax.get_kind().is_literal_value()));
    parser.bump_with_marker(SyntaxKind::Literal)
}

fn variable_ref<B: SyntaxingBehavior>(parser: &mut Parser) -> Marker<Complete> {
    assert!(parser.is_next::<B>(SyntaxKind::Ident));
    parser.bump_with_marker(SyntaxKind::VariableRef)
}

fn paren_expr<B: SyntaxingBehavior>(parser: &mut Parser) -> Marker<Complete> {
    assert!(parser.is_next::<B>(SyntaxKind::LParen));
    let marker = parser.start();
    parser.bump();
    parse_expression_until_binding_power::<B>(parser, 0);
    parser.expect_and_bump::<B>(SyntaxKind::RParen);
    marker.complete(&mut parser.event_holder, SyntaxKind::ParenExpr)
}

fn block<B: SyntaxingBehavior>(parser: &mut Parser) -> Marker<Complete> {
    assert!(parser.is_next::<B>(SyntaxKind::LBrace));
    let marker = parser.start();
    parser.bump();
    while statement::<B>(parser).is_some() {}
    parser.expect_and_bump::<B>(SyntaxKind::RBrace);
    marker.complete(&mut parser.event_holder, SyntaxKind::Block)
}
