use crate::{
    expression::parse_expression_until_binding_power,
    marker::{Complete, Marker},
    parse_behaviors::ASTBehavior,
    parser::Parser,
};

use syntax::syntax::SyntaxKind;

pub(crate) fn statement<B: ASTBehavior>(parser: &mut Parser) -> Option<Marker<Complete>> {
    parser.restart_a_branch();
    let syntax = parser.peek::<B>()?;
    match syntax {
        Ok(syntax) if syntax.is_of_kind(SyntaxKind::LetKw) => variable_def::<B>(parser),
        Ok(syntax) if syntax.is_of_kind(SyntaxKind::RBrace) => block::<B>(parser),
        _ => parse_expression_until_binding_power::<B>(parser, 0),
    }
}

fn variable_def<B: ASTBehavior>(parser: &mut Parser) -> Option<Marker<Complete>> {
    assert!(parser.is_next::<B>(SyntaxKind::LetKw));
    let marker = parser.start();
    parser.bump();
    let _marker = parse_expression_until_binding_power::<B>(parser, 0);
    if _marker.is_none() {
        // recovering possibly no name VarDef let = <acceptable>
        if parser.check_next_syntax::<B>(|syntax| {
            syntax.kind.is_literal_value()
                || matches!(syntax.kind, SyntaxKind::Identifier | SyntaxKind::LBrace)
        }) {
            // try again if something valid is there, it will be bumped
            parse_expression_until_binding_power::<B>(parser, 0);
        }
    }
    Some(marker.complete(&mut parser.event_holder, SyntaxKind::VariableDef))
}

fn block<B: ASTBehavior>(parser: &mut Parser) -> Option<Marker<Complete>> {
    assert!(parser.is_next::<B>(SyntaxKind::RBrace));
    let marker = parser.start();
    parser.bump();
    let _marker = parse_expression_until_binding_power::<B>(parser, 0);
    if _marker.is_none() {
        // recovering possibly no name VarDef let = <acceptable>
        if parser.check_next_syntax::<B>(|syntax| {
            syntax.kind.is_literal_value()
                || matches!(syntax.kind, SyntaxKind::Identifier | SyntaxKind::LBrace)
        }) {
            // try again if something valid is there, it will be bumped
            parse_expression_until_binding_power::<B>(parser, 0);
        }
    }
    Some(marker.complete(&mut parser.event_holder, SyntaxKind::VariableDef))
}

#[cfg(test)]
mod tests {
    use crate::{parse_behaviors::IgnoreTrivia, s_expression::tests::check};
    use expect_test::expect;

    #[test]
    fn parse_variable_definition() {
        check::<IgnoreTrivia>(
            "let foo = bar",
            expect![[r#"
                Root@0..10
                  VariableDef@0..10
                    LetKw@0..3 "let"
                    InfixBinaryOp@3..10
                      VariableRef@3..6
                        Identifier@3..6 "foo"
                      Eq@6..7 "="
                      VariableRef@7..10
                        Identifier@7..10 "bar""#]],
        );
    }

    #[test]
    fn recover_on_let_token() {
        check::<IgnoreTrivia>(
            "let a =\nlet b = a",
            expect![[r#"
                Root@0..11
                  VariableDef@0..5
                    LetKw@0..3 "let"
                    InfixBinaryOp@3..5
                      VariableRef@3..4
                        Identifier@3..4 "a"
                      Eq@4..5 "="
                  VariableDef@5..11
                    LetKw@5..8 "let"
                    InfixBinaryOp@8..11
                      VariableRef@8..9
                        Identifier@8..9 "b"
                      Eq@9..10 "="
                      VariableRef@10..11
                        Identifier@10..11 "a""#]],
        );
    }
    #[test]
    fn recover_on_let_token_with_semicolon() {
        check::<IgnoreTrivia>(
            "let a =;let b = a",
            expect![[r#"
                Root@0..11
                  VariableDef@0..5
                    LetKw@0..3 "let"
                    InfixBinaryOp@3..5
                      VariableRef@3..4
                        Identifier@3..4 "a"
                      Eq@4..5 "="
                  VariableDef@5..11
                    LetKw@5..8 "let"
                    InfixBinaryOp@8..11
                      VariableRef@8..9
                        Identifier@8..9 "b"
                      Eq@9..10 "="
                      VariableRef@10..11
                        Identifier@10..11 "a""#]],
        );
    }

    #[test]
    fn parse_multiple_statements() {
        check::<IgnoreTrivia>(
            "let a = 1\na",
            expect![[r#"
                Root@0..7
                  VariableDef@0..7
                    LetKw@0..3 "let"
                    InfixBinaryOp@3..7
                      VariableRef@3..4
                        Identifier@3..4 "a"
                      Eq@4..5 "="
                      Literal@5..6
                        Number@5..6 "1"
                      VariableRef@6..7
                        Identifier@6..7 "a""#]],
        );
    }
    #[test]
    fn parse_multiple_statements_with_semicolon() {
        check::<IgnoreTrivia>(
            "let a = 0; a",
            expect![[r#"
                Root@0..7
                  VariableDef@0..6
                    LetKw@0..3 "let"
                    InfixBinaryOp@3..6
                      VariableRef@3..4
                        Identifier@3..4 "a"
                      Eq@4..5 "="
                      Literal@5..6
                        Number@5..6 "0"
                  VariableRef@6..7
                    Identifier@6..7 "a""#]],
        );
    }

    /*
    {
      let a = {
        let bar =
      }
      10
    }

         */
}
