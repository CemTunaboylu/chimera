use crate::{
    marker::{Incomplete, Marker},
    operator::starting_precedence,
    parse::{Finished, SeparatedElement},
    parser::Parser,
};
use lexer::token_type::TokenType;
use syntax::{Syntax, syntax_kind::SyntaxKind};

use SyntaxKind::*;
use thin_vec::{ThinVec, thin_vec};

fn ident_or_type(syntax: Syntax) -> bool {
    matches!(
        syntax.get_token_type(),
        TokenType::Type | TokenType::Identifier
    )
}

impl<'input> Parser<'input> {
    // fn <ident>({parameters as CSV}) {-> RetType} {}
    #[allow(unused_variables)]
    pub fn parse_function_def(&self) -> Option<Finished> {
        let marker = self.start();
        self.expect_and_bump(KwFn);
        {
            let rollback_after_drop = self.roll_back_context_after_drop();
            self.context.borrow().disallow_recovery_of(LParen);
            self.expect_and_bump(Ident);
        }
        self.expect_and_bump(LParen);
        if !self.is_next(RParen) {
            let rollback_after_drop = self.roll_back_context_after_drop();
            self.context.borrow().disallow_recovery_of(RParen);
            self.parse_comma_separated_typed_declarations_until(|syntax: Syntax| {
                !syntax.is_of_kind(RParen)
            });
        }

        let rollback_after_drop = self.roll_back_context_after_drop();
        self.context.borrow().disallow_recovery_of(LBrace);
        self.expect_and_bump(RParen);

        if self.is_next(RArrow) {
            let ret_type_marker = self.start();
            self.expect_and_bump(RArrow);
            if self.is_next(And) {
                self.parse_prefix_unary_operation(And);
            } else {
                self.expect_f_and_bump(ident_or_type);
            }
            self.complete_marker_with(ret_type_marker, RetType);
        }

        self.parse_block();
        Some(self.complete_marker_with(marker, SyntaxKind::FnDef))
    }

    // the identifier is already bumped at this point because we ended up here
    // from parsing an identifier and saw a LParen as well.
    // TODO: this should also need to be able to parse func(&mut obj)
    pub fn parse_function_call(&self, marker: Marker<Incomplete>) -> Finished {
        self.expect_and_bump(LParen);
        self.parse_comma_separated_arguments_until(|syntax: Syntax| !syntax.is_of_kind(RParen));
        self.expect_and_bump(RParen);
        self.complete_marker_with(marker, FnCall)
    }

    #[allow(unused_variables)]
    pub fn parse_comma_separated_typed_declarations_until(&self, until_false: fn(Syntax) -> bool) {
        let rollback_when_dropped = self.roll_back_context_after_drop();
        let ctx = self.context.borrow();
        ctx.forbid_all();
        ctx.allow([And, Colon, Comma, Ident, KwMut, RParen].as_ref());
        ctx.allow(SyntaxKind::types().as_ref());

        use SeparatedElement::*;

        let ref_mut_with = |s: SeparatedElement| RefMut(thin_vec![s]);
        let arg_elements = thin_vec![Kind(Ident), Kind(Colon), ref_mut_with(Fn(ident_or_type))];
        let can_be_a_method = thin_vec![Branched(
            thin_vec![ref_mut_with(KindWithMarker(Kwself, SelfRef))],
            arg_elements
        )];

        self.parse_arg(&can_be_a_method);

        let arg_elements = thin_vec![Kind(Ident), Kind(Colon), ref_mut_with(Fn(ident_or_type))];
        while let Some(Ok(peeked)) = self.peek() {
            if !until_false(peeked.clone()) {
                break;
            }
            self.parse_arg(&arg_elements);
        }
    }

    fn parse_arg(&self, elements: &ThinVec<SeparatedElement>) {
        let marker = self.start();
        self.parse_arg_after_colon(elements);
        self.complete_marker_with(marker, ParamDecl);
    }

    pub fn parse_arg_after_colon(&self, elements: &ThinVec<SeparatedElement>) {
        if self.is_next(And) {
            self.parse_ref_arg(elements);
        } else if self.is_next(KwMut) {
            self.parse_mut_arg(elements);
        } else {
            self.parse_arg_with(elements);
        }
    }
    fn parse_ref_arg(&self, elements: &ThinVec<SeparatedElement>) -> Finished {
        let marker = self.start();
        self.expect_and_bump(And);
        if self.is_next(KwMut) {
            self.parse_mut_arg(elements);
        } else {
            self.parse_arg_with(elements);
        }
        self.complete_marker_with(marker, PrefixUnaryOp)
    }

    fn parse_mut_arg(&self, elements: &ThinVec<SeparatedElement>) -> Finished {
        let mut_marker = self.start();
        self.expect_and_bump(KwMut);
        self.parse_arg_with(elements);
        self.complete_marker_with(mut_marker, Mut)
    }

    fn parse_arg_with(&self, elements: &ThinVec<SeparatedElement>) {
        self.parse_with(elements);
        // we stopped at Comma, thus need to consume if now to enable the following parse
        self.ignore_if(Comma);
    }

    #[allow(unused_variables)]
    pub fn parse_comma_separated_arguments_until(&self, until: fn(Syntax) -> bool) -> Option<()> {
        let rollback_when_dropped = self.roll_back_context_after_drop();
        self.context.borrow().allow(RParen);

        use SeparatedElement::*;

        self.parse_separated_by(
            &thin_vec![ParseExprWith(starting_precedence())],
            FnArg,
            Comma,
            until,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{cst::ConcreteSyntaxTree, parse::tests::check, sink::Sink};
    use expect_test::expect;
    use parameterized_test::create;

    create! {
        create_parser_test,
        (prog, expect), {
            check(prog, expect);
        }
    }

    #[test]
    fn several_valid_comma_separated_parameter_declarations() {
        use SyntaxKind::*;
        let params = "me:human, lang: Language, pet: Cat)";
        let parser = Parser::new(params);
        let root = parser.start();
        parser.parse_comma_separated_typed_declarations_until(|syntax: Syntax| {
            !syntax.is_of_kind(RParen)
        });
        parser.complete_marker_with(root, Root);
        let lexer = parser.lexer.borrow();
        let sink = Sink::new(parser.event_holder.take().into(), lexer.source());
        let cst = ConcreteSyntaxTree::from(sink);

        let expect = expect![[r#"
            Root@0..32
              ParamDecl@0..9
                Ident@0..2 "me"
                Colon@2..3 ":"
                Ident@3..8 "human"
                Whitespace@8..9 " "
              ParamDecl@9..24
                Ident@9..13 "lang"
                Colon@13..14 ":"
                Whitespace@14..15 " "
                Ident@15..23 "Language"
                Whitespace@23..24 " "
              ParamDecl@24..32
                Ident@24..27 "pet"
                Colon@27..28 ":"
                Whitespace@28..29 " "
                Ident@29..32 "Cat""#]];

        let debug_tree = cst.debug_tree();
        expect.assert_eq(&debug_tree);
    }

    create_parser_test! {
    function_def_with_no_parameters: ("fn empty() {}",
              expect![[r#"
                Root@0..13
                  FnDef@0..13
                    KwFn@0..2 "fn"
                    Whitespace@2..3 " "
                    Ident@3..8 "empty"
                    LParen@8..9 "("
                    RParen@9..10 ")"
                    Whitespace@10..11 " "
                    Block@11..13
                      LBrace@11..12 "{"
                      RBrace@12..13 "}""#]],
          ),

    function_def_with_single_parameter: ("fn empty(single:i32) {}",
        expect![[r#"
            Root@0..23
              FnDef@0..23
                KwFn@0..2 "fn"
                Whitespace@2..3 " "
                Ident@3..8 "empty"
                LParen@8..9 "("
                ParamDecl@9..19
                  Ident@9..15 "single"
                  Colon@15..16 ":"
                  TyI32@16..19 "i32"
                RParen@19..20 ")"
                Whitespace@20..21 " "
                Block@21..23
                  LBrace@21..22 "{"
                  RBrace@22..23 "}""#]],
    ),

    function_def_with_multiple_parameters: ("fn empty(first:i32, second:char, third: Structure) {}",
        expect![[r#"
            Root@0..51
              FnDef@0..51
                KwFn@0..2 "fn"
                Whitespace@2..3 " "
                Ident@3..8 "empty"
                LParen@8..9 "("
                ParamDecl@9..19
                  Ident@9..14 "first"
                  Colon@14..15 ":"
                  TyI32@15..18 "i32"
                  Whitespace@18..19 " "
                ParamDecl@19..31
                  Ident@19..25 "second"
                  Colon@25..26 ":"
                  TyChar@26..30 "char"
                  Whitespace@30..31 " "
                ParamDecl@31..47
                  Ident@31..36 "third"
                  Colon@36..37 ":"
                  Whitespace@37..38 " "
                  Ident@38..47 "Structure"
                RParen@47..48 ")"
                Whitespace@48..49 " "
                Block@49..51
                  LBrace@49..50 "{"
                  RBrace@50..51 "}""#]],
    ),

    function_def_with_multiple_parameters_with_actual_body: ("fn empty(first:i32, second:char) -> bool {let sum_1 = first + second; let sum_2 = second+first; first == check}",
              expect![[r#"
                  Root@0..110
                    FnDef@0..110
                      KwFn@0..2 "fn"
                      Whitespace@2..3 " "
                      Ident@3..8 "empty"
                      LParen@8..9 "("
                      ParamDecl@9..19
                        Ident@9..14 "first"
                        Colon@14..15 ":"
                        TyI32@15..18 "i32"
                        Whitespace@18..19 " "
                      ParamDecl@19..30
                        Ident@19..25 "second"
                        Colon@25..26 ":"
                        TyChar@26..30 "char"
                      RParen@30..31 ")"
                      Whitespace@31..32 " "
                      RetType@32..39
                        RArrow@32..34 "->"
                        Whitespace@34..35 " "
                        TyBool@35..39 "bool"
                      Block@39..110
                        Whitespace@39..40 " "
                        LBrace@40..41 "{"
                        VarDef@41..68
                          KwLet@41..44 "let"
                          Whitespace@44..45 " "
                          InfixBinOp@45..67
                            VarRef@45..51
                              Ident@45..50 "sum_1"
                              Whitespace@50..51 " "
                            Eq@51..52 "="
                            Whitespace@52..53 " "
                            InfixBinOp@53..67
                              VarRef@53..59
                                Ident@53..58 "first"
                                Whitespace@58..59 " "
                              Plus@59..60 "+"
                              Whitespace@60..61 " "
                              VarRef@61..67
                                Ident@61..67 "second"
                          Semi@67..68 ";"
                        Whitespace@68..69 " "
                        VarDef@69..94
                          KwLet@69..72 "let"
                          Whitespace@72..73 " "
                          InfixBinOp@73..93
                            VarRef@73..79
                              Ident@73..78 "sum_2"
                              Whitespace@78..79 " "
                            Eq@79..80 "="
                            Whitespace@80..81 " "
                            InfixBinOp@81..93
                              VarRef@81..87
                                Ident@81..87 "second"
                              Plus@87..88 "+"
                              VarRef@88..93
                                Ident@88..93 "first"
                          Semi@93..94 ";"
                        Whitespace@94..95 " "
                        InfixBinOp@95..109
                          VarRef@95..101
                            Ident@95..100 "first"
                            Whitespace@100..101 " "
                          EqEq@101..103 "=="
                          Whitespace@103..104 " "
                          VarRef@104..109
                            Ident@104..109 "check"
                        RBrace@109..110 "}""#]],
    ),

    function_def_with_return: ("fn sum(a:i32, b:i32) -> i32 { return a+b; }",
              expect![[r#"
                  Root@0..42
                    FnDef@0..42
                      KwFn@0..2 "fn"
                      Whitespace@2..3 " "
                      Ident@3..6 "sum"
                      LParen@6..7 "("
                      ParamDecl@7..13
                        Ident@7..8 "a"
                        Colon@8..9 ":"
                        TyI32@9..12 "i32"
                        Whitespace@12..13 " "
                      ParamDecl@13..18
                        Ident@13..14 "b"
                        Colon@14..15 ":"
                        TyI32@15..18 "i32"
                      RParen@18..19 ")"
                      Whitespace@19..20 " "
                      RetType@20..26
                        RArrow@20..22 "->"
                        Whitespace@22..23 " "
                        TyI32@23..26 "i32"
                      Block@26..42
                        Whitespace@26..27 " "
                        LBrace@27..28 "{"
                        Whitespace@28..29 " "
                        Semi@29..40
                          Jump@29..39
                            KwReturn@29..35 "return"
                            Whitespace@35..36 " "
                            InfixBinOp@36..39
                              VarRef@36..37
                                Ident@36..37 "a"
                              Plus@37..38 "+"
                              VarRef@38..39
                                Ident@38..39 "b"
                          Semi@39..40 ";"
                        Whitespace@40..41 " "
                        RBrace@41..42 "}""#]],
    ),

    function_call_with_single_parameter: ("empty(single)",
        expect![[r#"
            Root@0..13
              FnCall@0..13
                Ident@0..5 "empty"
                LParen@5..6 "("
                FnArg@6..12
                  VarRef@6..12
                    Ident@6..12 "single"
                RParen@12..13 ")""#]],
    ),
        fn_with_ref_mut_self: ("fn translate(&mut self, by: Point) { self.x += by.x; self.y += by.y;}",
            expect![[r#"
                Root@0..68
                  FnDef@0..68
                    KwFn@0..2 "fn"
                    Whitespace@2..3 " "
                    Ident@3..12 "translate"
                    LParen@12..13 "("
                    ParamDecl@13..23
                      PrefixUnaryOp@13..23
                        And@13..14 "&"
                        Mut@14..23
                          KwMut@14..17 "mut"
                          Whitespace@17..18 " "
                          SelfRef@18..22
                            Kwself@18..22 "self"
                          Whitespace@22..23 " "
                    ParamDecl@23..32
                      Ident@23..25 "by"
                      Colon@25..26 ":"
                      Whitespace@26..27 " "
                      Ident@27..32 "Point"
                    RParen@32..33 ")"
                    Whitespace@33..34 " "
                    Block@34..68
                      LBrace@34..35 "{"
                      Whitespace@35..36 " "
                      Semi@36..51
                        InfixBinOp@36..50
                          InfixBinOp@36..43
                            SelfRef@36..40
                              Kwself@36..40 "self"
                            Dot@40..41 "."
                            VarRef@41..43
                              Ident@41..42 "x"
                              Whitespace@42..43 " "
                          PlusEq@43..45 "+="
                          Whitespace@45..46 " "
                          InfixBinOp@46..50
                            VarRef@46..48
                              Ident@46..48 "by"
                            Dot@48..49 "."
                            VarRef@49..50
                              Ident@49..50 "x"
                        Semi@50..51 ";"
                      Whitespace@51..52 " "
                      Semi@52..67
                        InfixBinOp@52..66
                          InfixBinOp@52..59
                            SelfRef@52..56
                              Kwself@52..56 "self"
                            Dot@56..57 "."
                            VarRef@57..59
                              Ident@57..58 "y"
                              Whitespace@58..59 " "
                          PlusEq@59..61 "+="
                          Whitespace@61..62 " "
                          InfixBinOp@62..66
                            VarRef@62..64
                              Ident@62..64 "by"
                            Dot@64..65 "."
                            VarRef@65..66
                              Ident@65..66 "y"
                        Semi@66..67 ";"
                      RBrace@67..68 "}""#]],
        ),

        fn_with_ref_mut_param: ("fn translate(&mut self, by: &mut Point) { }",
            expect![[r#"
                Root@0..42
                  FnDef@0..42
                    KwFn@0..2 "fn"
                    Whitespace@2..3 " "
                    Ident@3..12 "translate"
                    LParen@12..13 "("
                    ParamDecl@13..23
                      PrefixUnaryOp@13..23
                        And@13..14 "&"
                        Mut@14..23
                          KwMut@14..17 "mut"
                          Whitespace@17..18 " "
                          SelfRef@18..22
                            Kwself@18..22 "self"
                          Whitespace@22..23 " "
                    ParamDecl@23..37
                      Ident@23..25 "by"
                      Colon@25..26 ":"
                      Whitespace@26..27 " "
                      PrefixUnaryOp@27..37
                        And@27..28 "&"
                        Mut@28..37
                          KwMut@28..31 "mut"
                          Whitespace@31..32 " "
                          Ident@32..37 "Point"
                    RParen@37..38 ")"
                    Whitespace@38..39 " "
                    Block@39..42
                      LBrace@39..40 "{"
                      Whitespace@40..41 " "
                      RBrace@41..42 "}""#]],
        ),
      }
}
