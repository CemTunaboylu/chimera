use crate::{
    marker::{Incomplete, Marker},
    operator::starting_precedence,
    parse::{Finished, SeparatedElement},
    parser::Parser,
};
use syntax::{
    Syntax, anchor::RollingBackAnchor, bitset::SyntaxKindBitSet, syntax_kind::SyntaxKind,
};

use SyntaxKind::*;
use thin_vec::{ThinVec, thin_vec};

impl<'input> Parser<'input> {
    // fn <ident>({parameters as CSV}) {-> RetType} {}
    #[allow(unused_variables)]
    pub fn parse_function_def(&self) -> Option<Finished> {
        let marker = self.start();
        self.expect_and_bump(KwFn);
        {
            let rollback_after_drop = self.roll_back_context_after_drop();
            self.dont_recover_in_ctx(LParen);
            self.expect_and_bump(Ident);
        }
        self.expect_and_bump(LParen);
        if !self.is_next(RParen) {
            let rollback_after_drop = self.roll_back_context_after_drop();
            self.dont_recover_in_ctx(RParen);
            self.expect_in_ctx(FnDef);
            self.parse_comma_separated_typed_declarations_until(|syntax: Syntax| {
                !syntax.is_of_kind(RParen)
            });
        }

        let rollback_after_drop = self.roll_back_context_after_drop();
        self.dont_recover_in_ctx(LBrace);
        self.expect_and_bump(RParen);

        if self.is_next(RArrow) {
            let ret_type_marker = self.start();
            let rollback_after_drop = self.roll_back_context_after_drop();
            self.expect_and_bump(RArrow);
            self.allow_only_in_ctx(SyntaxKind::types().as_ref());
            self.allow_in_ctx([And, Mut].as_ref());
            self.expect_in_ctx(StructAsType);

            if self.is_next(And) {
                self.parse_prefix_unary_operation(And);
            } else {
                self.parse_left_hand_side();
            }
            self.complete_marker_with(ret_type_marker, RetType);
        }

        self.parse_block();
        Some(self.complete_marker_with(marker, SyntaxKind::FnDef))
    }

    // the identifier is already bumped at this point because we ended up here
    // from parsing an identifier and saw a LParen as well.
    #[allow(unused_variables)]
    pub fn parse_function_call(&self, marker: Marker<Incomplete>) -> Finished {
        self.expect_and_bump(LParen);
        let rollback_when_dropped = self.roll_back_context_after_drop();
        self.expect_in_ctx(SyntaxKind::operators());
        self.parse_comma_separated_arguments_until(RParen);
        self.expect_and_bump(RParen);
        self.complete_marker_with(marker, FnCall)
    }

    fn impose_comma_sep_typed_decl_restrictions(&self) -> RollingBackAnchor {
        let rollback_when_dropped = self.roll_back_context_after_drop();
        let ctx = self.context.borrow();
        ctx.forbid_all();
        ctx.allow([And, Colon, Comma, Ident, KwMut, RParen].as_ref());
        ctx.allow(SyntaxKind::types().as_ref());
        rollback_when_dropped
    }

    #[allow(unused_variables)]
    pub fn parse_comma_separated_typed_declarations_until(&self, until_false: fn(Syntax) -> bool) {
        let rollback_when_dropped = self.impose_comma_sep_typed_decl_restrictions();
        use SeparatedElement::*;

        let ref_mut_with = |s: SeparatedElement| RefMut(thin_vec![s]);
        let types_set: SyntaxKindBitSet = SyntaxKind::types().into();
        let arg_elements = thin_vec![
            Kind(Ident),
            Kind(Colon),
            ref_mut_with(Branched(
                thin_vec![KindAs(Ident, StructAsType)],
                // thin_vec![InSet(types_set)],
                thin_vec![ParseExprWith(starting_precedence())],
            ))
        ];
        let can_be_a_method = thin_vec![Branched(
            thin_vec![ref_mut_with(KindWithMarker(Kwself, SelfRef))],
            arg_elements.clone()
        )];

        self.parse_arg(&can_be_a_method);

        while let Some(Ok(peeked)) = self.peek() {
            if !until_false(peeked.clone()) {
                break;
            }
            self.parse_arg(&arg_elements);
        }
    }

    fn parse_arg(&self, elements: &ThinVec<SeparatedElement>) {
        let marker = self.start();
        self.parse_possible_ref_mut_arg_and_elms(elements);
        // since we stopped at Comma above, we need to consume Comma if there is one now to enable the following parse
        self.ignore_if(Comma);
        self.complete_marker_with(marker, ParamDecl);
    }

    pub fn parse_possible_ref_mut_arg_and_elms(&self, elements: &ThinVec<SeparatedElement>) {
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
    }

    fn impose_comma_sep_args_restrictions(&self) -> RollingBackAnchor {
        let rollback_when_dropped = self.roll_back_context_after_drop();
        let ctx = self.context.borrow();
        ctx.allow(RParen);
        ctx.expect(FnCall);
        rollback_when_dropped
    }

    #[allow(unused_variables)]
    pub fn parse_comma_separated_arguments_until(
        &self,
        unwanted: impl Into<SyntaxKindBitSet>,
    ) -> Option<()> {
        let rollback_when_dropped = self.impose_comma_sep_args_restrictions();
        use SeparatedElement::*;
        let ref_mut_with = |s: SeparatedElement| RefMut(thin_vec![s]);
        let can_be = thin_vec![ref_mut_with(ParseExprWith(starting_precedence()))];

        self.parse_separated_by(&can_be, FnArg, Comma, unwanted)
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
              ParamDecl@0..8
                Ident@0..2 "me"
                Colon@2..3 ":"
                StructAsType@3..8 "human"
              Whitespace@8..9 " "
              ParamDecl@9..23
                Ident@9..13 "lang"
                Colon@13..14 ":"
                Whitespace@14..15 " "
                StructAsType@15..23 "Language"
              Whitespace@23..24 " "
              ParamDecl@24..32
                Ident@24..27 "pet"
                Colon@27..28 ":"
                Whitespace@28..29 " "
                StructAsType@29..32 "Cat""#]];

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
                ParamDecl@9..18
                  Ident@9..14 "first"
                  Colon@14..15 ":"
                  TyI32@15..18 "i32"
                Whitespace@18..19 " "
                ParamDecl@19..30
                  Ident@19..25 "second"
                  Colon@25..26 ":"
                  TyChar@26..30 "char"
                Whitespace@30..31 " "
                ParamDecl@31..47
                  Ident@31..36 "third"
                  Colon@36..37 ":"
                  Whitespace@37..38 " "
                  StructAsType@38..47 "Structure"
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
                      ParamDecl@9..18
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

    fn_with_return: ("fn sum(a:tensor, b:tensor) -> tensor{ return a+b; }",
              expect![[r#"
                  Root@0..50
                    FnDef@0..50
                      KwFn@0..2 "fn"
                      Whitespace@2..3 " "
                      Ident@3..6 "sum"
                      LParen@6..7 "("
                      ParamDecl@7..15
                        Ident@7..8 "a"
                        Colon@8..9 ":"
                        TensorType@9..15
                          TyTensor@9..15 "tensor"
                      Whitespace@15..16 " "
                      ParamDecl@16..24
                        Ident@16..17 "b"
                        Colon@17..18 ":"
                        TensorType@18..24
                          TyTensor@18..24 "tensor"
                      RParen@24..25 ")"
                      Whitespace@25..26 " "
                      RetType@26..35
                        RArrow@26..28 "->"
                        Whitespace@28..29 " "
                        TensorType@29..35
                          TyTensor@29..35 "tensor"
                      Block@35..50
                        LBrace@35..36 "{"
                        Whitespace@36..37 " "
                        Return@37..48
                          KwReturn@37..43 "return"
                          Whitespace@43..44 " "
                          InfixBinOp@44..47
                            VarRef@44..45
                              Ident@44..45 "a"
                            Plus@45..46 "+"
                            VarRef@46..47
                              Ident@46..47 "b"
                          Semi@47..48 ";"
                        Whitespace@48..49 " "
                        RBrace@49..50 "}""#]],
    ),

        fn_with_ref_mut_self: ("fn translate(&mut self, by: Point) { self.x += by.x; self.y += by.y;}",
            expect![[r#"
                Root@0..68
                  FnDef@0..68
                    KwFn@0..2 "fn"
                    Whitespace@2..3 " "
                    Ident@3..12 "translate"
                    LParen@12..13 "("
                    ParamDecl@13..22
                      PrefixUnaryOp@13..22
                        And@13..14 "&"
                        Mut@14..22
                          KwMut@14..17 "mut"
                          Whitespace@17..18 " "
                          SelfRef@18..22
                            Kwself@18..22 "self"
                    Whitespace@22..23 " "
                    ParamDecl@23..32
                      Ident@23..25 "by"
                      Colon@25..26 ":"
                      Whitespace@26..27 " "
                      StructAsType@27..32 "Point"
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
                    ParamDecl@13..22
                      PrefixUnaryOp@13..22
                        And@13..14 "&"
                        Mut@14..22
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
                          StructAsType@32..37 "Point"
                    RParen@37..38 ")"
                    Whitespace@38..39 " "
                    Block@39..42
                      LBrace@39..40 "{"
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

    function_call_with_ref_mut_parameter: ("steal(&mut me.mine)",
        expect![[r#"
            Root@0..19
              FnCall@0..19
                Ident@0..5 "steal"
                LParen@5..6 "("
                FnArg@6..18
                  PrefixUnaryOp@6..18
                    And@6..7 "&"
                    Mut@7..18
                      KwMut@7..10 "mut"
                      Whitespace@10..11 " "
                      InfixBinOp@11..18
                        VarRef@11..13
                          Ident@11..13 "me"
                        Dot@13..14 "."
                        VarRef@14..18
                          Ident@14..18 "mine"
                RParen@18..19 ")""#]],
    ),

    function_call_with_ref_parameters: ("meet(&me, &him, &her)",
        expect![[r#"
            Root@0..19
              FnCall@0..19
                Ident@0..4 "meet"
                LParen@4..5 "("
                FnArg@5..8
                  PrefixUnaryOp@5..8
                    And@5..6 "&"
                    VarRef@6..8
                      Ident@6..8 "me"
                Whitespace@8..9 " "
                FnArg@9..13
                  PrefixUnaryOp@9..13
                    And@9..10 "&"
                    VarRef@10..13
                      Ident@10..13 "him"
                Whitespace@13..14 " "
                FnArg@14..18
                  PrefixUnaryOp@14..18
                    And@14..15 "&"
                    VarRef@15..18
                      Ident@15..18 "her"
                RParen@18..19 ")""#]],
    ),

    function_call_with_ref_mut_parameters: ("bond(&mut me, &mut him, &mut her)",
        expect![[r#"
            Root@0..31
              FnCall@0..31
                Ident@0..4 "bond"
                LParen@4..5 "("
                FnArg@5..12
                  PrefixUnaryOp@5..12
                    And@5..6 "&"
                    Mut@6..12
                      KwMut@6..9 "mut"
                      Whitespace@9..10 " "
                      VarRef@10..12
                        Ident@10..12 "me"
                Whitespace@12..13 " "
                FnArg@13..21
                  PrefixUnaryOp@13..21
                    And@13..14 "&"
                    Mut@14..21
                      KwMut@14..17 "mut"
                      Whitespace@17..18 " "
                      VarRef@18..21
                        Ident@18..21 "him"
                Whitespace@21..22 " "
                FnArg@22..30
                  PrefixUnaryOp@22..30
                    And@22..23 "&"
                    Mut@23..30
                      KwMut@23..26 "mut"
                      Whitespace@26..27 " "
                      VarRef@27..30
                        Ident@27..30 "her"
                RParen@30..31 ")""#]],
    ),

    function_call_with_complex_parameters: ("am_i_happy(me.expectations().as_tensor() - reality.variable_tensor )",
        expect![[r#"
            Root@0..68
              FnCall@0..68
                Ident@0..10 "am_i_happy"
                LParen@10..11 "("
                FnArg@11..67
                  InfixBinOp@11..67
                    InfixBinOp@11..41
                      InfixBinOp@11..28
                        VarRef@11..13
                          Ident@11..13 "me"
                        Dot@13..14 "."
                        FnCall@14..28
                          Ident@14..26 "expectations"
                          LParen@26..27 "("
                          RParen@27..28 ")"
                      Dot@28..29 "."
                      FnCall@29..40
                        Ident@29..38 "as_tensor"
                        LParen@38..39 "("
                        RParen@39..40 ")"
                      Whitespace@40..41 " "
                    Minus@41..42 "-"
                    Whitespace@42..43 " "
                    InfixBinOp@43..67
                      VarRef@43..50
                        Ident@43..50 "reality"
                      Dot@50..51 "."
                      VarRef@51..67
                        Ident@51..66 "variable_tensor"
                        Whitespace@66..67 " "
                RParen@67..68 ")""#]],
    ),
    static_function_call: ("Life::diff(me.expectations().as_tensor())",
        expect![[r#"
            Root@0..41
              InfixBinOp@0..41
                VarRef@0..4
                  Ident@0..4 "Life"
                ColonColon@4..6 "::"
                FnCall@6..41
                  Ident@6..10 "diff"
                  LParen@10..11 "("
                  FnArg@11..40
                    InfixBinOp@11..40
                      InfixBinOp@11..28
                        VarRef@11..13
                          Ident@11..13 "me"
                        Dot@13..14 "."
                        FnCall@14..28
                          Ident@14..26 "expectations"
                          LParen@26..27 "("
                          RParen@27..28 ")"
                      Dot@28..29 "."
                      FnCall@29..40
                        Ident@29..38 "as_tensor"
                        LParen@38..39 "("
                        RParen@39..40 ")"
                  RParen@40..41 ")""#]],
    ),
    }
}
