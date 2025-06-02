use crate::{
    marker::{Incomplete, Marker},
    operator::starting_precedence,
    parse::{Element, Finished},
    parser::{IsNext, Parser},
};
use syntax::{Syntax, bitset::SyntaxKindBitSet, syntax_kind::SyntaxKind};

use SyntaxKind::*;
use thin_vec::{ThinVec, thin_vec};

impl Parser<'_> {
    // fn <ident>({parameters as CSV}) {-> RetType} {}
    #[allow(unused_variables)]
    pub fn parse_function_def(&self) -> Option<Finished> {
        let marker = self.start();
        let rollback_after_drop = self.parsing(KwFn);
        self.expect_and_bump(KwFn);
        {
            let rollback_after_drop = self.impose_restrictions_of_kind_on_context(Ident);
            self.expect_and_bump(Ident);
        }
        self.expect_and_bump(LParen);
        if !self.is_next(RParen) {
            let rollback_after_drop = self.impose_restrictions_of_kind_on_context(RParen);
            self.parse_comma_separated_typed_declarations_until(|syntax: Syntax| {
                !syntax.is_of_kind(RParen)
            });
        }

        let rollback_after_drop = self.impose_restrictions_of_kind_on_context(LBrace);
        self.expect_and_bump(RParen);

        self.parse_return_type_if_any();

        self.parse_block();
        Some(self.complete_marker_with(marker, SyntaxKind::FnDef))
    }

    #[allow(unused_variables)]
    fn parse_return_type_if_any(&self) {
        if self.is_next(RArrow) {
            let ret_type_marker = self.start();
            self.expect_and_bump(RArrow);
            let rollback_after_drop = self.impose_context_for_parsing(RetType);

            if self.is_next(And) {
                self.parse_prefix_unary_operation(And);
            } else {
                self.parse_left_hand_side();
            }
            self.complete_marker_with(ret_type_marker, RetType);
        }
    }

    // let <ident> = |{parameters as CSV}| {-> RetType} {}
    #[allow(unused_variables)]
    pub fn parse_lambda_def(&self) -> Option<Finished> {
        let marker = self.start();
        let rollback_after_drop = self.impose_context_for_parsing(Lambda);

        if self.is_next(OrOr) {
            self.expect_and_bump(OrOr);
        } else {
            self.expect_and_bump(Or);
            if !self.is_next(Or) {
                let rollback_after_drop = self.roll_back_context_after_drop();
                self.dont_recover_in_ctx(Or);
                self.parse_comma_separated_declarations_with_optional_type_declarations_until(
                    |syntax: Syntax| !syntax.is_of_kind(Or),
                );
            }
            self.expect_and_bump(Or);
        }

        self.parse_return_type_if_any();

        self.parse_block();
        let lambda_marker = self.complete_marker_with(marker, SyntaxKind::Lambda);
        let wrapping_in_literal = self.precede_marker_with(&lambda_marker);
        let mut m = self.complete_marker_with(wrapping_in_literal, SyntaxKind::Literal);
        // check if immediately calling the lambda
        if IsNext::Yes == self.is_next_strict(LParen) {
            let c = self.precede_marker_with(&m);
            m = self.parse_call(c);
        }
        Some(m)
    }

    #[allow(unused_variables)]
    pub fn parse_function_as_type(&self) -> Option<Finished> {
        let rollback_after_drop = self.parsing(TyFn);
        let marker = self.start();
        self.expect_and_bump(KwFn);
        {
            let rollback_after_drop = self.impose_restrictions_of_kind_on_context(Ident);
            self.expect_and_bump(Ident);
        }
        self.expect_and_bump(LParen);
        if !self.is_next(RParen) {
            let rollback_after_drop = self.impose_restrictions_of_kind_on_context(RParen);
            self.parse_comma_separated_types_until(|syntax: Syntax| !syntax.is_of_kind(RParen));
        }

        self.expect_and_bump(RParen);

        if self.is_next(RArrow) {
            let ret_type_marker = self.start();
            self.expect_and_bump(RArrow);
            let rollback_after_drop = self.impose_context_for_parsing(RetType);

            if self.is_next(And) {
                self.parse_prefix_unary_operation(And);
            } else {
                self.parse_left_hand_side();
            }
            self.complete_marker_with(ret_type_marker, RetType);
        }

        Some(self.complete_marker_with(marker, SyntaxKind::TyFn))
    }

    // the identifier is already bumped at this point because we ended up here
    // from parsing an identifier and saw a LParen as well.
    #[allow(unused_variables)]
    pub fn parse_call(&self, marker: Marker<Incomplete>) -> Finished {
        self.expect_and_bump(LParen);
        let rollback_when_dropped = self.impose_context_for_parsing(Call);
        self.parse_comma_separated_arguments_until(RParen);
        self.expect_and_bump(RParen);
        self.complete_marker_with(marker, Call)
    }

    #[allow(unused_variables)]
    pub fn parse_comma_separated_types_until(&self, until_false: fn(Syntax) -> bool) {
        let rollback_when_dropped = self.impose_context_for_parsing(ParamDecl);
        use Element::*;

        let ref_mut_with = |s: Element| RefMut(thin_vec![s]);
        let arg_elements = thin_vec![ref_mut_with(Branched(
            thin_vec![KindAs(Ident, StructAsType)],
            thin_vec![ParseExprWith(starting_precedence())],
        ))];

        while let Some(Ok(peeked)) = self.peek() {
            if !until_false(peeked.clone()) {
                break;
            }
            self.parse_arg(&arg_elements);
        }
    }

    #[allow(unused_variables)]
    pub fn parse_comma_separated_typed_declarations_until(&self, until_false: fn(Syntax) -> bool) {
        let rollback_when_dropped = self.impose_context_for_parsing(ParamDecl);
        use Element::*;

        let ref_mut_with = |s: Element| RefMut(thin_vec![s]);
        let types_set: SyntaxKindBitSet = SyntaxKind::types().as_ref().into();
        let arg_elements = thin_vec![
            Kind(Ident),
            Kind(Colon),
            ref_mut_with(Branched(
                thin_vec![KindAs(Ident, StructAsType)],
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

    #[allow(unused_variables)]
    pub fn parse_comma_separated_declarations_with_optional_type_declarations_until(
        &self,
        until_false: fn(Syntax) -> bool,
    ) {
        let rollback_when_dropped = self.impose_context_for_parsing(ParamDecl);
        use Element::*;

        let ref_mut_with = |s: Element| RefMut(thin_vec![s]);
        let types_set: SyntaxKindBitSet = SyntaxKind::types().as_ref().into();
        let arg_elements = thin_vec![
            Kind(Ident),
            Branched(
                thin_vec![
                    Kind(Colon),
                    ref_mut_with(Branched(
                        thin_vec![KindAs(Ident, StructAsType)],
                        thin_vec![ParseExprWith(starting_precedence())],
                    ),)
                ],
                thin_vec![]
            ),
        ];

        while let Some(Ok(peeked)) = self.peek() {
            if !until_false(peeked.clone()) {
                break;
            }
            self.parse_arg(&arg_elements);
        }
    }

    fn parse_arg(&self, elements: &ThinVec<Element>) {
        let marker = self.start();
        self.parse_possible_ref_mut_arg_and_elms(elements);
        // since we stopped at Comma above, we need to consume Comma if there is one now to enable the following parse
        self.ignore_if(Comma);
        self.complete_marker_with(marker, ParamDecl);
    }

    pub fn parse_possible_ref_mut_arg_and_elms(&self, elements: &ThinVec<Element>) {
        if self.is_next(And) {
            self.parse_ref_arg(elements);
        } else if self.is_next(KwMut) {
            self.parse_mut_arg(elements);
        } else {
            self.parse_arg_with(elements);
        }
    }
    fn parse_ref_arg(&self, elements: &ThinVec<Element>) -> Finished {
        let marker = self.start();
        self.expect_and_bump(And);
        if self.is_next(KwMut) {
            self.parse_mut_arg(elements);
        } else {
            self.parse_arg_with(elements);
        }
        self.complete_marker_with(marker, PrefixUnaryOp)
    }

    fn parse_mut_arg(&self, elements: &ThinVec<Element>) -> Finished {
        let mut_marker = self.start();
        self.expect_and_bump(KwMut);
        self.parse_arg_with(elements);
        self.complete_marker_with(mut_marker, Mut)
    }

    fn parse_arg_with(&self, elements: &ThinVec<Element>) {
        self.parse_with(elements);
    }

    #[allow(unused_variables)]
    pub fn parse_comma_separated_arguments_until(&self, unwanted: impl Into<SyntaxKindBitSet>) {
        let rollback_when_dropped = self.impose_context_for_parsing(FnArg);
        use Element::*;
        let ref_mut_with = |s: Element| RefMut(thin_vec![s]);
        let can_be = thin_vec![ref_mut_with(ParseExprWith(starting_precedence()))];

        self.parse_separated_by(&can_be, FnArg, Comma, unwanted);
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

    lambda_with_no_parameters_not_handled: ("let empty = || {0}",
              expect![[r#"
                  Root@0..18
                    LetBinding@0..18
                      KwLet@0..3 "let"
                      Whitespace@3..4 " "
                      InfixBinOp@4..18
                        VarRef@4..10
                          Ident@4..9 "empty"
                          Whitespace@9..10 " "
                        Eq@10..11 "="
                        Whitespace@11..12 " "
                        Literal@12..18
                          Lambda@12..18
                            OrOr@12..14 "||"
                            Whitespace@14..15 " "
                            Block@15..18
                              LBrace@15..16 "{"
                              Literal@16..17
                                Int@16..17 "0"
                              RBrace@17..18 "}""#]],
    ),

        call_on_lambda_literal: ("let two = |a| {2*a}(1);",
              expect![[r#"
                  Root@0..23
                    LetBinding@0..23
                      KwLet@0..3 "let"
                      Whitespace@3..4 " "
                      InfixBinOp@4..22
                        VarRef@4..8
                          Ident@4..7 "two"
                          Whitespace@7..8 " "
                        Eq@8..9 "="
                        Whitespace@9..10 " "
                        Call@10..22
                          Literal@10..19
                            Lambda@10..19
                              Or@10..11 "|"
                              ParamDecl@11..12
                                Ident@11..12 "a"
                              Or@12..13 "|"
                              Whitespace@13..14 " "
                              Block@14..19
                                LBrace@14..15 "{"
                                InfixBinOp@15..18
                                  Literal@15..16
                                    Int@15..16 "2"
                                  Star@16..17 "*"
                                  VarRef@17..18
                                    Ident@17..18 "a"
                                RBrace@18..19 "}"
                          LParen@19..20 "("
                          FnArg@20..21
                            Literal@20..21
                              Int@20..21 "1"
                          RParen@21..22 ")"
                      Semi@22..23 ";""#]],
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

    lambda_with_single_typeless_parameter: ("let identity = |s| {s};",
        expect![[r#"
            Root@0..23
              LetBinding@0..23
                KwLet@0..3 "let"
                Whitespace@3..4 " "
                InfixBinOp@4..22
                  VarRef@4..13
                    Ident@4..12 "identity"
                    Whitespace@12..13 " "
                  Eq@13..14 "="
                  Whitespace@14..15 " "
                  Literal@15..22
                    Lambda@15..22
                      Or@15..16 "|"
                      ParamDecl@16..17
                        Ident@16..17 "s"
                      Or@17..18 "|"
                      Whitespace@18..19 " "
                      Block@19..22
                        LBrace@19..20 "{"
                        VarRef@20..21
                          Ident@20..21 "s"
                        RBrace@21..22 "}"
                Semi@22..23 ";""#]],
    ),

    lambda_with_single_type_hinted_parameter: ("let ident = |s:i32| {s};",
        expect![[r#"
            Root@0..24
              LetBinding@0..24
                KwLet@0..3 "let"
                Whitespace@3..4 " "
                InfixBinOp@4..23
                  VarRef@4..10
                    Ident@4..9 "ident"
                    Whitespace@9..10 " "
                  Eq@10..11 "="
                  Whitespace@11..12 " "
                  Literal@12..23
                    Lambda@12..23
                      Or@12..13 "|"
                      ParamDecl@13..18
                        Ident@13..14 "s"
                        Colon@14..15 ":"
                        TyI32@15..18 "i32"
                      Or@18..19 "|"
                      Whitespace@19..20 " "
                      Block@20..23
                        LBrace@20..21 "{"
                        VarRef@21..22
                          Ident@21..22 "s"
                        RBrace@22..23 "}"
                Semi@23..24 ";""#]],
    ),
    lambda_with_tuple_type_hinted_parameters: ("let ident = |s:(i32, char)| {s};",
        expect![[r#"
            Root@0..32
              LetBinding@0..32
                KwLet@0..3 "let"
                Whitespace@3..4 " "
                InfixBinOp@4..31
                  VarRef@4..10
                    Ident@4..9 "ident"
                    Whitespace@9..10 " "
                  Eq@10..11 "="
                  Whitespace@11..12 " "
                  Literal@12..31
                    Lambda@12..31
                      Or@12..13 "|"
                      ParamDecl@13..26
                        Ident@13..14 "s"
                        Colon@14..15 ":"
                        Tuple@15..26
                          LParen@15..16 "("
                          TyI32@16..19 "i32"
                          Comma@19..20 ","
                          Whitespace@20..21 " "
                          TyChar@21..25 "char"
                          RParen@25..26 ")"
                      Or@26..27 "|"
                      Whitespace@27..28 " "
                      Block@28..31
                        LBrace@28..29 "{"
                        VarRef@29..30
                          Ident@29..30 "s"
                        RBrace@30..31 "}"
                Semi@31..32 ";""#]],
    ),

    function_def_with_fn_parameter: ("fn apply(f:fn(tensor)->tensor) {}",
        expect![[r#"
            Root@0..33
              FnDef@0..33
                KwFn@0..2 "fn"
                Whitespace@2..3 " "
                Ident@3..8 "apply"
                LParen@8..9 "("
                ParamDecl@9..29
                  Ident@9..10 "f"
                  Colon@10..11 ":"
                  TyFn@11..29
                    KwFn@11..13 "fn"
                    LParen@13..14 "("
                    ParamDecl@14..20
                      TyTensor@14..20
                        KwTensor@14..20 "tensor"
                    RParen@20..21 ")"
                    RetType@21..29
                      RArrow@21..23 "->"
                      TyTensor@23..29
                        KwTensor@23..29 "tensor"
                RParen@29..30 ")"
                Whitespace@30..31 " "
                Block@31..33
                  LBrace@31..32 "{"
                  RBrace@32..33 "}""#]],
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
    function_def_with_tuple_parameter: ("fn empty(first:(i32, char, Structure), third: Structure) {}",
        expect![[r#"
            Root@0..58
              FnDef@0..58
                KwFn@0..2 "fn"
                Whitespace@2..3 " "
                Ident@3..8 "empty"
                LParen@8..9 "("
                ParamDecl@9..37
                  Ident@9..14 "first"
                  Colon@14..15 ":"
                  Tuple@15..37
                    LParen@15..16 "("
                    TyI32@16..19 "i32"
                    Comma@19..20 ","
                    Whitespace@20..21 " "
                    TyChar@21..25 "char"
                    Comma@25..26 ","
                    Whitespace@26..27 " "
                    StructAsType@27..36 "Structure"
                    RParen@36..37 ")"
                Whitespace@37..38 " "
                ParamDecl@38..54
                  Ident@38..43 "third"
                  Colon@43..44 ":"
                  Whitespace@44..45 " "
                  StructAsType@45..54 "Structure"
                RParen@54..55 ")"
                Whitespace@55..56 " "
                Block@56..58
                  LBrace@56..57 "{"
                  RBrace@57..58 "}""#]],
    ),
    function_def_with_multiple_parameters_returning_a_tuple: ("fn empty(first:i32, second:char, third: Structure) -> (i32, char, Structure) {}",
        expect![[r#"
            Root@0..77
              FnDef@0..77
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
                RetType@49..74
                  RArrow@49..51 "->"
                  Whitespace@51..52 " "
                  Tuple@52..74
                    LParen@52..53 "("
                    TyI32@53..56 "i32"
                    Comma@56..57 ","
                    Whitespace@57..58 " "
                    TyChar@58..62 "char"
                    Comma@62..63 ","
                    Whitespace@63..64 " "
                    StructAsType@64..73 "Structure"
                    RParen@73..74 ")"
                Block@74..77
                  Whitespace@74..75 " "
                  LBrace@75..76 "{"
                  RBrace@76..77 "}""#]],
    ),
    lambda_with_multiple_type_hinted_parameters: ("let tri = |first:i32, second:char, third: Structure| { third.juggle(first, second) }",
        expect![[r#"
            Root@0..82
              LetBinding@0..82
                KwLet@0..3 "let"
                Whitespace@3..4 " "
                InfixBinOp@4..82
                  VarRef@4..8
                    Ident@4..7 "tri"
                    Whitespace@7..8 " "
                  Eq@8..9 "="
                  Whitespace@9..10 " "
                  Literal@10..82
                    Lambda@10..82
                      Or@10..11 "|"
                      ParamDecl@11..20
                        Ident@11..16 "first"
                        Colon@16..17 ":"
                        TyI32@17..20 "i32"
                      Whitespace@20..21 " "
                      ParamDecl@21..32
                        Ident@21..27 "second"
                        Colon@27..28 ":"
                        TyChar@28..32 "char"
                      Whitespace@32..33 " "
                      ParamDecl@33..49
                        Ident@33..38 "third"
                        Colon@38..39 ":"
                        Whitespace@39..40 " "
                        StructAsType@40..49 "Structure"
                      Or@49..50 "|"
                      Whitespace@50..51 " "
                      Block@51..82
                        LBrace@51..52 "{"
                        Whitespace@52..53 " "
                        InfixBinOp@53..81
                          VarRef@53..58
                            Ident@53..58 "third"
                          Dot@58..59 "."
                          Call@59..80
                            Ident@59..65 "juggle"
                            LParen@65..66 "("
                            FnArg@66..72
                              VarRef@66..71
                                Ident@66..71 "first"
                              Comma@71..72 ","
                            Whitespace@72..73 " "
                            FnArg@73..79
                              VarRef@73..79
                                Ident@73..79 "second"
                            RParen@79..80 ")"
                          Whitespace@80..81 " "
                        RBrace@81..82 "}""#]],
    ),

    lambda_with_multiple_type_hinted_parameters_returning_a_tuple: ("let tri = |first:i32, second:char, third: Structure| -> (i32, char, Structure) { third.juggle(first, second) }",
        expect![[r#"
            Root@0..108
              LetBinding@0..108
                KwLet@0..3 "let"
                Whitespace@3..4 " "
                InfixBinOp@4..108
                  VarRef@4..8
                    Ident@4..7 "tri"
                    Whitespace@7..8 " "
                  Eq@8..9 "="
                  Whitespace@9..10 " "
                  Literal@10..108
                    Lambda@10..108
                      Or@10..11 "|"
                      ParamDecl@11..20
                        Ident@11..16 "first"
                        Colon@16..17 ":"
                        TyI32@17..20 "i32"
                      Whitespace@20..21 " "
                      ParamDecl@21..32
                        Ident@21..27 "second"
                        Colon@27..28 ":"
                        TyChar@28..32 "char"
                      Whitespace@32..33 " "
                      ParamDecl@33..49
                        Ident@33..38 "third"
                        Colon@38..39 ":"
                        Whitespace@39..40 " "
                        StructAsType@40..49 "Structure"
                      Or@49..50 "|"
                      Whitespace@50..51 " "
                      RetType@51..76
                        RArrow@51..53 "->"
                        Whitespace@53..54 " "
                        Tuple@54..76
                          LParen@54..55 "("
                          TyI32@55..58 "i32"
                          Comma@58..59 ","
                          Whitespace@59..60 " "
                          TyChar@60..64 "char"
                          Comma@64..65 ","
                          Whitespace@65..66 " "
                          StructAsType@66..75 "Structure"
                          RParen@75..76 ")"
                      Block@76..108
                        Whitespace@76..77 " "
                        LBrace@77..78 "{"
                        Whitespace@78..79 " "
                        InfixBinOp@79..107
                          VarRef@79..84
                            Ident@79..84 "third"
                          Dot@84..85 "."
                          Call@85..106
                            Ident@85..91 "juggle"
                            LParen@91..92 "("
                            FnArg@92..98
                              VarRef@92..97
                                Ident@92..97 "first"
                              Comma@97..98 ","
                            Whitespace@98..99 " "
                            FnArg@99..105
                              VarRef@99..105
                                Ident@99..105 "second"
                            RParen@105..106 ")"
                          Whitespace@106..107 " "
                        RBrace@107..108 "}""#]],
    ),

    lambda_with_mixed_parameters: ("let tri = |typeless, primitive:char, structure: Structure, another_typles| { structure.juggle(typeless, primitive,another_typles) }",
        expect![[r#"
            Root@0..128
              LetBinding@0..128
                KwLet@0..3 "let"
                Whitespace@3..4 " "
                InfixBinOp@4..128
                  VarRef@4..8
                    Ident@4..7 "tri"
                    Whitespace@7..8 " "
                  Eq@8..9 "="
                  Whitespace@9..10 " "
                  Literal@10..128
                    Lambda@10..128
                      Or@10..11 "|"
                      ParamDecl@11..19
                        Ident@11..19 "typeless"
                      Whitespace@19..20 " "
                      ParamDecl@20..34
                        Ident@20..29 "primitive"
                        Colon@29..30 ":"
                        TyChar@30..34 "char"
                      Whitespace@34..35 " "
                      ParamDecl@35..55
                        Ident@35..44 "structure"
                        Colon@44..45 ":"
                        Whitespace@45..46 " "
                        StructAsType@46..55 "Structure"
                      Whitespace@55..56 " "
                      ParamDecl@56..70
                        Ident@56..70 "another_typles"
                      Or@70..71 "|"
                      Whitespace@71..72 " "
                      Block@72..128
                        LBrace@72..73 "{"
                        Whitespace@73..74 " "
                        InfixBinOp@74..127
                          VarRef@74..83
                            Ident@74..83 "structure"
                          Dot@83..84 "."
                          Call@84..126
                            Ident@84..90 "juggle"
                            LParen@90..91 "("
                            FnArg@91..100
                              VarRef@91..99
                                Ident@91..99 "typeless"
                              Comma@99..100 ","
                            Whitespace@100..101 " "
                            FnArg@101..111
                              VarRef@101..110
                                Ident@101..110 "primitive"
                              Comma@110..111 ","
                            FnArg@111..125
                              VarRef@111..125
                                Ident@111..125 "another_typles"
                            RParen@125..126 ")"
                          Whitespace@126..127 " "
                        RBrace@127..128 "}""#]],
    ),

    lambda_returnin_another_lambda: ("let inception = |typeless| { |t| {t.steal_type(typeless)} }",
        expect![[r#"
            Root@0..59
              LetBinding@0..59
                KwLet@0..3 "let"
                Whitespace@3..4 " "
                InfixBinOp@4..59
                  VarRef@4..14
                    Ident@4..13 "inception"
                    Whitespace@13..14 " "
                  Eq@14..15 "="
                  Whitespace@15..16 " "
                  Literal@16..59
                    Lambda@16..59
                      Or@16..17 "|"
                      ParamDecl@17..25
                        Ident@17..25 "typeless"
                      Or@25..26 "|"
                      Whitespace@26..27 " "
                      Block@27..59
                        LBrace@27..28 "{"
                        Whitespace@28..29 " "
                        Literal@29..57
                          Lambda@29..57
                            Or@29..30 "|"
                            ParamDecl@30..31
                              Ident@30..31 "t"
                            Or@31..32 "|"
                            Whitespace@32..33 " "
                            Block@33..57
                              LBrace@33..34 "{"
                              InfixBinOp@34..56
                                VarRef@34..35
                                  Ident@34..35 "t"
                                Dot@35..36 "."
                                Call@36..56
                                  Ident@36..46 "steal_type"
                                  LParen@46..47 "("
                                  FnArg@47..55
                                    VarRef@47..55
                                      Ident@47..55 "typeless"
                                  RParen@55..56 ")"
                              RBrace@56..57 "}"
                        Whitespace@57..58 " "
                        RBrace@58..59 "}""#]],
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
                        LetBinding@41..68
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
                        LetBinding@69..94
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
                        TyTensor@9..15
                          KwTensor@9..15 "tensor"
                      Whitespace@15..16 " "
                      ParamDecl@16..24
                        Ident@16..17 "b"
                        Colon@17..18 ":"
                        TyTensor@18..24
                          KwTensor@18..24 "tensor"
                      RParen@24..25 ")"
                      Whitespace@25..26 " "
                      RetType@26..35
                        RArrow@26..28 "->"
                        Whitespace@28..29 " "
                        TyTensor@29..35
                          KwTensor@29..35 "tensor"
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

    lambda_with_ref_mut_parameter: ("let like_a_method = |s:&mut Structure| {s.mutate()};",
        expect![[r#"
            Root@0..52
              LetBinding@0..52
                KwLet@0..3 "let"
                Whitespace@3..4 " "
                InfixBinOp@4..51
                  VarRef@4..18
                    Ident@4..17 "like_a_method"
                    Whitespace@17..18 " "
                  Eq@18..19 "="
                  Whitespace@19..20 " "
                  Literal@20..51
                    Lambda@20..51
                      Or@20..21 "|"
                      ParamDecl@21..37
                        Ident@21..22 "s"
                        Colon@22..23 ":"
                        PrefixUnaryOp@23..37
                          And@23..24 "&"
                          Mut@24..37
                            KwMut@24..27 "mut"
                            Whitespace@27..28 " "
                            StructAsType@28..37 "Structure"
                      Or@37..38 "|"
                      Whitespace@38..39 " "
                      Block@39..51
                        LBrace@39..40 "{"
                        InfixBinOp@40..50
                          VarRef@40..41
                            Ident@40..41 "s"
                          Dot@41..42 "."
                          Call@42..50
                            Ident@42..48 "mutate"
                            LParen@48..49 "("
                            RParen@49..50 ")"
                        RBrace@50..51 "}"
                Semi@51..52 ";""#]],
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
              Call@0..13
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
              Call@0..19
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
            Root@0..21
              Call@0..21
                Ident@0..4 "meet"
                LParen@4..5 "("
                FnArg@5..9
                  PrefixUnaryOp@5..8
                    And@5..6 "&"
                    VarRef@6..8
                      Ident@6..8 "me"
                  Comma@8..9 ","
                Whitespace@9..10 " "
                FnArg@10..15
                  PrefixUnaryOp@10..14
                    And@10..11 "&"
                    VarRef@11..14
                      Ident@11..14 "him"
                  Comma@14..15 ","
                Whitespace@15..16 " "
                FnArg@16..20
                  PrefixUnaryOp@16..20
                    And@16..17 "&"
                    VarRef@17..20
                      Ident@17..20 "her"
                RParen@20..21 ")""#]],
    ),

    function_call_with_ref_mut_parameters: ("bond(&mut me, &mut him, &mut her)",
        expect![[r#"
            Root@0..33
              Call@0..33
                Ident@0..4 "bond"
                LParen@4..5 "("
                FnArg@5..13
                  PrefixUnaryOp@5..12
                    And@5..6 "&"
                    Mut@6..12
                      KwMut@6..9 "mut"
                      Whitespace@9..10 " "
                      VarRef@10..12
                        Ident@10..12 "me"
                  Comma@12..13 ","
                Whitespace@13..14 " "
                FnArg@14..23
                  PrefixUnaryOp@14..22
                    And@14..15 "&"
                    Mut@15..22
                      KwMut@15..18 "mut"
                      Whitespace@18..19 " "
                      VarRef@19..22
                        Ident@19..22 "him"
                  Comma@22..23 ","
                Whitespace@23..24 " "
                FnArg@24..32
                  PrefixUnaryOp@24..32
                    And@24..25 "&"
                    Mut@25..32
                      KwMut@25..28 "mut"
                      Whitespace@28..29 " "
                      VarRef@29..32
                        Ident@29..32 "her"
                RParen@32..33 ")""#]],
    ),

    function_call_with_complex_parameters: ("am_i_happy(me.expectations().as_tensor() - reality.variable_tensor )",
        expect![[r#"
            Root@0..68
              Call@0..68
                Ident@0..10 "am_i_happy"
                LParen@10..11 "("
                FnArg@11..67
                  InfixBinOp@11..67
                    InfixBinOp@11..41
                      InfixBinOp@11..28
                        VarRef@11..13
                          Ident@11..13 "me"
                        Dot@13..14 "."
                        Call@14..28
                          Ident@14..26 "expectations"
                          LParen@26..27 "("
                          RParen@27..28 ")"
                      Dot@28..29 "."
                      Call@29..40
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
                Call@6..41
                  Ident@6..10 "diff"
                  LParen@10..11 "("
                  FnArg@11..40
                    InfixBinOp@11..40
                      InfixBinOp@11..28
                        VarRef@11..13
                          Ident@11..13 "me"
                        Dot@13..14 "."
                        Call@14..28
                          Ident@14..26 "expectations"
                          LParen@26..27 "("
                          RParen@27..28 ")"
                      Dot@28..29 "."
                      Call@29..40
                        Ident@29..38 "as_tensor"
                        LParen@38..39 "("
                        RParen@39..40 ")"
                  RParen@40..41 ")""#]],
    ),
    }
}
