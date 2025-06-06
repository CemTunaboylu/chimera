use crate::{
    marker::{Incomplete, Marker},
    operator::starting_precedence,
    parse::{Element, Finished},
    parser::{IsNext, Parser},
};
use syntax::{bitset::SyntaxKindBitSet, syntax_kind::SyntaxKind};

use SyntaxKind::*;
use thin_vec::thin_vec;

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
            self.parse_comma_separated_typed_declarations_until(RParen);
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
            self.parse_expression_until_binding_power(starting_precedence());
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
                self.parse_comma_separated_typed_declarations_until(Or);
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
        self.expect_and_bump(LParen);
        if !self.is_next(RParen) {
            let rollback_after_drop = self.impose_restrictions_of_kind_on_context(RParen);
            self.parse_comma_separated_types_until(RParen);
        }

        self.expect_and_bump(RParen);
        self.parse_return_type_if_any();

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
    pub fn parse_comma_separated_types_until(&self, until: impl Into<SyntaxKindBitSet>) {
        let rollback_when_dropped = self.impose_context_for_parsing(ParamDecl);
        use Element::*;

        let arg_elements = thin_vec![ParseExprWith(starting_precedence())];
        self.bump_separated_by(&arg_elements, Comma, until);
    }

    #[allow(unused_variables)]
    fn try_to_parse_selfref_if_any(&self) {
        if !self.is_next_in([And, KwMut, SelfRef].as_ref().into()) {
            return;
        }
        let marker = self.start();
        if self.is_next(And) {
            let rollback_when_dropped = self.by_forbidding_all();
            self.allow_in_ctx([And, SelfRef, KwMut].as_ref());
            self.parse_prefix_unary_operation(And);
            self.complete_marker_with(marker, ParamDecl);
        } else if self.is_next(KwMut) {
            let rollback_when_dropped = self.by_forbidding_all();
            self.allow_in_ctx([SelfRef, KwMut].as_ref());
            self.parse_left_hand_side();
            self.complete_marker_with(marker, ParamDecl);
        } else if self.is_next(SelfRef) {
            self.expect_and_bump(SelfRef);
            self.complete_marker_with(marker, ParamDecl);
        }
    }

    #[allow(unused_variables)]
    pub fn parse_comma_separated_typed_declarations_until(
        &self,
        until: impl Into<SyntaxKindBitSet>,
    ) {
        let rollback_when_dropped = self.impose_context_for_parsing(ParamDecl);
        use Element::*;
        self.try_to_parse_selfref_if_any();

        let arg_elements = thin_vec![ParseExprWith(starting_precedence())];
        self.parse_separated_by(&arg_elements, ParamDecl, Comma, until);
    }

    #[allow(unused_variables)]
    pub fn parse_comma_separated_arguments_until(&self, unwanted: impl Into<SyntaxKindBitSet>) {
        let rollback_when_dropped = self.impose_context_for_parsing(FnArg);
        use Element::*;

        let arg_elements = thin_vec![ParseExprWith(starting_precedence())];
        self.parse_separated_by(&arg_elements, FnArg, Comma, unwanted);
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
        parser.parse_comma_separated_typed_declarations_until(RParen);
        parser.complete_marker_with(root, Root);
        let lexer = parser.lexer.borrow();
        let sink = Sink::new(parser.event_holder.take().into(), lexer.source());
        let cst = ConcreteSyntaxTree::from(sink);

        let expect = expect![[r#"
            Root@0..34
              ParamDecl@0..9
                TypeHint@0..8
                  VarRef@0..2
                    Ident@0..2 "me"
                  Colon@2..3 ":"
                  StructAsType@3..8 "human"
                Comma@8..9 ","
              Whitespace@9..10 " "
              ParamDecl@10..25
                TypeHint@10..24
                  VarRef@10..14
                    Ident@10..14 "lang"
                  Colon@14..15 ":"
                  Whitespace@15..16 " "
                  StructAsType@16..24 "Language"
                Comma@24..25 ","
              Whitespace@25..26 " "
              ParamDecl@26..34
                TypeHint@26..34
                  VarRef@26..29
                    Ident@26..29 "pet"
                  Colon@29..30 ":"
                  Whitespace@30..31 " "
                  StructAsType@31..34 "Cat""#]];

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
                                VarRef@11..12
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
                  TypeHint@9..19
                    VarRef@9..15
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
                        VarRef@16..17
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
                        TypeHint@13..18
                          VarRef@13..14
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
                        TypeHint@13..26
                          VarRef@13..14
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
                  TypeHint@9..29
                    VarRef@9..10
                      Ident@9..10 "f"
                    Colon@10..11 ":"
                    TyFn@11..29
                      KwFn@11..13 "fn"
                      LParen@13..14 "("
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
            Root@0..53
              FnDef@0..53
                KwFn@0..2 "fn"
                Whitespace@2..3 " "
                Ident@3..8 "empty"
                LParen@8..9 "("
                ParamDecl@9..19
                  TypeHint@9..18
                    VarRef@9..14
                      Ident@9..14 "first"
                    Colon@14..15 ":"
                    TyI32@15..18 "i32"
                  Comma@18..19 ","
                Whitespace@19..20 " "
                ParamDecl@20..32
                  TypeHint@20..31
                    VarRef@20..26
                      Ident@20..26 "second"
                    Colon@26..27 ":"
                    TyChar@27..31 "char"
                  Comma@31..32 ","
                Whitespace@32..33 " "
                ParamDecl@33..49
                  TypeHint@33..49
                    VarRef@33..38
                      Ident@33..38 "third"
                    Colon@38..39 ":"
                    Whitespace@39..40 " "
                    StructAsType@40..49 "Structure"
                RParen@49..50 ")"
                Whitespace@50..51 " "
                Block@51..53
                  LBrace@51..52 "{"
                  RBrace@52..53 "}""#]],
    ),
    function_def_with_tuple_parameter: ("fn empty(first:(i32, char, Structure), third: Structure) {}",
        expect![[r#"
            Root@0..59
              FnDef@0..59
                KwFn@0..2 "fn"
                Whitespace@2..3 " "
                Ident@3..8 "empty"
                LParen@8..9 "("
                ParamDecl@9..38
                  TypeHint@9..37
                    VarRef@9..14
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
                  Comma@37..38 ","
                Whitespace@38..39 " "
                ParamDecl@39..55
                  TypeHint@39..55
                    VarRef@39..44
                      Ident@39..44 "third"
                    Colon@44..45 ":"
                    Whitespace@45..46 " "
                    StructAsType@46..55 "Structure"
                RParen@55..56 ")"
                Whitespace@56..57 " "
                Block@57..59
                  LBrace@57..58 "{"
                  RBrace@58..59 "}""#]],
    ),
    function_def_with_multiple_parameters_returning_a_tuple: ("fn empty(first:i32, second:char, third: Structure) -> (i32, char, Structure) {}",
        expect![[r#"
            Root@0..79
              FnDef@0..79
                KwFn@0..2 "fn"
                Whitespace@2..3 " "
                Ident@3..8 "empty"
                LParen@8..9 "("
                ParamDecl@9..19
                  TypeHint@9..18
                    VarRef@9..14
                      Ident@9..14 "first"
                    Colon@14..15 ":"
                    TyI32@15..18 "i32"
                  Comma@18..19 ","
                Whitespace@19..20 " "
                ParamDecl@20..32
                  TypeHint@20..31
                    VarRef@20..26
                      Ident@20..26 "second"
                    Colon@26..27 ":"
                    TyChar@27..31 "char"
                  Comma@31..32 ","
                Whitespace@32..33 " "
                ParamDecl@33..49
                  TypeHint@33..49
                    VarRef@33..38
                      Ident@33..38 "third"
                    Colon@38..39 ":"
                    Whitespace@39..40 " "
                    StructAsType@40..49 "Structure"
                RParen@49..50 ")"
                Whitespace@50..51 " "
                RetType@51..77
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
                  Whitespace@76..77 " "
                Block@77..79
                  LBrace@77..78 "{"
                  RBrace@78..79 "}""#]],
    ),
    lambda_with_multiple_type_hinted_parameters: ("let tri = |first:i32, second:char, third: Structure| { third.juggle(first, second) }",
        expect![[r#"
            Root@0..84
              LetBinding@0..84
                KwLet@0..3 "let"
                Whitespace@3..4 " "
                InfixBinOp@4..84
                  VarRef@4..8
                    Ident@4..7 "tri"
                    Whitespace@7..8 " "
                  Eq@8..9 "="
                  Whitespace@9..10 " "
                  Literal@10..84
                    Lambda@10..84
                      Or@10..11 "|"
                      ParamDecl@11..21
                        TypeHint@11..20
                          VarRef@11..16
                            Ident@11..16 "first"
                          Colon@16..17 ":"
                          TyI32@17..20 "i32"
                        Comma@20..21 ","
                      Whitespace@21..22 " "
                      ParamDecl@22..34
                        TypeHint@22..33
                          VarRef@22..28
                            Ident@22..28 "second"
                          Colon@28..29 ":"
                          TyChar@29..33 "char"
                        Comma@33..34 ","
                      Whitespace@34..35 " "
                      ParamDecl@35..51
                        TypeHint@35..51
                          VarRef@35..40
                            Ident@35..40 "third"
                          Colon@40..41 ":"
                          Whitespace@41..42 " "
                          StructAsType@42..51 "Structure"
                      Or@51..52 "|"
                      Whitespace@52..53 " "
                      Block@53..84
                        LBrace@53..54 "{"
                        Whitespace@54..55 " "
                        InfixBinOp@55..83
                          VarRef@55..60
                            Ident@55..60 "third"
                          Dot@60..61 "."
                          Call@61..82
                            Ident@61..67 "juggle"
                            LParen@67..68 "("
                            FnArg@68..74
                              VarRef@68..73
                                Ident@68..73 "first"
                              Comma@73..74 ","
                            Whitespace@74..75 " "
                            FnArg@75..81
                              VarRef@75..81
                                Ident@75..81 "second"
                            RParen@81..82 ")"
                          Whitespace@82..83 " "
                        RBrace@83..84 "}""#]],
    ),

    lambda_with_multiple_type_hinted_parameters_returning_a_tuple: ("let tri = |first:i32, second:char, third: Structure| -> (i32, char, Structure) { third.juggle(first, second) }",
        expect![[r#"
            Root@0..110
              LetBinding@0..110
                KwLet@0..3 "let"
                Whitespace@3..4 " "
                InfixBinOp@4..110
                  VarRef@4..8
                    Ident@4..7 "tri"
                    Whitespace@7..8 " "
                  Eq@8..9 "="
                  Whitespace@9..10 " "
                  Literal@10..110
                    Lambda@10..110
                      Or@10..11 "|"
                      ParamDecl@11..21
                        TypeHint@11..20
                          VarRef@11..16
                            Ident@11..16 "first"
                          Colon@16..17 ":"
                          TyI32@17..20 "i32"
                        Comma@20..21 ","
                      Whitespace@21..22 " "
                      ParamDecl@22..34
                        TypeHint@22..33
                          VarRef@22..28
                            Ident@22..28 "second"
                          Colon@28..29 ":"
                          TyChar@29..33 "char"
                        Comma@33..34 ","
                      Whitespace@34..35 " "
                      ParamDecl@35..51
                        TypeHint@35..51
                          VarRef@35..40
                            Ident@35..40 "third"
                          Colon@40..41 ":"
                          Whitespace@41..42 " "
                          StructAsType@42..51 "Structure"
                      Or@51..52 "|"
                      Whitespace@52..53 " "
                      RetType@53..79
                        RArrow@53..55 "->"
                        Whitespace@55..56 " "
                        Tuple@56..78
                          LParen@56..57 "("
                          TyI32@57..60 "i32"
                          Comma@60..61 ","
                          Whitespace@61..62 " "
                          TyChar@62..66 "char"
                          Comma@66..67 ","
                          Whitespace@67..68 " "
                          StructAsType@68..77 "Structure"
                          RParen@77..78 ")"
                        Whitespace@78..79 " "
                      Block@79..110
                        LBrace@79..80 "{"
                        Whitespace@80..81 " "
                        InfixBinOp@81..109
                          VarRef@81..86
                            Ident@81..86 "third"
                          Dot@86..87 "."
                          Call@87..108
                            Ident@87..93 "juggle"
                            LParen@93..94 "("
                            FnArg@94..100
                              VarRef@94..99
                                Ident@94..99 "first"
                              Comma@99..100 ","
                            Whitespace@100..101 " "
                            FnArg@101..107
                              VarRef@101..107
                                Ident@101..107 "second"
                            RParen@107..108 ")"
                          Whitespace@108..109 " "
                        RBrace@109..110 "}""#]],
    ),

    lambda_with_mixed_parameters: ("let tri = |typeless, primitive:char, structure: Structure, another_typles| { structure.juggle(typeless, primitive,another_typles) }",
        expect![[r#"
            Root@0..131
              LetBinding@0..131
                KwLet@0..3 "let"
                Whitespace@3..4 " "
                InfixBinOp@4..131
                  VarRef@4..8
                    Ident@4..7 "tri"
                    Whitespace@7..8 " "
                  Eq@8..9 "="
                  Whitespace@9..10 " "
                  Literal@10..131
                    Lambda@10..131
                      Or@10..11 "|"
                      ParamDecl@11..20
                        VarRef@11..19
                          Ident@11..19 "typeless"
                        Comma@19..20 ","
                      Whitespace@20..21 " "
                      ParamDecl@21..36
                        TypeHint@21..35
                          VarRef@21..30
                            Ident@21..30 "primitive"
                          Colon@30..31 ":"
                          TyChar@31..35 "char"
                        Comma@35..36 ","
                      Whitespace@36..37 " "
                      ParamDecl@37..58
                        TypeHint@37..57
                          VarRef@37..46
                            Ident@37..46 "structure"
                          Colon@46..47 ":"
                          Whitespace@47..48 " "
                          StructAsType@48..57 "Structure"
                        Comma@57..58 ","
                      Whitespace@58..59 " "
                      ParamDecl@59..73
                        VarRef@59..73
                          Ident@59..73 "another_typles"
                      Or@73..74 "|"
                      Whitespace@74..75 " "
                      Block@75..131
                        LBrace@75..76 "{"
                        Whitespace@76..77 " "
                        InfixBinOp@77..130
                          VarRef@77..86
                            Ident@77..86 "structure"
                          Dot@86..87 "."
                          Call@87..129
                            Ident@87..93 "juggle"
                            LParen@93..94 "("
                            FnArg@94..103
                              VarRef@94..102
                                Ident@94..102 "typeless"
                              Comma@102..103 ","
                            Whitespace@103..104 " "
                            FnArg@104..114
                              VarRef@104..113
                                Ident@104..113 "primitive"
                              Comma@113..114 ","
                            FnArg@114..128
                              VarRef@114..128
                                Ident@114..128 "another_typles"
                            RParen@128..129 ")"
                          Whitespace@129..130 " "
                        RBrace@130..131 "}""#]],
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
                        VarRef@17..25
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
                              VarRef@30..31
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
                  Root@0..111
                    FnDef@0..111
                      KwFn@0..2 "fn"
                      Whitespace@2..3 " "
                      Ident@3..8 "empty"
                      LParen@8..9 "("
                      ParamDecl@9..19
                        TypeHint@9..18
                          VarRef@9..14
                            Ident@9..14 "first"
                          Colon@14..15 ":"
                          TyI32@15..18 "i32"
                        Comma@18..19 ","
                      Whitespace@19..20 " "
                      ParamDecl@20..31
                        TypeHint@20..31
                          VarRef@20..26
                            Ident@20..26 "second"
                          Colon@26..27 ":"
                          TyChar@27..31 "char"
                      RParen@31..32 ")"
                      Whitespace@32..33 " "
                      RetType@33..40
                        RArrow@33..35 "->"
                        Whitespace@35..36 " "
                        TyBool@36..40 "bool"
                      Block@40..111
                        Whitespace@40..41 " "
                        LBrace@41..42 "{"
                        LetBinding@42..69
                          KwLet@42..45 "let"
                          Whitespace@45..46 " "
                          InfixBinOp@46..68
                            VarRef@46..52
                              Ident@46..51 "sum_1"
                              Whitespace@51..52 " "
                            Eq@52..53 "="
                            Whitespace@53..54 " "
                            InfixBinOp@54..68
                              VarRef@54..60
                                Ident@54..59 "first"
                                Whitespace@59..60 " "
                              Plus@60..61 "+"
                              Whitespace@61..62 " "
                              VarRef@62..68
                                Ident@62..68 "second"
                          Semi@68..69 ";"
                        Whitespace@69..70 " "
                        LetBinding@70..95
                          KwLet@70..73 "let"
                          Whitespace@73..74 " "
                          InfixBinOp@74..94
                            VarRef@74..80
                              Ident@74..79 "sum_2"
                              Whitespace@79..80 " "
                            Eq@80..81 "="
                            Whitespace@81..82 " "
                            InfixBinOp@82..94
                              VarRef@82..88
                                Ident@82..88 "second"
                              Plus@88..89 "+"
                              VarRef@89..94
                                Ident@89..94 "first"
                          Semi@94..95 ";"
                        Whitespace@95..96 " "
                        InfixBinOp@96..110
                          VarRef@96..102
                            Ident@96..101 "first"
                            Whitespace@101..102 " "
                          EqEq@102..104 "=="
                          Whitespace@104..105 " "
                          VarRef@105..110
                            Ident@105..110 "check"
                        RBrace@110..111 "}""#]],
    ),

    fn_with_return: ("fn sum(a:tensor, b:tensor) -> tensor{ return a+b; }",
              expect![[r#"
                  Root@0..51
                    FnDef@0..51
                      KwFn@0..2 "fn"
                      Whitespace@2..3 " "
                      Ident@3..6 "sum"
                      LParen@6..7 "("
                      ParamDecl@7..16
                        TypeHint@7..15
                          VarRef@7..8
                            Ident@7..8 "a"
                          Colon@8..9 ":"
                          TyTensor@9..15
                            KwTensor@9..15 "tensor"
                        Comma@15..16 ","
                      Whitespace@16..17 " "
                      ParamDecl@17..25
                        TypeHint@17..25
                          VarRef@17..18
                            Ident@17..18 "b"
                          Colon@18..19 ":"
                          TyTensor@19..25
                            KwTensor@19..25 "tensor"
                      RParen@25..26 ")"
                      Whitespace@26..27 " "
                      RetType@27..36
                        RArrow@27..29 "->"
                        Whitespace@29..30 " "
                        TyTensor@30..36
                          KwTensor@30..36 "tensor"
                      Block@36..51
                        LBrace@36..37 "{"
                        Whitespace@37..38 " "
                        Return@38..49
                          KwReturn@38..44 "return"
                          Whitespace@44..45 " "
                          InfixBinOp@45..48
                            VarRef@45..46
                              Ident@45..46 "a"
                            Plus@46..47 "+"
                            VarRef@47..48
                              Ident@47..48 "b"
                          Semi@48..49 ";"
                        Whitespace@49..50 " "
                        RBrace@50..51 "}""#]],
    ),

    fn_with_ref_mut_self: ("fn translate(&mut self, mut by: Point) { self.x += by.x; self.y += by.y;}",
            expect![[r#"
                Root@0..73
                  FnDef@0..73
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
                    ParamDecl@22..23
                      Comma@22..23 ","
                    Whitespace@23..24 " "
                    ParamDecl@24..37
                      TypeHint@24..37
                        Mut@24..30
                          KwMut@24..27 "mut"
                          Whitespace@27..28 " "
                          VarRef@28..30
                            Ident@28..30 "by"
                        Colon@30..31 ":"
                        Whitespace@31..32 " "
                        StructAsType@32..37 "Point"
                    RParen@37..38 ")"
                    Whitespace@38..39 " "
                    Block@39..73
                      LBrace@39..40 "{"
                      Whitespace@40..41 " "
                      Semi@41..56
                        InfixBinOp@41..55
                          InfixBinOp@41..48
                            SelfRef@41..45
                              Kwself@41..45 "self"
                            Dot@45..46 "."
                            VarRef@46..48
                              Ident@46..47 "x"
                              Whitespace@47..48 " "
                          PlusEq@48..50 "+="
                          Whitespace@50..51 " "
                          InfixBinOp@51..55
                            VarRef@51..53
                              Ident@51..53 "by"
                            Dot@53..54 "."
                            VarRef@54..55
                              Ident@54..55 "x"
                        Semi@55..56 ";"
                      Whitespace@56..57 " "
                      Semi@57..72
                        InfixBinOp@57..71
                          InfixBinOp@57..64
                            SelfRef@57..61
                              Kwself@57..61 "self"
                            Dot@61..62 "."
                            VarRef@62..64
                              Ident@62..63 "y"
                              Whitespace@63..64 " "
                          PlusEq@64..66 "+="
                          Whitespace@66..67 " "
                          InfixBinOp@67..71
                            VarRef@67..69
                              Ident@67..69 "by"
                            Dot@69..70 "."
                            VarRef@70..71
                              Ident@70..71 "y"
                        Semi@71..72 ";"
                      RBrace@72..73 "}""#]],
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
                        TypeHint@21..37
                          VarRef@21..22
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
                Root@0..43
                  FnDef@0..43
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
                    ParamDecl@22..23
                      Comma@22..23 ","
                    Whitespace@23..24 " "
                    ParamDecl@24..38
                      TypeHint@24..38
                        VarRef@24..26
                          Ident@24..26 "by"
                        Colon@26..27 ":"
                        Whitespace@27..28 " "
                        PrefixUnaryOp@28..38
                          And@28..29 "&"
                          Mut@29..38
                            KwMut@29..32 "mut"
                            Whitespace@32..33 " "
                            StructAsType@33..38 "Point"
                    RParen@38..39 ")"
                    Whitespace@39..40 " "
                    Block@40..43
                      LBrace@40..41 "{"
                      Whitespace@41..42 " "
                      RBrace@42..43 "}""#]],
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
                    InfixBinOp@7..18
                      Mut@7..13
                        KwMut@7..10 "mut"
                        Whitespace@10..11 " "
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
