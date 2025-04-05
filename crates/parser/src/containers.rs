use crate::{
    operator::starting_precedence,
    parse::Finished,
    parser::{IsNext, Parser},
};

use syntax::{
    anchor::RollingBackAnchor,
    bitset::SyntaxKindBitSet,
    is_of_type, non_assigning_operators,
    syntax_kind::SyntaxKind::{self, *},
};

enum State {
    TypeHinted,
    DimHinted,
}

impl State {
    fn parse(&self, parser: &Parser) -> SyntaxKind {
        match self {
            State::TypeHinted => TypeHinted::parse(parser),
            State::DimHinted => DimHinted::parse(parser),
        }
    }
}
struct TypeHinted {}
impl TypeHinted {
    pub const CLOSING: SyntaxKind = SyntaxKind::Gt;
    #[allow(unused_variables)]
    // note: opening delimiter is bumped, we parse what is in btw. and let bumping of closed to the caller
    fn parse(parser: &Parser) -> SyntaxKind {
        let rollback_when_dropped = parser.impose_tensor_structure_restrictions();
        parser.forbid_in_ctx(SyntaxKind::types().as_ref());
        parser.comma_separated_expressions_until(TypeHinted::CLOSING, DimHint);
        SyntaxKind::DimHints
    }
}
struct DimHinted {}
impl DimHinted {
    pub const CLOSING: SyntaxKind = SyntaxKind::Gt;
    #[allow(unused_variables)]
    // note: opening delimiter is bumped, we parse what is in btw. and let bumping of closed to the caller
    fn parse(parser: &Parser) -> SyntaxKind {
        let rollback_when_dropped = parser.impose_tensor_structure_restrictions();
        parser.expect_in_ctx(SyntaxKind::types().as_ref());
        parser.only_allow_in_ctx(SyntaxKind::types().as_ref());
        parser.expect_f_and_bump(is_of_type);
        parser.recover_until(Self::CLOSING);
        SyntaxKind::TypeHint
    }
}

#[allow(unused_variables)]
impl<'input> Parser<'input> {
    // the identifier is already bumped at this point because we ended up here
    // from parsing an identifier and saw a LBrack as well.
    #[allow(unused_variables)]
    pub fn parse_container_indexing(&self) {
        let rollback_when_dropped = self.roll_back_context_after_drop();
        self.expect_in_ctx(SyntaxKind::operators());
        self.allow_in_ctx(non_assigning_operators());
        while IsNext::Yes == self.is_next_strict(LBrack) {
            let marker = self.start();
            {
                let rollback_when_dropped = self.disallow_recovery_of_for_indexing(RBrack);
                self.expect_and_bump(LBrack);
                self.parse_expression_until_binding_power(starting_precedence());
            }
            {
                let rollback_when_dropped = self.disallow_recovery_of_for_indexing(LBrack);
                self.expect_and_bump(RBrack);
            }
            self.complete_marker_with(marker, Indexing);
        }
    }

    fn recover_until(&self, stop: SyntaxKind) {
        while IsNext::Yes != self.is_next_strict(stop) {
            self.recover();
        }
    }
    fn disallow_recovery_of_for_indexing(&self, kind: SyntaxKind) -> RollingBackAnchor {
        let rollback_when_dropped = self.roll_back_context_after_drop();
        self.dont_recover_in_ctx(kind);
        rollback_when_dropped
    }

    #[allow(unused_variables)]
    pub fn parse_tensor_literal(&self) {
        let marker = self.start();
        {
            let rollback_when_dropped = self.disallow_recovery_of_for_indexing(RBrack);
            self.expect_and_bump(LBrack);
            self.comma_separated_expressions_until(RBrack, DimValue);
        }
        {
            let rollback_when_dropped = self.disallow_recovery_of_for_indexing(LBrack);
            self.expect_and_bump(RBrack);
        }
        let dim_values_marker = self.complete_marker_with(marker, TensorLit);
        let literal_marker = self.precede_marker_with(&dim_values_marker);
        self.complete_marker_with(literal_marker, Literal);
    }
    fn impose_tensor_structure_restrictions(&self) -> RollingBackAnchor {
        let rollback_when_dropped = self.roll_back_context_after_drop();
        self.dont_recover_in_ctx([Le, ColonColon, Semi].as_ref());
        rollback_when_dropped
    }
    fn impose_tensor_hinting_restrictions(&self, closing: SyntaxKind) -> RollingBackAnchor {
        let rollback_when_dropped = self.roll_back_context_after_drop();
        self.forbid_in_ctx(closing);
        self.dont_recover_in_ctx([closing].as_ref());
        self.forbid_in_ctx(closing);
        rollback_when_dropped
    }

    #[allow(unused_variables)]
    fn parse_tensor_hint(&self, state: Option<State>) -> Option<State> {
        let opening = Lt;
        let closing = Gt;
        // Tensor<1,2,3> -> [ [x_1_1, x_1_2, x_1_3], [x_2_1, x_2_2, x_2_3] ]
        if !self.is_next(opening) {
            return None;
        }
        let marker = self.start();
        let rollback_when_dropped = self.impose_tensor_hinting_restrictions(closing);
        self.expect_and_bump(opening);

        let (state, kind) = match state {
            Some(state) => (None, state.parse(self)),
            None => {
                let (state, kind) =
                    if IsNext::Yes == self.is_next_in_strict(SyntaxKind::types().as_ref().into()) {
                        let kind = DimHinted::parse(self);
                        let new_state = State::TypeHinted;
                        (new_state, kind)
                    } else {
                        let kind = TypeHinted::parse(self);
                        let new_state = State::DimHinted;
                        (new_state, kind)
                    };
                (Some(state), kind)
            }
        };

        self.expect_and_bump(closing);
        self.complete_marker_with(marker, kind);
        state
    }
    #[allow(unused_variables)]
    fn start_tensor_hints(&self) -> Option<State> {
        self.parse_tensor_hint(None)
    }
    #[allow(unused_variables)]
    fn continue_tensor_hints(&self, state: State) {
        self.parse_tensor_hint(Some(state));
    }

    #[allow(unused_variables)]
    pub fn parse_tensor_structure(&self) -> Option<Finished> {
        let marker = self.start();
        let rollback_when_dropped = self.impose_tensor_structure_restrictions();
        self.expect_and_bump(KwTensor);

        if let Some(state) = self.start_tensor_hints() {
            self.continue_tensor_hints(state);
        };

        self.parse_tensor_initializer();

        Some(self.complete_marker_with(marker, TensorStruct))
    }

    fn comma_separated_expressions_until(
        &self,
        unwanted: impl Into<SyntaxKindBitSet>,
        with: SyntaxKind,
    ) {
        let unwanted: SyntaxKindBitSet = unwanted.into();
        while IsNext::No == self.is_next_in_strict(unwanted) {
            let marker = self.start();
            self.parse_expression_until_binding_power(starting_precedence());
            self.complete_marker_with(marker, with);
            self.ignore_if(Comma);
        }
    }

    #[allow(unused_variables)]
    pub fn parse_tensor_typing(&self) -> Option<Finished> {
        let marker = self.start();
        let rollback_when_dropped = self.impose_tensor_structure_restrictions();
        self.expect_and_bump(TyTensor);

        if let Some(state) = self.start_tensor_hints() {
            self.continue_tensor_hints(state);
        };

        Some(self.complete_marker_with(marker, TensorType))
    }

    #[allow(unused_variables)]
    // Tensor<f32><3,3,3>[<default_value>] OR Tensor<f32>[<default_value>;9]  note: latter is one dimensional
    // note: KwTensor is already bumped at this point
    pub fn parse_tensor_initializer(&self) -> Option<Finished> {
        if IsNext::Yes != self.is_next_strict(LBrack) {
            return None;
        }
        let (opening, closing) = (LBrack, RBrack);
        let marker = self.start();
        // [ <default_value> { ; <dimension> } ]
        let rollback_when_dropped = self.impose_tensor_structure_restrictions();
        self.dont_recover_in_ctx(closing);
        self.forbid_in_ctx(closing);
        self.allow_in_ctx(Semi);
        self.expect_and_bump(opening);
        self.parse_expression_until_binding_power(starting_precedence());
        if IsNext::Yes == self.is_next_strict(Semi) {
            self.expect_and_bump(Semi);
            self.parse_expression_until_binding_power(starting_precedence());
        } else {
            self.recover_until(closing);
        }
        self.expect_and_bump(closing);
        Some(self.complete_marker_with(marker, TensorInit))
    }
}

#[cfg(test)]
mod tests {
    use crate::parse::tests::check;
    use expect_test::expect;
    use parameterized_test::create;

    create! {
        create_parser_test,
        (prog, expect), {
            check(prog, expect);
        }
    }
    create_parser_test! {
        empty_dim_values: ("[]",
            expect![[r#"
                Root@0..2
                  Literal@0..2
                    TensorLit@0..2
                      LBrack@0..1 "["
                      RBrack@1..2 "]""#]]
        ),

        missing_closing_bracket_does_not_panic: ("[",
            expect![[r#"
                Root@0..1
                  Literal@0..1
                    TensorLit@0..1
                      LBrack@0..1 "[""#]]
        ),

        arr_indexing: ("arr[arr.len() - 1]",
            expect![[r#"
                Root@0..18
                  ContainerRef@0..18
                    Ident@0..3 "arr"
                    Indexing@3..18
                      LBrack@3..4 "["
                      InfixBinOp@4..17
                        InfixBinOp@4..14
                          VarRef@4..7
                            Ident@4..7 "arr"
                          Dot@7..8 "."
                          FnCall@8..13
                            Ident@8..11 "len"
                            LParen@11..12 "("
                            RParen@12..13 ")"
                          Whitespace@13..14 " "
                        Minus@14..15 "-"
                        Whitespace@15..16 " "
                        Literal@16..17
                          Int@16..17 "1"
                      RBrack@17..18 "]""#]],
        ),
        tensor_indexing: ("matrix[matrix[0].len() - 1][0]",
            expect![[r#"
                Root@0..30
                  ContainerRef@0..30
                    Ident@0..6 "matrix"
                    Indexing@6..27
                      LBrack@6..7 "["
                      InfixBinOp@7..26
                        InfixBinOp@7..23
                          ContainerRef@7..16
                            Ident@7..13 "matrix"
                            Indexing@13..16
                              LBrack@13..14 "["
                              Literal@14..15
                                Int@14..15 "0"
                              RBrack@15..16 "]"
                          Dot@16..17 "."
                          FnCall@17..22
                            Ident@17..20 "len"
                            LParen@20..21 "("
                            RParen@21..22 ")"
                          Whitespace@22..23 " "
                        Minus@23..24 "-"
                        Whitespace@24..25 " "
                        Literal@25..26
                          Int@25..26 "1"
                      RBrack@26..27 "]"
                    Indexing@27..30
                      LBrack@27..28 "["
                      Literal@28..29
                        Int@28..29 "0"
                      RBrack@29..30 "]""#]]
        ),
        var_def_from_arr: ("let z = arr[me.z - she.z];",
            expect![[r#"
                Root@0..26
                  VarDef@0..26
                    KwLet@0..3 "let"
                    Whitespace@3..4 " "
                    InfixBinOp@4..25
                      VarRef@4..6
                        Ident@4..5 "z"
                        Whitespace@5..6 " "
                      Eq@6..7 "="
                      Whitespace@7..8 " "
                      ContainerRef@8..25
                        Ident@8..11 "arr"
                        Indexing@11..25
                          LBrack@11..12 "["
                          InfixBinOp@12..24
                            InfixBinOp@12..17
                              VarRef@12..14
                                Ident@12..14 "me"
                              Dot@14..15 "."
                              VarRef@15..17
                                Ident@15..16 "z"
                                Whitespace@16..17 " "
                            Minus@17..18 "-"
                            Whitespace@18..19 " "
                            InfixBinOp@19..24
                              VarRef@19..22
                                Ident@19..22 "she"
                              Dot@22..23 "."
                              VarRef@23..24
                                Ident@23..24 "z"
                          RBrack@24..25 "]"
                    Semi@25..26 ";""#]]
        ),
        var_def_with_tensor_literal: ("let z = [1,1,1];",
            expect![[r#"
                Root@0..14
                  VarDef@0..14
                    KwLet@0..3 "let"
                    Whitespace@3..4 " "
                    InfixBinOp@4..13
                      VarRef@4..6
                        Ident@4..5 "z"
                        Whitespace@5..6 " "
                      Eq@6..7 "="
                      Whitespace@7..8 " "
                      Literal@8..13
                        TensorLit@8..13
                          LBrack@8..9 "["
                          DimValue@9..10
                            Literal@9..10
                              Int@9..10 "1"
                          DimValue@10..11
                            Literal@10..11
                              Int@10..11 "1"
                          DimValue@11..12
                            Literal@11..12
                              Int@11..12 "1"
                          RBrack@12..13 "]"
                    Semi@13..14 ";""#]]
        ),
        var_def_with_2d_tensor_literal: ("let u = [[1,0,0],[0,1,0],[0,0,1]];",
            expect![[r#"
                Root@0..26
                  VarDef@0..26
                    KwLet@0..3 "let"
                    Whitespace@3..4 " "
                    InfixBinOp@4..25
                      VarRef@4..6
                        Ident@4..5 "u"
                        Whitespace@5..6 " "
                      Eq@6..7 "="
                      Whitespace@7..8 " "
                      Literal@8..25
                        TensorLit@8..25
                          LBrack@8..9 "["
                          DimValue@9..14
                            Literal@9..14
                              TensorLit@9..14
                                LBrack@9..10 "["
                                DimValue@10..11
                                  Literal@10..11
                                    Int@10..11 "1"
                                DimValue@11..12
                                  Literal@11..12
                                    Int@11..12 "0"
                                DimValue@12..13
                                  Literal@12..13
                                    Int@12..13 "0"
                                RBrack@13..14 "]"
                          DimValue@14..19
                            Literal@14..19
                              TensorLit@14..19
                                LBrack@14..15 "["
                                DimValue@15..16
                                  Literal@15..16
                                    Int@15..16 "0"
                                DimValue@16..17
                                  Literal@16..17
                                    Int@16..17 "1"
                                DimValue@17..18
                                  Literal@17..18
                                    Int@17..18 "0"
                                RBrack@18..19 "]"
                          DimValue@19..24
                            Literal@19..24
                              TensorLit@19..24
                                LBrack@19..20 "["
                                DimValue@20..21
                                  Literal@20..21
                                    Int@20..21 "0"
                                DimValue@21..22
                                  Literal@21..22
                                    Int@21..22 "0"
                                DimValue@22..23
                                  Literal@22..23
                                    Int@22..23 "1"
                                RBrack@23..24 "]"
                          RBrack@24..25 "]"
                    Semi@25..26 ";""#]]
        ),
        var_def_with_tensor_structure: ("let z = Tensor<3,3,3>;",
            expect![[r#"
                Root@0..20
                  VarDef@0..20
                    KwLet@0..3 "let"
                    Whitespace@3..4 " "
                    InfixBinOp@4..19
                      VarRef@4..6
                        Ident@4..5 "z"
                        Whitespace@5..6 " "
                      Eq@6..7 "="
                      Whitespace@7..8 " "
                      TensorStruct@8..19
                        KwTensor@8..14 "Tensor"
                        DimHints@14..19
                          Lt@14..15 "<"
                          DimHint@15..16
                            Literal@15..16
                              Int@15..16 "3"
                          DimHint@16..17
                            Literal@16..17
                              Int@16..17 "3"
                          DimHint@17..18
                            Literal@17..18
                              Int@17..18 "3"
                          Gt@18..19 ">"
                    Semi@19..20 ";""#]]
        ),
        var_def_with_tensor_structure_with_type_hint: ("let z = Tensor<f32><3,3,3>::new();",
            expect![[r#"
                Root@0..32
                  VarDef@0..32
                    KwLet@0..3 "let"
                    Whitespace@3..4 " "
                    InfixBinOp@4..31
                      VarRef@4..6
                        Ident@4..5 "z"
                        Whitespace@5..6 " "
                      Eq@6..7 "="
                      Whitespace@7..8 " "
                      InfixBinOp@8..31
                        TensorStruct@8..24
                          KwTensor@8..14 "Tensor"
                          TypeHint@14..19
                            Lt@14..15 "<"
                            TyF32@15..18 "f32"
                            Gt@18..19 ">"
                          DimHints@19..24
                            Lt@19..20 "<"
                            DimHint@20..21
                              Literal@20..21
                                Int@20..21 "3"
                            DimHint@21..22
                              Literal@21..22
                                Int@21..22 "3"
                            DimHint@22..23
                              Literal@22..23
                                Int@22..23 "3"
                            Gt@23..24 ">"
                        ColonColon@24..26 "::"
                        FnCall@26..31
                          Ident@26..29 "new"
                          LParen@29..30 "("
                          RParen@30..31 ")"
                    Semi@31..32 ";""#]]
        ),

        var_def_with_tensor_structure_with_consecutive_type_hints: ("let z = Tensor<f32><i32>::new();",
            expect![[r#"
                Root@0..32
                  VarDef@0..32
                    KwLet@0..3 "let"
                    Whitespace@3..4 " "
                    InfixBinOp@4..31
                      VarRef@4..6
                        Ident@4..5 "z"
                        Whitespace@5..6 " "
                      Eq@6..7 "="
                      Whitespace@7..8 " "
                      InfixBinOp@8..31
                        TensorStruct@8..24
                          KwTensor@8..14 "Tensor"
                          TypeHint@14..19
                            Lt@14..15 "<"
                            TyF32@15..18 "f32"
                            Gt@18..19 ">"
                          DimHints@19..24
                            Lt@19..20 "<"
                            DimHint@20..23
                              Recovered@20..23
                                TyI32@20..23 "i32"
                            Gt@23..24 ">"
                        ColonColon@24..26 "::"
                        FnCall@26..31
                          Ident@26..29 "new"
                          LParen@29..30 "("
                          RParen@30..31 ")"
                    Semi@31..32 ";""#]]
        ),
        var_def_with_tensor_structure_with_consecutive_dim_hints: ("let z = Tensor<3,3,3><1,1,1>::new();",
            expect![[r#"
                Root@0..34
                  VarDef@0..34
                    KwLet@0..3 "let"
                    Whitespace@3..4 " "
                    InfixBinOp@4..33
                      VarRef@4..6
                        Ident@4..5 "z"
                        Whitespace@5..6 " "
                      Eq@6..7 "="
                      Whitespace@7..8 " "
                      InfixBinOp@8..33
                        TensorStruct@8..26
                          KwTensor@8..14 "Tensor"
                          DimHints@14..19
                            Lt@14..15 "<"
                            DimHint@15..16
                              Literal@15..16
                                Int@15..16 "3"
                            DimHint@16..17
                              Literal@16..17
                                Int@16..17 "3"
                            DimHint@17..18
                              Literal@17..18
                                Int@17..18 "3"
                            Gt@18..19 ">"
                          TypeHint@19..26
                            Lt@19..20 "<"
                            Recovered@20..21
                              Int@20..21 "1"
                            Recovered@21..22
                              Comma@21..22 ","
                            Recovered@22..23
                              Int@22..23 "1"
                            Recovered@23..24
                              Comma@23..24 ","
                            Recovered@24..25
                              Int@24..25 "1"
                            Gt@25..26 ">"
                        ColonColon@26..28 "::"
                        FnCall@28..33
                          Ident@28..31 "new"
                          LParen@31..32 "("
                          RParen@32..33 ")"
                    Semi@33..34 ";""#]]
        ),
    fn_def_with_tensor_typing_dim_hints: ("fn matmul() -> tensor<3,3,3> {return inner_matmul(); }",
            expect![[r#"
                Root@0..52
                  FnDef@0..52
                    KwFn@0..2 "fn"
                    Whitespace@2..3 " "
                    Ident@3..9 "matmul"
                    LParen@9..10 "("
                    RParen@10..11 ")"
                    Whitespace@11..12 " "
                    RetType@12..27
                      RArrow@12..14 "->"
                      Whitespace@14..15 " "
                      TensorType@15..27
                        TyTensor@15..21 "tensor"
                        DimHints@21..26
                          Lt@21..22 "<"
                          DimHint@22..23
                            Literal@22..23
                              Int@22..23 "3"
                          DimHint@23..24
                            Literal@23..24
                              Int@23..24 "3"
                          DimHint@24..25
                            Literal@24..25
                              Int@24..25 "3"
                          Gt@25..26 ">"
                        Whitespace@26..27 " "
                    Block@27..52
                      LBrace@27..28 "{"
                      Return@28..50
                        KwReturn@28..34 "return"
                        Whitespace@34..35 " "
                        FnCall@35..49
                          Ident@35..47 "inner_matmul"
                          LParen@47..48 "("
                          RParen@48..49 ")"
                        Semi@49..50 ";"
                      Whitespace@50..51 " "
                      RBrace@51..52 "}""#]]
        ),

    tensor_initializer_with_typehint_and_default_value_with_dim: ("Tensor<f32>[0.0; 100]",
            expect![[r#"
                Root@0..21
                  TensorStruct@0..21
                    KwTensor@0..6 "Tensor"
                    TypeHint@6..11
                      Lt@6..7 "<"
                      TyF32@7..10 "f32"
                      Gt@10..11 ">"
                    TensorInit@11..21
                      LBrack@11..12 "["
                      Literal@12..15
                        Float@12..15 "0.0"
                      Semi@15..16 ";"
                      Whitespace@16..17 " "
                      Literal@17..20
                        Int@17..20 "100"
                      RBrack@20..21 "]""#]]
        ),
    tensor_initializer_with_dim_and_typehint_and_default_value: ("Tensor<f32><100,100,100>[0.0]",
            expect![[r#"
                Root@0..27
                  TensorStruct@0..27
                    KwTensor@0..6 "Tensor"
                    TypeHint@6..11
                      Lt@6..7 "<"
                      TyF32@7..10 "f32"
                      Gt@10..11 ">"
                    DimHints@11..22
                      Lt@11..12 "<"
                      DimHint@12..15
                        Literal@12..15
                          Int@12..15 "100"
                      DimHint@15..18
                        Literal@15..18
                          Int@15..18 "100"
                      DimHint@18..21
                        Literal@18..21
                          Int@18..21 "100"
                      Gt@21..22 ">"
                    TensorInit@22..27
                      LBrack@22..23 "["
                      Literal@23..26
                        Float@23..26 "0.0"
                      RBrack@26..27 "]""#]]
        ),
    tensor_initializer_with_dim_and_typehint_and_default_value_with_recovery: ("Tensor<f32><100,100,100>[0.0, recover_this]",
            expect![[r#"
                Root@0..41
                  TensorStruct@0..41
                    KwTensor@0..6 "Tensor"
                    TypeHint@6..11
                      Lt@6..7 "<"
                      TyF32@7..10 "f32"
                      Gt@10..11 ">"
                    DimHints@11..22
                      Lt@11..12 "<"
                      DimHint@12..15
                        Literal@12..15
                          Int@12..15 "100"
                      DimHint@15..18
                        Literal@15..18
                          Int@15..18 "100"
                      DimHint@18..21
                        Literal@18..21
                          Int@18..21 "100"
                      Gt@21..22 ">"
                    TensorInit@22..41
                      LBrack@22..23 "["
                      Literal@23..26
                        Float@23..26 "0.0"
                      Recovered@26..27
                        Comma@26..27 ","
                      Whitespace@27..28 " "
                      Recovered@28..40
                        Ident@28..40 "recover_this"
                      RBrack@40..41 "]""#]]
        ),
    }
}
