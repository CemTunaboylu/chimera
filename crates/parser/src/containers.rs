use crate::{
    operator::starting_precedence,
    parse::{Element, Finished, Started},
    parser::{IsNext, Parser},
};

use syntax::{
    Syntax,
    bitset::SyntaxKindBitSet,
    syntax_kind::SyntaxKind::{self, *},
};

use thin_vec::thin_vec;

#[derive(Clone, Debug, PartialEq)]
pub struct HintParser {
    opening_kind: SyntaxKind,
    closing_kind: SyntaxKind,
    parsing_buffer: bool,
    parsed_type_hint: bool,
    parsed_dim_hints: bool,
}

impl HintParser {
    fn new(o: SyntaxKind, c: SyntaxKind, is_buffer: bool) -> Self {
        Self {
            opening_kind: o,
            closing_kind: c,
            parsing_buffer: is_buffer,
            parsed_type_hint: false,
            parsed_dim_hints: false,
        }
    }
    fn dim_hinting(o: SyntaxKind, c: SyntaxKind, is_buffer: bool) -> Self {
        let mut me = Self::new(o, c, is_buffer);
        me.parsed_type_hint = true;
        me
    }

    // note: opening delimiter is bumped, we parse what is in btw. and let bumping of closed to the caller
    #[allow(unused_variables)]
    fn parse_dim_hints(&mut self, parser: &Parser) -> Option<SyntaxKind> {
        if self.parsed_dim_hints {
            parser.recover_until(self.closing_kind);
            return None;
        }
        let rollback_when_dropped = parser.impose_restrictions_of_kind_on_context(DimHints);
        let elements = if self.parsing_buffer {
            // for buffer to be a static type (allocated on the stack)
            // its size must be known at compile time
            parser.allow_in_ctx([Int, Comma].as_ref());
            thin_vec![Element::Kind(Int)]
        } else {
            // tensor, since it is a dynamic type (allocated on the heap)
            // its size can be adjusted dynamically thus can be any expression
            // that evaluates to an integer
            // e.g.:  tensor<f32><x,y,z> where x,y and z are int variables is valid
            parser.allow_in_ctx([Star, LBrace, LBrack, LParen, Ident].as_ref());
            parser.allow_in_ctx(SyntaxKind::operators().as_ref());
            // TODO: seems like a bad idea but solves a lot of problems :)
            parser.forbid_in_ctx(self.closing_kind);
            thin_vec![Element::Branched(
                thin_vec![Element::InSet([Int, Under].as_ref().into())],
                thin_vec![Element::ParseExprWith(starting_precedence())],
            )]
        };
        let stopper = [self.closing_kind, Semi, RBrace];
        parser.bump_separated_by(&elements, Comma, stopper.as_ref());
        self.parsed_dim_hints = true;
        Some(DimHints)
    }

    #[allow(unused_variables)]
    fn parse_type_hint(&mut self, parser: &Parser) -> Option<SyntaxKind> {
        if self.parsed_type_hint {
            parser.recover_until(self.closing_kind);
            return None;
        }
        // TODO: in case of buffers, we should return an error
        let rollback_when_dropped = parser.impose_restrictions_of_kind_on_context(TypeHint);
        parser.parse_type(
            &parser
                .peek()
                .expect("something to peek")
                .expect("ok to peek"),
        );
        self.parsed_type_hint = true;
        Some(TypeHint)
    }

    #[allow(unused_variables)]
    fn parse_hints(&mut self, parser: &Parser) {
        if !parser.is_next(self.opening_kind) {
            return;
        }
        let rollback_when_dropped = parser.impose_restrictions_of_kind_on_context(Gt);
        let allowed = if self.parsing_buffer {
            [Int, TyBuffer].as_ref()
        } else {
            [Int, TyBuffer, TyTensor, Ident].as_ref()
        };
        parser.allow_in_ctx(allowed);
        for _ in 0..2 {
            if !parser.is_next(self.opening_kind) {
                break;
            }
            parser.expect_and_bump(self.opening_kind);
            let marker = parser.start();
            let kind = if let Some(kind) = self.parse(parser) {
                kind
            } else {
                parser.recover_until(self.closing_kind);
                if self.parsed_dim_hints {
                    TypeHint
                } else {
                    DimHints
                }
            };
            parser.complete_marker_with(marker, kind);
            parser.expect_and_bump(self.closing_kind);
        }
    }
    fn parse(&mut self, parser: &Parser) -> Option<SyntaxKind> {
        let peeked = parser.peek()?.expect("ok to peek");
        match peeked.get_kind() {
            // note: we can allow any expression, type inference pass will catch all
            // do not evaluate to Int (usize actually) but it would be more cumbersome, thus this granularity
            op if op.is_unary_operator() && !self.parsing_buffer => self.parse_dim_hints(parser),
            Ident | Under if !self.parsing_buffer => self.parse_dim_hints(parser),
            Int => self.parse_dim_hints(parser),
            // including KwBuffer, KwFn, and KwTensor
            t if SyntaxKindBitSet::from(SyntaxKind::can_be_parameter().as_ref()).contains(t) => {
                self.parse_type_hint(parser)
            }
            _ => {
                parser.recover_until(self.closing_kind);
                None
            }
        }
    }
}

#[allow(unused_variables)]
impl Parser<'_> {
    // the identifier is already bumped at this point because we ended up here
    // from parsing an identifier and saw a LBrack as well.
    #[allow(unused_variables)]
    pub fn parse_container_indexing(&self) {
        let rollback_when_dropped = self.impose_context_for_parsing(Indexing);
        while IsNext::Yes == self.is_next_strict(LBrack) {
            let marker = self.start();
            {
                let rollback_when_dropped = self.impose_context_for_parsing(LBrack);
                self.expect_and_bump(LBrack);
                self.parse_expression_until_binding_power(starting_precedence());
            }
            {
                self.impose_context_for_parsing(RBrack);
                self.expect_and_bump(RBrack);
            }
            self.complete_marker_with(marker, Indexing);
        }
    }

    #[allow(unused_variables)]
    pub fn parse_buffer_literal(&self) {
        let marker = self.start();
        {
            let rollback_when_dropped = self.impose_context_for_parsing(LBrack);
            self.expect_and_bump(LBrack);
            self.comma_separated_expressions_until(RBrack, DimValue);
        }
        {
            let rollback_when_dropped = self.impose_context_for_parsing(RBrack);
            self.expect_and_bump(RBrack);
        }
        let dim_values_marker = self.complete_marker_with(marker, BufferLit);
        let literal_marker = self.precede_marker_with(&dim_values_marker);
        self.complete_marker_with(literal_marker, Literal);
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

    pub fn parse_container_constructs(&self, syntax: &Syntax) -> Option<Finished> {
        let marker = self.start();
        let kind = syntax.get_kind();
        self.expect_and_bump(kind);
        if IsNext::Yes == self.is_next_strict(LBrack) {
            self.parse_initializer(kind, marker)
        } else if IsNext::Yes == self.is_next_strict(ColonColon) {
            // parse the class method
            let with = if kind == KwTensor { TyTensor } else { TyBuffer };
            let complete = self.complete_marker_with(marker, with);
            self.parse_binary_operation(syntax.clone(), &starting_precedence(), &complete)
        } else {
            // else we directly try typing since buffers need type and shape hint
            // so that they become a complete type, otherwise it is an error
            self.parse_buffer_or_tensor_typing(kind, marker)
        }
    }

    /*
       Hints apply to buffers(mandatory) or tensors(optional) to indicate the type and shape.
       buffer<type><[dim]> where:
        - type and dim order can change
        - each dim has to be concrete usize
       tensor<type><[dim]> where:
        - type and dim order can change and are optional
        - each dim can also be '_' to hint that the shape is not known BUT the dimensionality is hinted

     a dimension hint as <1,2,3> corresponds the following: [ [x_1_1, x_1_2, x_1_3], [x_2_1, x_2_2, x_2_3] ]
    */
    #[allow(unused_variables)]
    pub fn parse_buffer_or_tensor_typing(
        &self,
        kind: SyntaxKind,
        marker: Started,
    ) -> Option<Finished> {
        let (parsing_buffer, kind_to_complete_with) = if kind == KwTensor {
            (false, TyTensor)
        } else {
            (true, TyBuffer)
        };

        let (opening, closing) = (Lt, Gt);
        let mut hint_parser = HintParser::new(opening, closing, parsing_buffer);
        hint_parser.parse_hints(self);
        Some(self.complete_marker_with(marker, kind_to_complete_with))
    }

    /*
       An initializer is a practical syntactic sugar to declare and define the container (buffer or tensor).
       Given a value, the container is set to have the value in the last dimension
       buffer[<value> ; <shape>] where
           - value can be any literal, (including another buffer or a tensor); or an expression that returns non unit_type and has
           - shape can be a single usize or another buffer
           e.g. buffer[[1,0,1,0];[100,100]] -> results with a buffer of shape[100,100,4] with each set to [1,0,1,0]

        tensor[<value> ; <shape>] where
           - value can be any literal, (including a buffer and a tensor); or an expression that returns non unit_type and has
           - shape can be a single usize, a buffer, a variable or a buffer of variables
           e.g. tensor[arbitrary_values;[x,y]] -> results with a tensor of shape[x,y,arbitrary.len()] with each set to arbitrary_values

        note: an initializer cannot be used after hints, e.g. buffer<f32><100,100>[0.0; [100, 100]] is invalid.
        note: an initializer is just a syntactic sugar for a literal, it gives the compiler a heads up that the
            buffer/tensor is going to be initialized with a value and a shape, and let's the compiler handle
            the initialization logic i.e. being lazy, or COWing etc.
    */
    #[allow(unused_variables)]
    pub fn parse_initializer(&self, kind: SyntaxKind, marker: Started) -> Option<Finished> {
        let (opening, closing) = (LBrack, RBrack);
        let container_specific_lit = if kind == KwBuffer {
            BufferLit
        } else {
            TensorLit
        };
        let rollback_when_dropped = self.impose_context_for_parsing(kind);
        self.allow_in_ctx(Semi);
        self.expect_and_bump(opening);

        self.parse_expression_until_binding_power(starting_precedence());

        if IsNext::Yes == self.is_next_strict(Semi) {
            self.expect_and_bump(Semi);
            let is_buffer = kind == KwBuffer;
            if self.is_next(Int) {
                self.parse_literal(Int);
            } else if !is_buffer && self.is_next(Ident) {
                self.parse_starting_with_identifier();
            } else {
                let rollback_when_dropped = self.impose_restrictions_of_kind_on_context(DimHints);
                let mut hint_parser = HintParser::dim_hinting(opening, closing, is_buffer);
                hint_parser.parse_hints(self);
            }
        } else {
            self.recover_until(closing);
        }
        self.expect_and_bump(closing);
        let complete = self.complete_marker_with(marker, container_specific_lit);
        let wrapping_lit_marker = self.precede_marker_with(&complete);
        Some(self.complete_marker_with(wrapping_lit_marker, Literal))
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
                    BufferLit@0..2
                      LBrack@0..1 "["
                      RBrack@1..2 "]""#]]
        ),

        missing_closing_bracket_does_not_panic: ("[",
            expect![[r#"
                Root@0..1
                  Literal@0..1
                    BufferLit@0..1
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
                          Call@8..13
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
                          Call@17..22
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
                  LetBinding@0..26
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
        var_def_with_buffer_literal: ("let z = [1,1,1];",
            expect![[r#"
                Root@0..14
                  LetBinding@0..14
                    KwLet@0..3 "let"
                    Whitespace@3..4 " "
                    InfixBinOp@4..13
                      VarRef@4..6
                        Ident@4..5 "z"
                        Whitespace@5..6 " "
                      Eq@6..7 "="
                      Whitespace@7..8 " "
                      Literal@8..13
                        BufferLit@8..13
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
        var_def_with_generic_dim_hinted_buffer_fails: ("let z: buffer<_,_,_> = [1,1,1];",
            expect![[r#"
                Root@0..29
                  LetBinding@0..29
                    KwLet@0..3 "let"
                    Whitespace@3..4 " "
                    InfixBinOp@4..28
                      VarRef@4..21
                        Ident@4..5 "z"
                        Colon@5..6 ":"
                        TypeHint@6..21
                          Whitespace@6..7 " "
                          TyBuffer@7..21
                            KwBuffer@7..13 "buffer"
                            Lt@13..14 "<"
                            DimHints@14..19
                              Recovered@14..15
                                Under@14..15 "_"
                              Recovered@15..16
                                Comma@15..16 ","
                              Recovered@16..17
                                Under@16..17 "_"
                              Recovered@17..18
                                Comma@17..18 ","
                              Recovered@18..19
                                Under@18..19 "_"
                            Gt@19..20 ">"
                            Whitespace@20..21 " "
                      Eq@21..22 "="
                      Whitespace@22..23 " "
                      Literal@23..28
                        BufferLit@23..28
                          LBrack@23..24 "["
                          DimValue@24..25
                            Literal@24..25
                              Int@24..25 "1"
                          DimValue@25..26
                            Literal@25..26
                              Int@25..26 "1"
                          DimValue@26..27
                            Literal@26..27
                              Int@26..27 "1"
                          RBrack@27..28 "]"
                    Semi@28..29 ";""#]]
        ),
        var_def_with_2d_tensor_literal: ("let u = [[1,0,0],[0,1,0],[0,0,1]];",
            expect![[r#"
                Root@0..26
                  LetBinding@0..26
                    KwLet@0..3 "let"
                    Whitespace@3..4 " "
                    InfixBinOp@4..25
                      VarRef@4..6
                        Ident@4..5 "u"
                        Whitespace@5..6 " "
                      Eq@6..7 "="
                      Whitespace@7..8 " "
                      Literal@8..25
                        BufferLit@8..25
                          LBrack@8..9 "["
                          DimValue@9..14
                            Literal@9..14
                              BufferLit@9..14
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
                              BufferLit@14..19
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
                              BufferLit@19..24
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
        var_def_with_fully_generic_shaped_tensor: ("let z : tensor<_,_,_> = random_3d_tensor();",
            expect![[r#"
                Root@0..43
                  LetBinding@0..43
                    KwLet@0..3 "let"
                    Whitespace@3..4 " "
                    InfixBinOp@4..42
                      VarRef@4..22
                        Ident@4..5 "z"
                        Whitespace@5..6 " "
                        Colon@6..7 ":"
                        TypeHint@7..22
                          Whitespace@7..8 " "
                          TyTensor@8..22
                            KwTensor@8..14 "tensor"
                            Lt@14..15 "<"
                            DimHints@15..20
                              Under@15..16 "_"
                              Comma@16..17 ","
                              Under@17..18 "_"
                              Comma@18..19 ","
                              Under@19..20 "_"
                            Gt@20..21 ">"
                            Whitespace@21..22 " "
                      Eq@22..23 "="
                      Whitespace@23..24 " "
                      Call@24..42
                        Ident@24..40 "random_3d_tensor"
                        LParen@40..41 "("
                        RParen@41..42 ")"
                    Semi@42..43 ";""#]]
        ),
        var_def_with_tensor_type_with_hint_cannot_take_a_fn_call: ("let z = tensor<f32>::new();",
            expect![[r#"
                Root@0..27
                  LetBinding@0..27
                    KwLet@0..3 "let"
                    Whitespace@3..4 " "
                    InfixBinOp@4..26
                      VarRef@4..6
                        Ident@4..5 "z"
                        Whitespace@5..6 " "
                      Eq@6..7 "="
                      Whitespace@7..8 " "
                      InfixBinOp@8..26
                        TyTensor@8..19
                          KwTensor@8..14 "tensor"
                          Lt@14..15 "<"
                          TypeHint@15..18
                            TyF32@15..18 "f32"
                          Gt@18..19 ">"
                        ColonColon@19..21 "::"
                        Call@21..26
                          Ident@21..24 "new"
                          LParen@24..25 "("
                          RParen@25..26 ")"
                    Semi@26..27 ";""#]]
        ),

        var_def_with_tensor_structure_with_consecutive_type_hints: ("let z = tensor<f32><i32>::new();",
            expect![[r#"
                Root@0..32
                  LetBinding@0..32
                    KwLet@0..3 "let"
                    Whitespace@3..4 " "
                    InfixBinOp@4..31
                      VarRef@4..6
                        Ident@4..5 "z"
                        Whitespace@5..6 " "
                      Eq@6..7 "="
                      Whitespace@7..8 " "
                      InfixBinOp@8..31
                        TyTensor@8..24
                          KwTensor@8..14 "tensor"
                          Lt@14..15 "<"
                          TypeHint@15..18
                            TyF32@15..18 "f32"
                          Gt@18..19 ">"
                          Lt@19..20 "<"
                          DimHints@20..23
                            Recovered@20..23
                              TyI32@20..23 "i32"
                          Gt@23..24 ">"
                        ColonColon@24..26 "::"
                        Call@26..31
                          Ident@26..29 "new"
                          LParen@29..30 "("
                          RParen@30..31 ")"
                    Semi@31..32 ";""#]]
        ),
        // note: the initialized tensor will be promoted to stack since we know its size
        // even if it is escaped (in this case it will be moved)
        var_def_with_tensor_structure_with_consecutive_dim_hints: ("let z : tensor<3,3,3><1,1,1> = tensor[0; [3,3,3]];",
            expect![[r#"
                Root@0..50
                  LetBinding@0..50
                    KwLet@0..3 "let"
                    Whitespace@3..4 " "
                    InfixBinOp@4..49
                      VarRef@4..28
                        Ident@4..5 "z"
                        Whitespace@5..6 " "
                        Colon@6..7 ":"
                        TypeHint@7..28
                          Whitespace@7..8 " "
                          TyTensor@8..28
                            KwTensor@8..14 "tensor"
                            Lt@14..15 "<"
                            DimHints@15..20
                              Int@15..16 "3"
                              Comma@16..17 ","
                              Int@17..18 "3"
                              Comma@18..19 ","
                              Int@19..20 "3"
                            Gt@20..21 ">"
                            Lt@21..22 "<"
                            TypeHint@22..27
                              Recovered@22..23
                                Int@22..23 "1"
                              Recovered@23..24
                                Comma@23..24 ","
                              Recovered@24..25
                                Int@24..25 "1"
                              Recovered@25..26
                                Comma@25..26 ","
                              Recovered@26..27
                                Int@26..27 "1"
                            Gt@27..28 ">"
                      Whitespace@28..29 " "
                      Eq@29..30 "="
                      Whitespace@30..31 " "
                      Literal@31..49
                        TensorLit@31..49
                          KwTensor@31..37 "tensor"
                          LBrack@37..38 "["
                          Literal@38..39
                            Int@38..39 "0"
                          Semi@39..40 ";"
                          Whitespace@40..41 " "
                          LBrack@41..42 "["
                          DimHints@42..47
                            Int@42..43 "3"
                            Comma@43..44 ","
                            Int@44..45 "3"
                            Comma@45..46 ","
                            Int@46..47 "3"
                          RBrack@47..48 "]"
                          RBrack@48..49 "]"
                    Semi@49..50 ";""#]]
        ),
    fn_def_with_tensor_typing_dim_hints: ("fn matmul() -> tensor<_,_,3> {return inner_matmul(); }",
            expect![[r#"
                Root@0..54
                  FnDef@0..54
                    KwFn@0..2 "fn"
                    Whitespace@2..3 " "
                    Ident@3..9 "matmul"
                    LParen@9..10 "("
                    RParen@10..11 ")"
                    Whitespace@11..12 " "
                    RetType@12..29
                      RArrow@12..14 "->"
                      Whitespace@14..15 " "
                      TyTensor@15..29
                        KwTensor@15..21 "tensor"
                        Lt@21..22 "<"
                        DimHints@22..27
                          Under@22..23 "_"
                          Comma@23..24 ","
                          Under@24..25 "_"
                          Comma@25..26 ","
                          Int@26..27 "3"
                        Gt@27..28 ">"
                        Whitespace@28..29 " "
                    Block@29..54
                      LBrace@29..30 "{"
                      Return@30..52
                        KwReturn@30..36 "return"
                        Whitespace@36..37 " "
                        Call@37..51
                          Ident@37..49 "inner_matmul"
                          LParen@49..50 "("
                          RParen@50..51 ")"
                        Semi@51..52 ";"
                      Whitespace@52..53 " "
                      RBrace@53..54 "}""#]]
        ),

    tensor_initializer_with_dim_and_typehint_and_default_value_wont_work: ("tensor<f32><100,100,100>[0.0]",
            expect![[r#"
                Root@0..29
                  TyTensor@0..24
                    KwTensor@0..6 "tensor"
                    Lt@6..7 "<"
                    TypeHint@7..10
                      TyF32@7..10 "f32"
                    Gt@10..11 ">"
                    Lt@11..12 "<"
                    DimHints@12..23
                      Int@12..15 "100"
                      Comma@15..16 ","
                      Int@16..19 "100"
                      Comma@19..20 ","
                      Int@20..23 "100"
                    Gt@23..24 ">"
                  Literal@24..29
                    BufferLit@24..29
                      LBrack@24..25 "["
                      DimValue@25..28
                        Literal@25..28
                          Float@25..28 "0.0"
                      RBrack@28..29 "]""#]]
        ),
    tensor_type_with_dim_and_typehint: ("tensor<f32><100,100,100>",
            expect![[r#"
                Root@0..24
                  TyTensor@0..24
                    KwTensor@0..6 "tensor"
                    Lt@6..7 "<"
                    TypeHint@7..10
                      TyF32@7..10 "f32"
                    Gt@10..11 ">"
                    Lt@11..12 "<"
                    DimHints@12..23
                      Int@12..15 "100"
                      Comma@15..16 ","
                      Int@16..19 "100"
                      Comma@19..20 ","
                      Int@20..23 "100"
                    Gt@23..24 ">""#]]
        ),
    generic_tensor_shape: ("tensor<100,_,100, Color::dims()>",
            expect![[r#"
                Root@0..32
                  TyTensor@0..32
                    KwTensor@0..6 "tensor"
                    Lt@6..7 "<"
                    DimHints@7..31
                      Int@7..10 "100"
                      Comma@10..11 ","
                      Under@11..12 "_"
                      Comma@12..13 ","
                      Int@13..16 "100"
                      Comma@16..17 ","
                      Whitespace@17..18 " "
                      InfixBinOp@18..31
                        VarRef@18..23
                          Ident@18..23 "Color"
                        ColonColon@23..25 "::"
                        Call@25..31
                          Ident@25..29 "dims"
                          LParen@29..30 "("
                          RParen@30..31 ")"
                    Gt@31..32 ">""#]]
        ),
    dynamic_tensor_initializer: ("tensor[0.0; dynamic]",
            expect![[r#"
                Root@0..20
                  Literal@0..20
                    TensorLit@0..20
                      KwTensor@0..6 "tensor"
                      LBrack@6..7 "["
                      Literal@7..10
                        Float@7..10 "0.0"
                      Semi@10..11 ";"
                      Whitespace@11..12 " "
                      VarRef@12..19
                        Ident@12..19 "dynamic"
                      RBrack@19..20 "]""#]]
        ),
    buffer_initializer_and_default_value_with_dim: ("buffer[0.0; 100 ]",
            expect![[r#"
                Root@0..17
                  Literal@0..17
                    BufferLit@0..17
                      KwBuffer@0..6 "buffer"
                      LBrack@6..7 "["
                      Literal@7..10
                        Float@7..10 "0.0"
                      Semi@10..11 ";"
                      Whitespace@11..12 " "
                      Literal@12..15
                        Int@12..15 "100"
                      Whitespace@15..16 " "
                      RBrack@16..17 "]""#]]
        ),
    dynamic_tensor_initializer_with_buffer: ("tensor[0.0; [dynamic_x, dynamic_y]",
            expect![[r#"
                Root@0..34
                  Literal@0..34
                    TensorLit@0..34
                      KwTensor@0..6 "tensor"
                      LBrack@6..7 "["
                      Literal@7..10
                        Float@7..10 "0.0"
                      Semi@10..11 ";"
                      Whitespace@11..12 " "
                      LBrack@12..13 "["
                      DimHints@13..33
                        VarRef@13..22
                          Ident@13..22 "dynamic_x"
                        Comma@22..23 ","
                        Whitespace@23..24 " "
                        VarRef@24..33
                          Ident@24..33 "dynamic_y"
                      RBrack@33..34 "]""#]]
        ),
    complex_tensor_type: ("tensor<i32><batch_number(),_,r,g,b,c>",
            expect![[r#"
                Root@0..37
                  TyTensor@0..37
                    KwTensor@0..6 "tensor"
                    Lt@6..7 "<"
                    TypeHint@7..10
                      TyI32@7..10 "i32"
                    Gt@10..11 ">"
                    Lt@11..12 "<"
                    DimHints@12..36
                      Call@12..26
                        Ident@12..24 "batch_number"
                        LParen@24..25 "("
                        RParen@25..26 ")"
                      Comma@26..27 ","
                      Under@27..28 "_"
                      Comma@28..29 ","
                      VarRef@29..30
                        Ident@29..30 "r"
                      Comma@30..31 ","
                      VarRef@31..32
                        Ident@31..32 "g"
                      Comma@32..33 ","
                      VarRef@33..34
                        Ident@33..34 "b"
                      Comma@34..35 ","
                      VarRef@35..36
                        Ident@35..36 "c"
                    Gt@36..37 ">""#]]
        ),
    }
}
