use crate::{
    operator::starting_precedence,
    parse::{Element, Finished},
    parser::Parser,
};

use syntax::{
    anchor::RollingBackAnchor,
    syntax_kind::SyntaxKind::{self, *},
};

use thin_vec::thin_vec;

#[allow(unused_variables)]
impl Parser<'_> {
    #[allow(unused_variables)]
    pub fn parse_struct_definition(&self) -> Option<Finished> {
        let marker = self.start();
        self.expect_and_bump(KwStruct);
        {
            let rollback_when_dropped = self.dont_recover_and_only_allow(LBrace, Ident);
            // note: currently we don't support generics, because the main point of this project for me is
            // to play with LLVM, MLIR and IR optimizations, generics does not seem to be necessary for now.
            self.expect_and_bump(Ident);
        }
        {
            let rollback_when_dropped = self.dont_recover_and_only_allow(RBrace, LBrace);
            self.expect_and_bump(LBrace);
        }

        self.parse_fields();

        {
            let rollback_when_dropped = self.roll_back_context_after_drop();
            self.allow_only_in_ctx(RBrace);
            self.expect_and_bump(RBrace);
        }

        Some(self.complete_marker_with(marker, StructDef))
    }

    #[allow(unused_variables)]
    pub fn parse_fields(&self) {
        use Element::*;
        let rollback_when_dropped = self.by_expecting(StructAsType);
        let idents = thin_vec![
            Kind(Ident),
            Kind(Colon),
            Branched(
                thin_vec![KindAs(Ident, StructAsType)],
                // thin_vec![Fn(is_of_type)]
                thin_vec![ParseExprWith(starting_precedence())]
            ),
        ];
        self.parse_separated_by(&idents, StructField, Comma, RBrace);
    }

    #[allow(unused_variables)]
    pub fn parse_struct_init_block(&self) {
        self.expect_and_bump(LBrace);

        let rollback_when_dropped = self.roll_back_context_after_drop();
        self.dont_recover_in_ctx(RBrace);

        use Element::*;
        let idents = thin_vec![
            Kind(Ident),
            Kind(Colon),
            ParseExprWith(starting_precedence()),
        ];
        self.parse_separated_by(&idents, StructField, Comma, RBrace);
        self.expect_and_bump(RBrace);
    }

    fn dont_recover_and_only_allow(
        &self,
        dont_recover: SyntaxKind,
        allow_only: SyntaxKind,
    ) -> RollingBackAnchor {
        let rollback_when_dropped = self.roll_back_context_after_drop();
        self.dont_recover_in_ctx(dont_recover);
        self.allow_only_in_ctx(allow_only);
        rollback_when_dropped
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

        basic_struct: ("struct Point { x: i32, y: i32, item: Item }",
            expect![[r#"
                Root@0..43
                  StructDef@0..43
                    KwStruct@0..6 "struct"
                    Whitespace@6..7 " "
                    Ident@7..12 "Point"
                    Whitespace@12..13 " "
                    LBrace@13..14 "{"
                    Whitespace@14..15 " "
                    StructField@15..22
                      Ident@15..16 "x"
                      Colon@16..17 ":"
                      Whitespace@17..18 " "
                      TyI32@18..21 "i32"
                      Comma@21..22 ","
                    Whitespace@22..23 " "
                    StructField@23..30
                      Ident@23..24 "y"
                      Colon@24..25 ":"
                      Whitespace@25..26 " "
                      TyI32@26..29 "i32"
                      Comma@29..30 ","
                    Whitespace@30..31 " "
                    StructField@31..42
                      Ident@31..35 "item"
                      Colon@35..36 ":"
                      Whitespace@36..37 " "
                      StructAsType@37..41 "Item"
                      Whitespace@41..42 " "
                    RBrace@42..43 "}""#]],
        ),
        basic_struct_with_tuple_field: ("struct Point { x: i32, y: i32, item: (Item, Item) }",
            expect![[r#"
                Root@0..51
                  StructDef@0..51
                    KwStruct@0..6 "struct"
                    Whitespace@6..7 " "
                    Ident@7..12 "Point"
                    Whitespace@12..13 " "
                    LBrace@13..14 "{"
                    Whitespace@14..15 " "
                    StructField@15..22
                      Ident@15..16 "x"
                      Colon@16..17 ":"
                      Whitespace@17..18 " "
                      TyI32@18..21 "i32"
                      Comma@21..22 ","
                    Whitespace@22..23 " "
                    StructField@23..30
                      Ident@23..24 "y"
                      Colon@24..25 ":"
                      Whitespace@25..26 " "
                      TyI32@26..29 "i32"
                      Comma@29..30 ","
                    Whitespace@30..31 " "
                    StructField@31..50
                      Ident@31..35 "item"
                      Colon@35..36 ":"
                      Whitespace@36..37 " "
                      Tuple@37..49
                        LParen@37..38 "("
                        StructAsType@38..42 "Item"
                        Comma@42..43 ","
                        Whitespace@43..44 " "
                        StructAsType@44..48 "Item"
                        RParen@48..49 ")"
                      Whitespace@49..50 " "
                    RBrace@50..51 "}""#]],
        ),
        struct_init: ("let origin = Point{ x: 0, y: 0, item: origin_item };",
            expect![[r#"
                Root@0..52
                  LetBinding@0..52
                    KwLet@0..3 "let"
                    Whitespace@3..4 " "
                    InfixBinOp@4..51
                      VarRef@4..11
                        Ident@4..10 "origin"
                        Whitespace@10..11 " "
                      Eq@11..12 "="
                      Whitespace@12..13 " "
                      StructLit@13..51
                        Ident@13..18 "Point"
                        LBrace@18..19 "{"
                        Whitespace@19..20 " "
                        StructField@20..25
                          Ident@20..21 "x"
                          Colon@21..22 ":"
                          Whitespace@22..23 " "
                          Literal@23..24
                            Int@23..24 "0"
                          Comma@24..25 ","
                        Whitespace@25..26 " "
                        StructField@26..31
                          Ident@26..27 "y"
                          Colon@27..28 ":"
                          Whitespace@28..29 " "
                          Literal@29..30
                            Int@29..30 "0"
                          Comma@30..31 ","
                        Whitespace@31..32 " "
                        StructField@32..50
                          Ident@32..36 "item"
                          Colon@36..37 ":"
                          Whitespace@37..38 " "
                          VarRef@38..50
                            Ident@38..49 "origin_item"
                            Whitespace@49..50 " "
                        RBrace@50..51 "}"
                    Semi@51..52 ";""#]],
        ),
        struct_init_with_tuple_field: ("let origin = Point{ x: 0, y: 0, item: (item_a, item_b) };",
            expect![[r#"
                Root@0..57
                  LetBinding@0..57
                    KwLet@0..3 "let"
                    Whitespace@3..4 " "
                    InfixBinOp@4..56
                      VarRef@4..11
                        Ident@4..10 "origin"
                        Whitespace@10..11 " "
                      Eq@11..12 "="
                      Whitespace@12..13 " "
                      StructLit@13..56
                        Ident@13..18 "Point"
                        LBrace@18..19 "{"
                        Whitespace@19..20 " "
                        StructField@20..25
                          Ident@20..21 "x"
                          Colon@21..22 ":"
                          Whitespace@22..23 " "
                          Literal@23..24
                            Int@23..24 "0"
                          Comma@24..25 ","
                        Whitespace@25..26 " "
                        StructField@26..31
                          Ident@26..27 "y"
                          Colon@27..28 ":"
                          Whitespace@28..29 " "
                          Literal@29..30
                            Int@29..30 "0"
                          Comma@30..31 ","
                        Whitespace@31..32 " "
                        StructField@32..55
                          Ident@32..36 "item"
                          Colon@36..37 ":"
                          Whitespace@37..38 " "
                          Tuple@38..54
                            LParen@38..39 "("
                            VarRef@39..45
                              Ident@39..45 "item_a"
                            Comma@45..46 ","
                            Whitespace@46..47 " "
                            VarRef@47..53
                              Ident@47..53 "item_b"
                            RParen@53..54 ")"
                          Whitespace@54..55 " "
                        RBrace@55..56 "}"
                    Semi@56..57 ";""#]],
        ),
    }
}
