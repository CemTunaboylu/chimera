use crate::{
    parse::{Finished, SeparatedElement},
    parser::Parser,
};

use lexer::token_type::TokenType;
use syntax::{Syntax, syntax_kind::SyntaxKind::*};

use thin_vec::thin_vec;

fn ident_or_type(syntax: Syntax) -> bool {
    matches!(
        syntax.get_token_type(),
        TokenType::Type | TokenType::Identifier
    )
}

#[allow(unused_variables)]
impl<'input> Parser<'input> {
    #[allow(unused_variables)]
    pub fn parse_struct_definition(&self) -> Option<Finished> {
        let marker = self.start();
        self.expect_and_bump(KwStruct);
        {
            let rollback_when_dropped = self.roll_back_context_after_drop();
            let ctx = self.context.borrow();
            ctx.disallow_recovery_of(LBrace);
            ctx.allow_only(Ident);
            // note: currently we don't support generics, because the main point of this project for me is
            // to play with LLVM, MLIR and IR optimizations, generics does not seem to be necessary for now.
            self.expect_and_bump(Ident);
        }
        {
            let rollback_when_dropped = self.roll_back_context_after_drop();
            let ctx = self.context.borrow();
            ctx.disallow_recovery_of(RBrace);
            ctx.allow_only(LBrace);
            self.expect_and_bump(LBrace);
        }

        self.parse_attributes();

        {
            let rollback_when_dropped = self.roll_back_context_after_drop();
            let ctx = self.context.borrow();
            ctx.allow_only(RBrace);
            self.expect_and_bump(RBrace);
        }

        Some(self.complete_marker_with(marker, StructDef))
    }

    #[allow(unused_variables)]
    pub fn parse_attributes(&self) -> Option<Finished> {
        let marker = self.start();

        use SeparatedElement::*;

        let idents = thin_vec![Kind(Ident), Kind(Colon), Fn(ident_or_type)];
        self.parse_separated_by(&idents, StructAttr, Comma, |syntax: Syntax| {
            !syntax.is_of_kind(RBrace)
        });

        Some(self.complete_marker_with(marker, StructAttrs))
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
                Root@0..41
                  StructDef@0..41
                    KwStruct@0..6 "struct"
                    Whitespace@6..7 " "
                    Ident@7..12 "Point"
                    Whitespace@12..13 " "
                    LBrace@13..14 "{"
                    StructAttrs@14..40
                      Whitespace@14..15 " "
                      StructAttr@15..21
                        Ident@15..16 "x"
                        Colon@16..17 ":"
                        Whitespace@17..18 " "
                        TyI32@18..21 "i32"
                      Whitespace@21..22 " "
                      StructAttr@22..28
                        Ident@22..23 "y"
                        Colon@23..24 ":"
                        Whitespace@24..25 " "
                        TyI32@25..28 "i32"
                      Whitespace@28..29 " "
                      StructAttr@29..40
                        Ident@29..33 "item"
                        Colon@33..34 ":"
                        Whitespace@34..35 " "
                        Ident@35..39 "Item"
                        Whitespace@39..40 " "
                    RBrace@40..41 "}""#]],
        ),
    }
}
