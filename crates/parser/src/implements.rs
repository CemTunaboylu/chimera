use crate::{
    parse::Finished,
    parser::{IsNext, Parser},
};

use syntax::syntax_kind::SyntaxKind::*;

#[allow(unused_variables)]
impl Parser<'_> {
    // impl <struct_identifier> {...}
    #[allow(unused_variables)]
    pub fn parse_impl_block(&self) -> Option<Finished> {
        let marker = self.start();
        self.expect_and_bump(KwImpl);
        {
            let rollback_when_dropped = self.impose_restrictions_of_kind_on_context(Ident);
            self.expect_and_bump_as(Ident, StructAsType);
        }
        {
            let rollback_when_dropped = self.impose_restrictions_of_kind_on_context(LBrace);
            self.expect_and_bump(LBrace);
        }

        self.parse_methods();

        {
            let rollback_when_dropped = self.impose_restrictions_of_kind_on_context(RBrace);
            self.expect_and_bump(RBrace);
        }

        Some(self.complete_marker_with(marker, ImplBlock))
    }

    #[allow(unused_variables)]
    pub fn parse_methods(&self) {
        let rollback_when_dropped = self.impose_restrictions_of_kind_on_context(RBrace);
        while IsNext::No == self.is_next_strict(RBrace) {
            self.parse_function_def();
        }
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
    impl_block_for_struct: ("impl Point { fn translate(&mut self, by: Point) { self.x += by.x; self.y += by.y; }\n}",
               expect![[r#"
                   Root@0..85
                     ImplBlock@0..85
                       KwImpl@0..4 "impl"
                       Whitespace@4..5 " "
                       StructAsType@5..10 "Point"
                       Whitespace@10..11 " "
                       LBrace@11..12 "{"
                       Whitespace@12..13 " "
                       FnDef@13..83
                         KwFn@13..15 "fn"
                         Whitespace@15..16 " "
                         Ident@16..25 "translate"
                         LParen@25..26 "("
                         ParamDecl@26..35
                           PrefixUnaryOp@26..35
                             And@26..27 "&"
                             Mut@27..35
                               KwMut@27..30 "mut"
                               Whitespace@30..31 " "
                               SelfRef@31..35
                                 Kwself@31..35 "self"
                         ParamDecl@35..36
                           Comma@35..36 ","
                         Whitespace@36..37 " "
                         ParamDecl@37..46
                           TypeHint@37..46
                             VarRef@37..39
                               Ident@37..39 "by"
                             Colon@39..40 ":"
                             Whitespace@40..41 " "
                             StructAsType@41..46 "Point"
                         RParen@46..47 ")"
                         Whitespace@47..48 " "
                         Block@48..83
                           LBrace@48..49 "{"
                           Whitespace@49..50 " "
                           Semi@50..65
                             InfixBinOp@50..64
                               InfixBinOp@50..57
                                 SelfRef@50..54
                                   Kwself@50..54 "self"
                                 Dot@54..55 "."
                                 VarRef@55..57
                                   Ident@55..56 "x"
                                   Whitespace@56..57 " "
                               PlusEq@57..59 "+="
                               Whitespace@59..60 " "
                               InfixBinOp@60..64
                                 VarRef@60..62
                                   Ident@60..62 "by"
                                 Dot@62..63 "."
                                 VarRef@63..64
                                   Ident@63..64 "x"
                             Semi@64..65 ";"
                           Whitespace@65..66 " "
                           Semi@66..81
                             InfixBinOp@66..80
                               InfixBinOp@66..73
                                 SelfRef@66..70
                                   Kwself@66..70 "self"
                                 Dot@70..71 "."
                                 VarRef@71..73
                                   Ident@71..72 "y"
                                   Whitespace@72..73 " "
                               PlusEq@73..75 "+="
                               Whitespace@75..76 " "
                               InfixBinOp@76..80
                                 VarRef@76..78
                                   Ident@76..78 "by"
                                 Dot@78..79 "."
                                 VarRef@79..80
                                   Ident@79..80 "y"
                             Semi@80..81 ";"
                           Whitespace@81..82 " "
                           RBrace@82..83 "}"
                       Whitespace@83..84 "\n"
                       RBrace@84..85 "}""#]],
           ),

       }
}
