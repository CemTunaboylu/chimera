use crate::{
    parse::Finished,
    parser::{IsNext, Parser},
};

use syntax::{anchor::RollingBackAnchor, syntax_kind::SyntaxKind::*};

#[allow(unused_variables)]
impl Parser<'_> {
    // impl <struct_identifier> {...}
    #[allow(unused_variables)]
    pub fn parse_impl_block(&self) -> Option<Finished> {
        let marker = self.start();
        self.expect_and_bump(KwImpl);
        {
            let rollback_when_dropped = self.impose_restrictions_for_identifier();
            self.expect_and_bump_as(Ident, StructAsType);
        }
        {
            let rollback_when_dropped = self.impose_restrictions_for_lbrace();
            self.expect_and_bump(LBrace);
        }

        self.parse_methods();

        {
            let rollback_when_dropped = self.impose_restrictions_for_rbrace();
            self.expect_and_bump(RBrace);
        }

        Some(self.complete_marker_with(marker, ImplBlock))
    }

    fn impose_restrictions_for_identifier(&self) -> RollingBackAnchor {
        let rollback_when_dropped = self.roll_back_context_after_drop();
        let ctx = self.context.borrow();
        ctx.disallow_recovery_of(LBrace);
        ctx.allow_only(Ident);
        rollback_when_dropped
    }

    fn impose_restrictions_for_lbrace(&self) -> RollingBackAnchor {
        let rollback_when_dropped = self.roll_back_context_after_drop();
        let ctx = self.context.borrow();
        ctx.disallow_recovery_of(RBrace);
        ctx.allow_only(LBrace);
        rollback_when_dropped
    }

    fn impose_restrictions_for_rbrace(&self) -> RollingBackAnchor {
        let rollback_when_dropped = self.roll_back_context_after_drop();
        let ctx = self.context.borrow();
        ctx.allow_only(RBrace);
        rollback_when_dropped
    }

    #[allow(unused_variables)]
    pub fn parse_methods(&self) {
        let rollback_when_dropped = self.roll_back_context_after_drop();
        self.dont_recover_in_ctx(RBrace);

        while IsNext::No == self.is_next_strict(RBrace) {
            println!("next: {:?}", self.peek());
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
                   Root@0..84
                     ImplBlock@0..84
                       KwImpl@0..4 "impl"
                       Whitespace@4..5 " "
                       StructAsType@5..10 "Point"
                       Whitespace@10..11 " "
                       LBrace@11..12 "{"
                       Whitespace@12..13 " "
                       FnDef@13..82
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
                         Whitespace@35..36 " "
                         ParamDecl@36..45
                           Ident@36..38 "by"
                           Colon@38..39 ":"
                           Whitespace@39..40 " "
                           StructAsType@40..45 "Point"
                         RParen@45..46 ")"
                         Whitespace@46..47 " "
                         Block@47..82
                           LBrace@47..48 "{"
                           Whitespace@48..49 " "
                           Semi@49..64
                             InfixBinOp@49..63
                               InfixBinOp@49..56
                                 SelfRef@49..53
                                   Kwself@49..53 "self"
                                 Dot@53..54 "."
                                 VarRef@54..56
                                   Ident@54..55 "x"
                                   Whitespace@55..56 " "
                               PlusEq@56..58 "+="
                               Whitespace@58..59 " "
                               InfixBinOp@59..63
                                 VarRef@59..61
                                   Ident@59..61 "by"
                                 Dot@61..62 "."
                                 VarRef@62..63
                                   Ident@62..63 "x"
                             Semi@63..64 ";"
                           Whitespace@64..65 " "
                           Semi@65..80
                             InfixBinOp@65..79
                               InfixBinOp@65..72
                                 SelfRef@65..69
                                   Kwself@65..69 "self"
                                 Dot@69..70 "."
                                 VarRef@70..72
                                   Ident@70..71 "y"
                                   Whitespace@71..72 " "
                               PlusEq@72..74 "+="
                               Whitespace@74..75 " "
                               InfixBinOp@75..79
                                 VarRef@75..77
                                   Ident@75..77 "by"
                                 Dot@77..78 "."
                                 VarRef@78..79
                                   Ident@78..79 "y"
                             Semi@79..80 ";"
                           Whitespace@80..81 " "
                           RBrace@81..82 "}"
                       Whitespace@82..83 "\n"
                       RBrace@83..84 "}""#]],
           ),

       }
}
