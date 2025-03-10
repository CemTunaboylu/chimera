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
        self.parse_separated_by(idents, StructAttr, Comma, |syntax: Syntax| {
            !syntax.is_of_kind(RBrace)
        });

        Some(self.complete_marker_with(marker, StructAttrs))
    }
}
