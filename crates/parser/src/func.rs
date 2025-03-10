use crate::{
    marker::{Incomplete, Marker},
    operator::starting_precedence,
    parse::{Finished, SeparatedElement},
    parser::Parser,
};
use lexer::token_type::TokenType;
use syntax::{Syntax, syntax_kind::SyntaxKind};

use SyntaxKind::*;
use thin_vec::thin_vec;

fn ident_or_type(syntax: Syntax) -> bool {
    matches!(
        syntax.get_token_type(),
        TokenType::Type | TokenType::Identifier
    )
}

impl<'input> Parser<'input> {
    // fn <ident>({parameters as CSV}) {-> RetType} {}
    #[allow(unused_variables)]
    pub fn parse_function_def(&self) -> Option<Finished> {
        let marker = self.start();
        self.expect_and_bump(KwFn);
        {
            let rollback_after_drop = self.roll_back_context_after_drop();
            self.context.borrow().disallow_recovery_of(LParen);
            self.expect_and_bump(Ident);
        }
        self.expect_and_bump(LParen);
        {
            let rollback_after_drop = self.roll_back_context_after_drop();
            self.context.borrow().disallow_recovery_of(RParen);
            self.parse_comma_separated_typed_declarations_until(|syntax: Syntax| {
                !syntax.is_of_kind(RParen)
            });
        }
        self.clean_buffer();

        let rollback_after_drop = self.roll_back_context_after_drop();
        self.context.borrow().disallow_recovery_of(LBrace);
        self.expect_and_bump(RParen);

        if self.is_next(RArrow) {
            let ret_type_marker = self.start();
            self.expect_and_bump(RArrow);
            self.expect_f_and_bump(ident_or_type);
            self.complete_marker_with(ret_type_marker, RetType);
        }

        self.parse_block();
        Some(self.complete_marker_with(marker, SyntaxKind::FnDef))
    }

    // the identifier is already bumped at this point because we ended up here
    // from parsing an identifier and saw a LParen as well.
    pub fn parse_function_call(&self, marker: Marker<Incomplete>) -> Finished {
        self.expect_and_bump(LParen);
        self.parse_comma_separated_arguments_until(|syntax: Syntax| !syntax.is_of_kind(RParen));
        self.expect_and_bump(RParen);
        self.complete_marker_with(marker, FnCall)
    }

    #[allow(unused_variables)]
    pub fn parse_comma_separated_typed_declarations_until(
        &self,
        until: fn(Syntax) -> bool,
    ) -> Option<()> {
        let rollback_when_dropped = self.roll_back_context_after_drop();
        let ctx = self.context.borrow();
        ctx.forbid_all();
        ctx.allow([RParen, Colon, Comma, Ident].as_ref());
        ctx.allow(SyntaxKind::types().as_ref());
        ctx.disallow_recovery_of(RParen);

        use SeparatedElement::*;

        let exps_ident_and_colon = thin_vec![Kind(Ident), Kind(Colon), Fn(ident_or_type)];
        self.parse_separated_by(exps_ident_and_colon, ParamDecl, Comma, until)
    }

    #[allow(unused_variables)]
    pub fn parse_comma_separated_arguments_until(&self, until: fn(Syntax) -> bool) -> Option<()> {
        let rollback_when_dropped = self.roll_back_context_after_drop();
        self.context.borrow().allow(RParen);

        use SeparatedElement::*;

        self.parse_separated_by(
            thin_vec![ParseExprWith(starting_precedence())],
            FnArg,
            Comma,
            until,
        )
    }
}
