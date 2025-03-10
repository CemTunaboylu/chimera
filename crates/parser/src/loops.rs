use crate::{
    operator::starting_precedence,
    parse::{Finished, SeparatedElement},
    parser::Parser,
};

use syntax::{Syntax, syntax_kind::SyntaxKind::*};

use thin_vec::thin_vec;

#[allow(unused_variables)]
impl<'input> Parser<'input> {
    #[allow(unused_variables)]
    pub fn parse_while_loop(&self) -> Option<Finished> {
        let marker = self.start();
        self.expect_and_bump(KwWhile);

        {
            let condition_marker = self.start();
            let rollback_when_dropped = self.roll_back_context_after_drop();
            let ctx = self.context.borrow();
            ctx.allow([KwTrue, KwFalse, LParen].as_ref());
            ctx.forbid(LBrace);
            ctx.disallow_recovery_of([LBrace].as_ref());
            if self
                .parse_expression_until_binding_power(starting_precedence())
                .is_none()
            {
                self.recover_with_msg("while loop expects a condition", "");
            }
            self.complete_marker_with(condition_marker, WhileLoopCond);
        }
        // TODO: does this block return anything?
        self.parse_block();

        Some(self.complete_marker_with(marker, WhileLoop))
    }
    #[allow(unused_variables)]
    pub fn parse_for_loop(&self) -> Option<Finished> {
        let marker = self.start();
        self.expect_and_bump(KwFor);

        self.parse_loop_identifiers();
        self.expect_and_bump(KwIn);
        // this can be 0_10, <iterable>, <iterable>.<method>,
        self.parse_expression_until_binding_power(starting_precedence());
        self.parse_block();

        Some(self.complete_marker_with(marker, ForLoop))
    }

    // this can be (<i1>, (<i2>, <i3>)) thus needs its own parsing
    #[allow(unused_variables)]
    pub fn parse_loop_identifiers(&self) {
        if self.is_next(LParen) {
            self.parse_left_hand_side();
        } else {
            // comma separated identifiers
            let rollback_when_dropped = self.roll_back_context_after_drop();
            self.context.borrow().allow(KwIn);

            use SeparatedElement::*;

            let idents = thin_vec![Kind(Ident)];
            self.parse_separated_by(idents, ForIdent, Comma, |syntax: Syntax| {
                !syntax.is_of_kind(KwIn)
            });
        }
    }
}
