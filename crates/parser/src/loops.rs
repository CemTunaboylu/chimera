use crate::{
    operator::starting_precedence,
    parse::{Finished, SeparatedElement},
    parser::Parser,
};

use syntax::{Syntax, anchor::RollingBackAnchor, syntax_kind::SyntaxKind::*};

use thin_vec::thin_vec;

#[allow(unused_variables)]
impl<'input> Parser<'input> {
    #[allow(unused_variables)]
    pub fn parse_while_loop(&self) -> Option<Finished> {
        let marker = self.start();
        self.expect_and_bump(KwWhile);

        {
            let rollback_when_dropped = self.impose_condition_parsing_restrictions_for_while();
            if self.parse_condition().is_none() {
                self.recover_with_msg("while loop expects a condition", "");
            }
        }
        // TODO: does this block return anything?
        self.parse_block();

        Some(self.complete_marker_with(marker, WhileLoop))
    }

    fn impose_condition_parsing_restrictions_for_while(&self) -> RollingBackAnchor {
        let rollback_when_dropped = self.roll_back_context_after_drop();
        let ctx = self.context.borrow();
        ctx.allow([KwTrue, KwFalse, LParen].as_ref());
        ctx.forbid(LBrace);
        ctx.disallow_recovery_of([LBrace].as_ref());
        rollback_when_dropped
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
