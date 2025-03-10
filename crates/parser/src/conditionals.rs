use crate::{parse::Finished, parser::Parser};

use syntax::{anchor::RollingBackAnchor, syntax_kind::SyntaxKind::*};

#[allow(unused_variables)]
impl<'input> Parser<'input> {
    #[allow(unused_variables)]
    pub fn parse_conditionals(&self) -> Option<Finished> {
        let marker = self.start();
        self.expect_and_bump(KwIf);

        {
            let anchor = self.impose_condition_parsing_restrictions_for_control_flow();
            if self.parse_condition().is_none() {
                self.recover_with_msg("if expects a condition", "");
            }
        }

        self.parse_block();

        while self.is_next(KwElif) {
            self.expect_and_bump(KwElif);
            {
                let anchor = self.impose_condition_parsing_restrictions_for_control_flow();
                if self.parse_condition().is_none() {
                    self.recover_with_msg("elif expects a condition", "");
                }
            }
            self.parse_block();
        }

        if self.is_next(KwElse) {
            self.expect_and_bump(KwElse);
            self.parse_block();
        }

        let finished_as_expr = self.complete_marker_with(marker, ControlFlow);
        // if self.is_next(Semi) {
        //     let semi_marker = self.precede_marker_with(&finished_as_expr);
        //     self.expect_and_bump(Semi);
        //     Some(self.complete_marker_with(semi_marker, Semi))
        // } else {
        //     Some(finished_as_expr)
        // }
        Some(finished_as_expr)
    }

    pub fn impose_condition_parsing_restrictions_for_control_flow(&self) -> RollingBackAnchor {
        let rollback_after_drop = self.roll_back_context_after_drop();
        let ctx = self.context.borrow();
        ctx.allow([KwTrue, KwFalse, LParen].as_ref());
        ctx.forbid(LBrace);
        ctx.disallow_recovery_of([LBrace, KwElif, KwElse].as_ref());
        rollback_after_drop
    }
}
