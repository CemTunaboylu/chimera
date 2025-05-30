use crate::{
    operator::starting_precedence,
    parse::Finished,
    parser::{IsNext, Parser},
};

use syntax::{
    Syntax,
    syntax_kind::SyntaxKind::{self, *},
};

#[allow(unused_variables)]
impl Parser<'_> {
    #[allow(unused_variables)] // for rollback anchor
    pub fn parse_tuple(&self) -> Option<Finished> {
        let rollback_when_dropped = self.impose_restrictions_of_currently_parsing_on_context(Tuple);
        let marker = self.start();
        self.expect_and_bump(LParen);
        while IsNext::No == self.is_next_strict(RParen) {
            self.parse_expression_until_binding_power(starting_precedence());
            self.bump_if(Comma);
        }

        self.expect_and_bump(RParen);
        let finished = self.complete_marker_with(marker, SyntaxKind::Tuple);
        Some(finished)
    }

    #[allow(unused_variables)] // for rollback anchor
    pub fn parse_paren_expr(&self, syntax: Syntax) -> Option<Finished> {
        use SyntaxKind::*;
        let marker = self.start();
        self.expect_and_bump(LParen);
        // restricts ']', '}' (recovers them by erroring), expects and allows ')'
        let rollback_when_dropped = self.impose_restrictions_on_context(syntax);
        _ = self.parse_expression_until_binding_power(starting_precedence());
        self.expect_and_bump(RParen);
        let finished = self.complete_marker_with(marker, SyntaxKind::ParenExpr);
        Some(finished)
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
    create_parser_test! {}
}
