use crate::{
    marker::{Incomplete, Marker},
    operator::starting_precedence,
    parse::Finished,
    parser::Parser,
};

use syntax::syntax_kind::SyntaxKind::*;

#[allow(unused_variables)]
impl<'input> Parser<'input> {
    // the identifier is already bumped at this point because we ended up here
    // from parsing an identifier and saw a LBrack as well.
    #[allow(unused_variables)]
    pub fn parse_container_indexing(&self, marker: Marker<Incomplete>) -> Finished {
        self.expect_and_bump(LBrack);
        self.parse_expression_until_binding_power(starting_precedence());
        self.expect_and_bump(RBrack);
        self.complete_marker_with(marker, ContainerRef)
    }
}
