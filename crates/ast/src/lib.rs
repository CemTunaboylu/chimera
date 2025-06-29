use ast::Root;
use parser::parser::Parser;
use syntax::language::{SyntaxNode, SyntaxToken};

use std::fmt::Debug;

pub mod ast;
pub mod container;
pub mod control_flow;
pub mod delimited;
pub mod errors;
pub mod expression;
pub mod function;
pub mod impl_block;
pub mod indexing;
pub mod jump;
mod lang_elems;
pub mod let_binding;
pub mod literal;
pub mod loops;
pub mod mutable;
pub mod operation;
pub mod parameter;
pub mod return_stmt;
pub mod self_ref;
pub mod semi;
pub mod statement;
pub mod structure;
pub mod types;

pub fn ast_root_from(program: &str) -> Root {
    let root = Parser::new(program).parse();
    Root::try_from(root).expect("should have been ok")
}

pub fn cast_node_into_type<'caller, T>(parent_node: &'caller SyntaxNode) -> T
where
    T: TryFrom<&'caller SyntaxNode>,
    <T as TryFrom<&'caller SyntaxNode>>::Error: Debug,
{
    let _desired_type: T = parent_node.try_into().unwrap_or_else(|_| {
        panic!(
            "try_into should have been successful for {:?}",
            parent_node.text()
        )
    });
    _desired_type
}
pub fn cast_token_into_type<'caller, T>(token: &'caller SyntaxToken) -> T
where
    T: TryFrom<&'caller SyntaxToken>,
    <T as TryFrom<&'caller SyntaxToken>>::Error: Debug,
{
    token.try_into().unwrap_or_else(|_| {
        panic!(
            "try_into should have been successful for {:?}",
            token.text()
        )
    })
}
