use miette::Diagnostic;
use thiserror::Error;

use crate::{ast, errors::Inner};

#[derive(Clone, Diagnostic, Debug, PartialEq, Error)]
#[error("LiteralValueValidationError")]
pub struct LiteralValueParseError {
    #[source]
    #[diagnostic_source]
    cause: Inner,
}

// fn validate_parsable(root: &ast::Root) -> Option<Vec<LiteralValueParseError>> {
//     let src = root.0.text().to_string();

//     let mut errors = vec![];
//     let mut stack = vec![root.0];

//     while !stack.is_empty() {
//         let syntax_node = stack.pop().unwrap();
//         match ast::Stmt::try_from(syntax_node){
//             Ok(stmt) => validate_parsable_stmt(&stmt),
//             Err(err) => errors.push(err),
//         }

//     }
//         None

// }

// fn validate_parsable_stmt(stmt: &ast::Stmt) -> Some(LiteralValueParseError) {
//     match stmt {
//         ast::Stmt::VarDef(var_def) => {
//             var_def.value().is_some_and(|var_def_expr|)
//         },
//         ast::Stmt::Expr(expr) => todo!(),
//     }

// }

// fn validate_parsable_expr(expr: &ast::Expr) -> Some(LiteralValueParseError) {
//     match expr {
//         ast::Expr::Infix(infix) => todo!(),
//         ast::Expr::Literal(literal) => literal.value(),
//         ast::Expr::Paren(paren) => todo!(),
//         ast::Expr::Unary(unary) => todo!(),
//     }

// }
