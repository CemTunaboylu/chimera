#![allow(dead_code)]
use crate::ast;
use smol_str::{SmolStr, ToSmolStr};

use syntax::language;
use syntax::syntax::SyntaxKind;

// HIR is the high-level intermediate representation that is built on top of the AST

#[derive(Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    No,
}

impl From<language::SyntaxToken> for BinaryOp {
    fn from(value: language::SyntaxToken) -> Self {
        let kind = value.kind();
        match kind {
            SyntaxKind::Plus => Self::Add,
            SyntaxKind::Minus => Self::Sub,
            SyntaxKind::Star => Self::Mul,
            SyntaxKind::Slash => Self::Div,
            _ => Self::No,
        }
    }
}

#[derive(Debug)]
pub enum UnaryOp {
    Neg,
    Not,
    No,
}

impl From<language::SyntaxToken> for UnaryOp {
    fn from(value: language::SyntaxToken) -> Self {
        let kind = value.kind();
        match kind {
            SyntaxKind::Minus => Self::Neg,
            SyntaxKind::Not => Self::Not,
            _ => Self::No,
        }
    }
}

#[derive(Debug)]
pub enum Infix {
    Binary {
        op: BinaryOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
}

impl From<ast::Infix> for Infix {
    fn from(value: ast::Infix) -> Self {
        match value.clone() {
            ast::Infix::Binary(_) => {
                let op: BinaryOp = value.op().unwrap().into();
                let lhs = Box::new(value.lhs().map_or(Expr::Missing, |e| e.into()));
                let rhs = Box::new(value.rhs().map_or(Expr::Missing, |e| e.into()));
                Self::Binary { op, lhs, rhs }
            }
        }
    }
}

#[derive(Debug)]
pub struct Literal(ast::Value);

impl From<ast::Literal> for Literal {
    fn from(value: ast::Literal) -> Self {
        Self(value.value())
    }
}

#[derive(Debug)]
pub enum Unary {
    Prefix { op: UnaryOp, expr: Box<Expr> },
    Postfix { op: UnaryOp, expr: Box<Expr> },
}

impl Unary {
    fn op(&self) -> &UnaryOp {
        match self {
            Self::Prefix { op, .. } => op,
            Self::Postfix { op, .. } => op,
        }
    }
}

impl From<ast::Unary> for Unary {
    fn from(value: ast::Unary) -> Self {
        match value {
            ast::Unary::Prefix(_) => Self::Prefix {
                op: value.op().unwrap().into(),
                expr: Box::new(value.expr().map_or(Expr::Missing, |e| e.into())),
            },
            ast::Unary::Postfix(_) => todo!(),
        }
    }
}

#[derive(Debug)]
pub enum Expr {
    Infix(Infix),
    Literal(Literal),
    Unary(Unary),
    VarRef { var: SmolStr },
    Missing, // handles parser errors
}
impl From<ast::Expr> for Expr {
    fn from(value: ast::Expr) -> Self {
        match value {
            ast::Expr::Infix(infix) => Self::Infix(infix.into()),
            ast::Expr::Literal(literal) => Self::Literal(literal.into()),
            ast::Expr::Paren(paren) => paren.expr().map_or(Self::Missing, |e| e.into()),
            ast::Expr::Unary(unary) => Self::Unary(unary.into()),
            ast::Expr::VarRef(var_ref) => Self::VarRef {
                var: var_ref.name(),
            },
        }
    }
}

#[derive(Debug)]
pub enum Stmt {
    VarDef { name: SmolStr, value: Expr },
    Expr(Expr),
}

impl From<ast::Stmt> for Stmt {
    fn from(value: ast::Stmt) -> Self {
        match value {
            ast::Stmt::VarDef(var_def) => Self::VarDef {
                name: var_def.name().unwrap().text().to_smolstr(),
                value: var_def.value().unwrap().into(),
            },
            ast::Stmt::Expr(expr) => Self::Expr(expr.into()),
        }
    }
}
fn from_opted<S: From<ast::Stmt>>(node: ast::Stmt) -> Option<S> {
    Some(S::from(node))
}

pub fn lower<'ast>(ast_root: &'ast ast::Root) -> impl Iterator<Item = Stmt> {
    ast_root.statements().filter_map(from_opted::<Stmt>)
}
