#![allow(dead_code)]

use crate::ast;
use smol_str::{SmolStr, ToSmolStr};

use la_arena::{Arena, Idx};
use syntax::language;
use syntax::syntax::SyntaxKind;

// HIR is the high-level intermediate representation that is built on top of the AST

#[derive(Debug)]
pub struct ExprArena {
    pub arena: Arena<Expr>,
    missing_idx: Idx<Expr>,
    stmts: Vec<ast::Stmt>,
}

impl ExprArena {
    pub fn new(root: &ast::Root) -> Self {
        let mut inner = Arena::new();
        let idx = inner.alloc(Expr::Missing);
        let mut vec = root.statements().collect::<Vec<_>>();
        vec.reverse();
        Self {
            arena: inner,
            missing_idx: idx,
            stmts: vec,
        }
    }

    pub fn get_expr(&self, idx: Idx<Expr>) -> &Expr {
        &self.arena[idx]
    }

    fn as_expr_idx(&mut self, exp: Expr) -> Idx<Expr> {
        self.arena.alloc(exp)
    }

    pub fn lower_stmt(&mut self, from: ast::Stmt) -> Option<Stmt> {
        let s = match from {
            ast::Stmt::VarDef(var_def) => Stmt::VarDef {
                name: var_def.name()?.text().to_smolstr(),
                value: self.lower_expr_as_idx(var_def.value().as_ref()),
            },
            ast::Stmt::Expr(expr) => {
                let ix = self.lower_expr_as_idx(Some(&expr));
                Stmt::Expr(self.get_expr(ix).clone())
            }
        };
        Some(s)
    }

    pub fn lower_expr_as_idx(&mut self, from: Option<&ast::Expr>) -> Idx<Expr> {
        if let None = from {
            return self.missing_idx;
        }
        let from = from.unwrap();
        let expr = match from {
            ast::Expr::Paren(paren) => return self.lower_expr_as_idx(paren.expr().as_ref()),
            ast::Expr::Infix(infix) => Expr::Infix(self.lower_infix(infix)),
            ast::Expr::Literal(literal) => Expr::Literal(literal.into()),
            ast::Expr::Unary(unary) => Expr::Unary(self.lower_unary(unary)),
            ast::Expr::VarRef(var_ref) => Expr::VarRef {
                var: var_ref.name(),
            },
        };
        self.as_expr_idx(expr)
    }

    pub fn lower_infix(&mut self, from: &ast::Infix) -> Infix {
        match from {
            ast::Infix::Binary(_) => {
                let op: BinaryOp = from.op().unwrap().into();
                let lhs = self.lower_expr_as_idx(from.lhs().as_ref());
                let rhs = self.lower_expr_as_idx(from.rhs().as_ref());
                Infix::Binary { op, lhs, rhs }
            }
        }
    }

    pub fn lower_unary(&mut self, from: &ast::Unary) -> Unary {
        match from {
            ast::Unary::Prefix(_) => Unary::Prefix {
                op: from.op().unwrap().into(),
                expr: self.lower_expr_as_idx(from.expr().as_ref()),
            },
            ast::Unary::Postfix(_) => todo!(),
        }
    }
}

impl Iterator for &mut ExprArena {
    type Item = Stmt;

    fn next(&mut self) -> Option<Self::Item> {
        let stmt = self.stmts.pop()?;
        self.lower_stmt(stmt)
    }
}

#[derive(Clone, Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Dot,
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
            SyntaxKind::Dot => Self::Dot,
            _ => Self::No,
        }
    }
}

#[derive(Clone, Copy, Debug)]
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

#[derive(Clone, Debug)]
pub enum Infix {
    Binary {
        op: BinaryOp,
        lhs: Idx<Expr>,
        rhs: Idx<Expr>,
    },
}
#[derive(Clone, Debug)]
pub struct Literal(ast::Value);

impl From<&ast::Literal> for Literal {
    fn from(value: &ast::Literal) -> Self {
        Self(value.value())
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Unary {
    Prefix { op: UnaryOp, expr: Idx<Expr> },
    Postfix { op: UnaryOp, expr: Idx<Expr> },
}

impl Unary {
    fn op(&self) -> &UnaryOp {
        match self {
            Self::Prefix { op, .. } => op,
            Self::Postfix { op, .. } => op,
        }
    }
}

#[derive(Clone, Debug)]
pub enum Expr {
    Infix(Infix),
    Literal(Literal),
    Unary(Unary),
    VarRef { var: SmolStr },
    Missing, // handles parser errors
}

#[derive(Debug)]
pub enum Stmt {
    VarDef { name: SmolStr, value: Idx<Expr> },
    // Expr(Idx<Expr>),
    Expr(Expr),
}

pub fn lower<'ast>(ast_root: &'ast ast::Root) -> ExprArena {
    let expr_arena = ExprArena::new(ast_root);
    expr_arena
}
