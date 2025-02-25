#![allow(dead_code)]

use crate::ast;
use smol_str::SmolStr;

use la_arena::{Arena, Idx};
use syntax::language;
use syntax::syntax::SyntaxKind;

// HIR is the high-level intermediate representation that is built on top of the AST

pub type ExprIdx = Idx<Expr>;
#[derive(Debug)]
pub struct ExprArena {
    pub arena: Arena<Expr>,
    missing_idx: ExprIdx,
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

    pub fn get_expr(&self, idx: ExprIdx) -> &Expr {
        &self.arena[idx]
    }

    fn as_expr_idx(&mut self, exp: Expr) -> ExprIdx {
        self.arena.alloc(exp)
    }

    pub fn lower_stmt(&mut self, from: ast::Stmt) -> Option<Stmt> {
        let s = match from {
            ast::Stmt::VarDef(var_def) => Stmt::VarDef {
                name: var_def.name(),
                value: self.lower_expr_as_idx(var_def.value()),
            },
            ast::Stmt::Expr(expr) => {
                let ix = self.lower_expr_as_idx(Some(&expr));
                Stmt::Expr(self.get_expr(ix).clone())
            }
        };
        Some(s)
    }

    pub fn lower_expr_as_idx(&mut self, from: Option<&ast::Expr>) -> ExprIdx {
        if from.is_none() {
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
            ast::Unary::Postfix(_) => Unary::Postfix {
                op: from.op().unwrap().into(),
                expr: self.lower_expr_as_idx(from.expr().as_ref()),
            },
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

#[derive(Clone, Debug, PartialEq)]
pub enum BinaryOp {
    Add,
    Div,
    Dot,
    Eq,
    Mul,
    No,
    Sub,
}

impl From<language::SyntaxToken> for BinaryOp {
    fn from(value: language::SyntaxToken) -> Self {
        let kind = value.kind();
        match kind {
            SyntaxKind::Dot => Self::Dot,
            SyntaxKind::Eq => Self::Eq,
            SyntaxKind::Minus => Self::Sub,
            SyntaxKind::Plus => Self::Add,
            SyntaxKind::Slash => Self::Div,
            SyntaxKind::Star => Self::Mul,
            _ => Self::No,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum UnaryOp {
    Neg,
    Not,
    Factorial,
    No,
}

impl From<language::SyntaxToken> for UnaryOp {
    fn from(value: language::SyntaxToken) -> Self {
        let kind = value.kind();
        match kind {
            SyntaxKind::Minus => Self::Neg,
            SyntaxKind::Not => Self::Not,
            SyntaxKind::Exclamation => Self::Factorial,
            _ => Self::No,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Infix {
    Binary {
        op: BinaryOp,
        lhs: ExprIdx,
        rhs: ExprIdx,
    },
}
#[derive(Clone, Debug, PartialEq)]
pub struct Literal(Option<ast::Value>);

impl From<&ast::Literal> for Literal {
    fn from(value: &ast::Literal) -> Self {
        Self(value.value().ok())
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Unary {
    Prefix { op: UnaryOp, expr: ExprIdx },
    Postfix { op: UnaryOp, expr: ExprIdx },
}

impl Unary {
    fn op(&self) -> &UnaryOp {
        match self {
            Self::Prefix { op, .. } => op,
            Self::Postfix { op, .. } => op,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Infix(Infix),
    Literal(Literal),
    Unary(Unary),
    VarRef { var: SmolStr },
    Missing, // handles parser errors
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
    VarDef { name: SmolStr, value: ExprIdx },
    // Expr(ExprIdx),
    Expr(Expr),
}

pub fn lower(ast_root: &ast::Root) -> ExprArena {
    ExprArena::new(ast_root)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{parse_behaviors::IgnoreTrivia, parser::Parser};
    use la_arena::RawIdx;

    fn parse(program: &str) -> ast::Root {
        let parse = Parser::new(program).parse::<IgnoreTrivia>();
        ast::Root::try_from(parse.syntax_node_root()).unwrap()
    }

    fn idx(from: u32) -> ExprIdx {
        Idx::from_raw(RawIdx::from_u32(from))
    }

    const MISSING: u32 = 0;

    #[test]
    fn lower_variable_def() {
        let root = parse("let foo = bar");
        let hir = lower(&root).into_iter().next().unwrap();

        assert_eq!(
            hir,
            Stmt::VarDef {
                name: "foo".into(),
                value: idx(1)
            }
        );
    }

    #[test]
    fn lower_infix_binary_expr() {
        let root = parse("3 + 14");
        let hir = lower(&root).into_iter().next().unwrap();

        assert_eq!(
            hir,
            Stmt::Expr(Expr::Infix(Infix::Binary {
                op: BinaryOp::Add,
                lhs: idx(1),
                rhs: idx(2),
            }))
        );
    }

    #[test]
    fn lower_literal_expr() {
        let root = parse("\"pi\"");
        let hir = lower(&root).into_iter().next().unwrap();

        assert_eq!(
            hir,
            Stmt::Expr(Expr::Literal(Literal(Some(ast::Value::Str(
                "\"pi\"".into()
            ))))),
        );
    }

    #[test]
    fn lower_unary_prefix_expr() {
        let root = parse("~ok");
        let hir = lower(&root).into_iter().next().unwrap();

        assert_eq!(
            hir,
            Stmt::Expr(Expr::Unary(Unary::Prefix {
                op: UnaryOp::Not,
                expr: idx(1)
            })),
        );
    }

    #[test]
    fn lower_unary_postfix_expr() {
        let root = parse("9!");
        let hir = lower(&root).into_iter().next().unwrap();

        assert_eq!(
            hir,
            Stmt::Expr(Expr::Unary(Unary::Postfix {
                op: UnaryOp::Factorial,
                expr: idx(1)
            })),
        );
    }

    #[test]
    fn lower_var_ref_expr() {
        let root = parse("var");
        let hir = lower(&root).into_iter().next().unwrap();

        assert_eq!(hir, Stmt::Expr(Expr::VarRef { var: "var".into() }));
    }

    #[test]
    fn lower_malformed_var_def_no_value() {
        let root = parse("let a = ");
        let hir = lower(&root).into_iter().next().unwrap();

        assert_eq!(
            hir,
            Stmt::VarDef {
                name: "a".into(),
                value: idx(MISSING)
            }
        );
    }

    #[test]
    fn lower_malformed_var_def_no_name() {
        let root = parse("let = 9");
        let hir = lower(&root).into_iter().next();
        assert_eq!(hir, None);
    }

    #[test]
    fn lower_infix_binary_expr_without_lhs() {
        let root = parse(" +3");
        let hir = lower(&root).into_iter().next().unwrap();

        assert_eq!(
            hir,
            Stmt::Expr(Expr::Literal(Literal(Some(ast::Value::Num(3)))))
        );
    }
    #[test]
    fn lower_infix_binary_expr_without_rhs() {
        let root = parse("3+ ");
        let hir = lower(&root).into_iter().next().unwrap();

        assert_eq!(
            hir,
            Stmt::Expr(Expr::Infix(Infix::Binary {
                op: BinaryOp::Add,
                lhs: idx(1),
                rhs: idx(MISSING),
            }))
        );
    }

    #[test]
    fn lower_unary_prefix_expr_without_rhs() {
        let root = parse("- ");
        let hir = lower(&root).into_iter().next().unwrap();

        assert_eq!(
            hir,
            Stmt::Expr(Expr::Unary(Unary::Prefix {
                op: UnaryOp::Neg,
                expr: idx(MISSING),
            })),
        );
    }

    #[test]
    fn lower_unary_postfix_expr_without_rhs() {
        let root = parse(" ! ");
        let hir = lower(&root).into_iter().next();

        assert_eq!(hir, None);
    }

    #[test]
    fn lower_literal_expr_overflowed_usize() {
        let root = parse("18446744073709551616");
        let hir = lower(&root).into_iter().next().unwrap();

        assert_eq!(hir, Stmt::Expr(Expr::Literal(Literal(None))),);
    }
}
