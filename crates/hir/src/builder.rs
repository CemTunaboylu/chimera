use std::fmt::Debug;

use la_arena::{Arena, Idx};
use miette::Report;
use smol_str::SmolStr;
use thin_vec::ThinVec;

use ast::{ast::Root as ASTRoot, statement::Stmt as ASTStmt};

use crate::{
    HIRResult,
    container::canonical::CanonicalBuffer,
    context::{LoweringContext, UsageContext},
    scope::{
        ContainerLiteralIdx, NameIndexed, Scope, ScopeIdx, ScopeKind, Selector, Span, StrIdx,
        into_idx,
    },
    statement::Stmt,
};

// HIR is the high-level intermediate representation that is built on top of the AST
#[derive(Debug)]
pub struct HIRBuilder {
    pub(crate) ast_root: ASTRoot,
    pub(crate) contexts: ThinVec<LoweringContext>,
    pub(crate) current_scope_cursor: ScopeIdx,
    pub(crate) errors: ThinVec<Report>,
    pub(crate) lowered: ThinVec<Stmt>,
    pub(crate) scopes: Arena<Scope>,
    stmts: ThinVec<ASTStmt>,
}

// Note: I may switch to LOUDS trie for some, measure the performance gain.
impl HIRBuilder {
    pub fn new(ast_root: ASTRoot) -> Self {
        let contexts = ThinVec::<LoweringContext>::new();

        let current_scope_cursor = into_idx::<Scope>(0);
        let errors = ThinVec::<Report>::new();

        let mut scopes = Arena::<Scope>::new();
        let master_scope = Scope::new(current_scope_cursor, ScopeKind::Master);
        assert_eq!(current_scope_cursor, scopes.alloc(master_scope));

        let lowered = ThinVec::<Stmt>::new();

        let mut stmts = ast_root.statements().collect::<ThinVec<_>>();
        stmts.reverse();

        Self {
            ast_root,
            contexts,
            current_scope_cursor,
            errors,
            lowered,
            scopes,
            stmts,
        }
    }
    pub fn start_new_scope(&mut self, with: ScopeKind) -> ScopeIdx {
        let new_scope = Scope::new(self.current_scope_cursor, with);
        self.current_scope_cursor = self.scopes.alloc(new_scope);
        self.current_scope_cursor
    }
    pub fn end_new_scope(&mut self) {
        let current_scope = &self.scopes[self.current_scope_cursor];
        self.current_scope_cursor = current_scope.parent;
    }
    pub fn push_usage_context(&mut self, usage: UsageContext) {
        let lowering_ctx = LoweringContext {
            usage,
            statement_idx: self.lowered.len(),
            scope_idx: self.current_scope_cursor,
        };
        self.contexts.push(lowering_ctx);
    }
    pub fn get_context(&self) -> Option<&LoweringContext> {
        self.contexts.last()
    }
    pub fn get_current_stmt_idx(&self) -> usize {
        self.lowered.len()
    }
    pub fn pop_usage_context(&mut self) {
        self.contexts.pop();
    }
    pub fn get_current_scope_mut(&mut self) -> &mut Scope {
        &mut self.scopes[self.current_scope_cursor]
    }
    pub fn get_current_scope(&self) -> &Scope {
        &self.scopes[self.current_scope_cursor]
    }
    pub fn allocate_span(&mut self, name: &SmolStr, span: Span) {
        let current_scope = self.get_current_scope_mut();
        current_scope.allocate_span(name, span);
    }
    pub fn allocate_string(&mut self, string: SmolStr) -> StrIdx {
        let current_scope = self.get_current_scope_mut();
        current_scope.allocate_string(string)
    }
    pub fn allocate_tensor_literal(
        &mut self,
        tensor_literal: CanonicalBuffer,
    ) -> ContainerLiteralIdx {
        let current_scope = self.get_current_scope_mut();
        current_scope.allocate_tensor_literal(tensor_literal)
    }
    // TODO: use context for metadata population
    pub fn allocate<E, S: Selector<E>>(&mut self, name: SmolStr, elm: E) -> HIRResult<Idx<E>>
    where
        E: Clone + Debug + PartialEq + NameIndexed,
    {
        let current_scope = self.get_current_scope_mut();
        current_scope.allocate::<E, S>(name, elm)
    }
}

impl Iterator for &mut HIRBuilder {
    type Item = Stmt;

    fn next(&mut self) -> Option<Self::Item> {
        let stmt = self.stmts.pop()?;
        let stmt = self.lower_statement(&stmt);
        match stmt {
            Ok(lowered) => {
                self.lowered.push(lowered.clone());
                Some(lowered)
            }
            Err(err) => {
                self.errors.push(err);
                self.next()
            }
        }
    }
}

pub fn lower(ast_root: ASTRoot) -> HIRBuilder {
    HIRBuilder::new(ast_root)
}

#[cfg(test)]
pub(crate) mod tests {
    use parser::parser::Parser;

    use ast::ast::Root as ASTRoot;

    pub(crate) fn ast_root_from(program: &str) -> ASTRoot {
        let root = Parser::new(program).parse();
        println!("debug_tree: {:?}", root.debug_tree());
        ASTRoot::try_from(root).expect("should have been ok")
    }
    /*
        fn parse(program: &str) -> AstRoot {
            let parse = Parser::new(program).parse();
            AstRoot::try_from(parse).unwrap()
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

            assert_eq!(hir, Stmt::Expr(Expr::Literal(Literal(AstValue::Str(0..2)))),);
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
            let root = parse("option?");
            let hir = lower(&root).into_iter().next().unwrap();

            assert_eq!(
                hir,
                Stmt::Expr(Expr::Unary(Unary::Postfix {
                    op: UnaryOp::CondUnwrap,
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

            assert_eq!(hir, Stmt::Expr(Expr::Literal(Literal(AstValue::Int(2..3)))));
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
            assert!(lower(&root).into_iter().next().is_none());
        }
    */
}
