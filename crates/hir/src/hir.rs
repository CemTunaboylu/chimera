#![allow(dead_code)]

use miette::Report;
use smol_str::SmolStr;

use ast::{ast::Root as ASTRoot, statement::Stmt as ASTStmt};

use la_arena::{Arena, Idx, RawIdx};
use patricia_tree::PatriciaMap;
use thin_vec::ThinVec;

use crate::{
    expression::Expr,
    function::{FnDef, UnresolvedFnCall},
    statement::Stmt,
    tensor::CanonicalTensor,
    variable::{UnresolvedVarRef, VarDef},
};

// HIR is the high-level intermediate representation that is built on top of the AST

pub type ExprIdx = Idx<Expr>;
pub type FnDefIdx = Idx<FnDef>;
pub type StrIdx = Idx<SmolStr>;
pub type TensorLiteralIdx = Idx<CanonicalTensor>;
pub type UnresolvedFnCallIdx = Idx<UnresolvedFnCall>;
pub type UnresolvedVarRefIdx = Idx<UnresolvedVarRef>;
pub type VarDefIdx = Idx<VarDef>;

pub type NameToIndexTrie<T> = PatriciaMap<T>;

pub const MISSING: u32 = 0;

pub(crate) fn into_idx<T>(from: u32) -> Idx<T> {
    Idx::from_raw(RawIdx::from_u32(from))
}

pub struct HIRBuilder {
    pub(crate) expr_arena: Arena<Expr>,

    pub(crate) var_names: Arena<SmolStr>,
    pub(crate) var_defs: Arena<VarDef>,
    pub(crate) var_name_to_idx_trie: NameToIndexTrie<VarDefIdx>,
    pub(crate) vars_to_resolve: Arena<UnresolvedVarRef>,

    pub(crate) str_arena: Arena<SmolStr>,
    pub(crate) tensor_arena: Arena<CanonicalTensor>,

    pub(crate) fn_arena: Arena<FnDef>,
    pub(crate) fn_names: Arena<SmolStr>,
    pub(crate) fn_name_to_idx_trie: NameToIndexTrie<FnDefIdx>,
    pub(crate) fn_to_resolve: Arena<UnresolvedFnCall>,

    // pub(crate) tensor_arena: Arena<>,
    pub(crate) lowered: ThinVec<Stmt>,
    pub(crate) ast_root: ASTRoot,
    stmts: ThinVec<ASTStmt>,

    pub(crate) errors: ThinVec<Report>,
}

fn expr_arena_with_missing() -> Arena<Expr> {
    let mut arena = Arena::new();
    _ = arena.alloc(Expr::Missing);
    arena
}

// Note: I may switch to LOUDS trie for some, measure the performance gain.
impl HIRBuilder {
    pub fn new(ast_root: ASTRoot) -> Self {
        let expr_arena = expr_arena_with_missing();

        let var_names = Arena::<SmolStr>::new();
        let var_defs = Arena::<VarDef>::new();
        let var_name_to_idx_trie = NameToIndexTrie::<VarDefIdx>::new();
        let vars_to_resolve = Arena::<UnresolvedVarRef>::new();

        let str_arena = Arena::<SmolStr>::new();
        let tensor_arena = Arena::<CanonicalTensor>::new();

        let fn_arena = Arena::<FnDef>::new();
        let fn_names = Arena::<SmolStr>::new();
        let fn_name_to_idx_trie = NameToIndexTrie::<FnDefIdx>::new();
        let fn_to_resolve = Arena::<UnresolvedFnCall>::new();

        let lowered = ThinVec::<Stmt>::new();

        let mut stmts = ast_root.statements().collect::<ThinVec<_>>();
        stmts.reverse();

        let errors = ThinVec::<Report>::new();

        Self {
            expr_arena,
            var_names,
            var_defs,
            var_name_to_idx_trie,
            vars_to_resolve,
            str_arena,
            tensor_arena,
            fn_arena,
            fn_names,
            fn_name_to_idx_trie,
            fn_to_resolve,
            lowered,
            ast_root,
            stmts,
            errors,
        }
    }
    pub fn allocate_var_def_with_name(&mut self, name: &SmolStr, var_def: VarDef) -> VarDefIdx {
        let idx = self.var_defs.alloc(var_def);
        self.var_name_to_idx_trie.insert(name, idx);
        idx
    }
    pub fn allocate_string(&mut self, string: SmolStr) -> StrIdx {
        self.str_arena.alloc(string)
    }
    pub fn allocate_tensor_literal(&mut self, tensor_literal: CanonicalTensor) -> TensorLiteralIdx {
        self.tensor_arena.alloc(tensor_literal)
    }
    pub fn insert_fn_def_in_trie(&mut self, key: &SmolStr, value: FnDefIdx) {
        self.fn_name_to_idx_trie.insert(key, value);
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
pub mod tests {
    use super::*;
    use la_arena::RawIdx;
    use parser::parser::Parser;

    use ast::ast::Root as ASTRoot;

    pub(crate) fn ast_root_from(program: &str) -> ASTRoot {
        let root = Parser::new(program).parse();
        println!("debug_tree: {:?}", root.debug_tree());
        ASTRoot::try_from(root).expect("should have been ok")
    }
    pub(crate) fn idx(from: &u32) -> ExprIdx {
        Idx::from_raw(RawIdx::from_u32(*from))
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
