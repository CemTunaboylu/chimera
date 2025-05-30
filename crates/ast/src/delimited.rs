use syntax::{language::SyntaxNode, syntax_kind::SyntaxKind};
use thin_vec::ThinVec;

use crate::{
    ast::ASTResult,
    errors::ASTError,
    expression::Expr,
    lang_elems::{
        ensure_node_kind_is, error_for_node, get_children_as, get_children_in,
        get_single_children_as_expr,
    },
    statement::Stmt,
};

#[derive(Clone, Debug, PartialEq)]
pub struct Paren(pub(crate) SyntaxNode);

impl Paren {
    pub fn expr(&self) -> ASTResult<Expr> {
        get_single_children_as_expr(&self.0)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Block {
    Semi(ThinVec<Stmt>),
    Returning(ThinVec<Stmt>),
}

impl TryFrom<&SyntaxNode> for Block {
    type Error = ASTError;
    fn try_from(block_node: &SyntaxNode) -> Result<Self, Self::Error> {
        ensure_node_kind_is(block_node, SyntaxKind::Block)?;
        // TODO: for now, we ignore the failures
        let stmts = get_children_as::<Stmt>(block_node)?;
        if matches!(stmts.last(), Some(Stmt::Expr(_))) {
            Ok(Self::Returning(stmts))
        } else {
            Ok(Self::Semi(stmts))
        }
    }
}

impl Block {
    pub fn statements(&self) -> &ThinVec<Stmt> {
        match self {
            Self::Returning(stmts) => stmts,
            Self::Semi(stmts) => stmts,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Indexing(pub(crate) SyntaxNode);

impl Indexing {
    pub fn get_indexing_nodes_from(node: &SyntaxNode) -> ThinVec<SyntaxNode> {
        get_children_in(node, SyntaxKind::Indexing)
    }
    pub fn index(&self) -> ASTResult<Expr> {
        if let Some(node) = self.0.children().find(|node_or_token| {
            !matches!(
                node_or_token.kind(),
                SyntaxKind::LBrack | SyntaxKind::RBrack
            )
        }) {
            Expr::try_from(&node)
        } else {
            Err(error_for_node(&self.0, "non empty child"))
        }
    }
}

#[cfg(test)]
mod test {

    use super::*;
    use crate::{
        ast::Root,
        literal::Value,
        operation::{Binary, test::assert_infix_bin_op_with},
        {ast_root_from, cast_node_into_type},
    };

    fn get_paren_from(ast_root: &Root) -> Paren {
        Paren(ast_root.get_root().first_child().unwrap())
    }

    fn get_block_from(ast_root: &Root) -> Block {
        Block::try_from(ast_root.get_root().first_child().as_ref().unwrap()).unwrap()
    }

    #[test]
    fn happy_path_for_paren() {
        let program = "(3+14)";
        let ast_root = ast_root_from(program);
        let paren = get_paren_from(&ast_root);
        let infix_bin_op = paren.expr().expect("shoud have been ok ");
        assert!(matches!(infix_bin_op, Expr::Infix(Binary::Infix(_))));
        if let Expr::Infix(infix_bin_op) = infix_bin_op {
            assert_infix_bin_op_with(
                &infix_bin_op,
                &Value::Int(3),
                &Value::Int(14),
                SyntaxKind::Plus,
            );
        }
    }

    #[test]
    fn happy_path_for_nested_paren() {
        let program = "((3+14)-4)";
        let ast_root = ast_root_from(program);
        let paren = get_paren_from(&ast_root);
        let infix_bin_op = paren.expr().expect("shoud have been ok ");
        assert!(matches!(infix_bin_op, Expr::Infix(Binary::Infix(_))));
        if let Expr::Infix(infix_bin_op) = infix_bin_op {
            let lhs_paren = infix_bin_op.lhs().unwrap();
            assert!(matches!(lhs_paren, Expr::Paren(_)));
            if let Expr::Paren(inner_bin_op) = lhs_paren {
                if let Expr::Infix(inner_binary) = inner_bin_op.expr().unwrap() {
                    assert_infix_bin_op_with(
                        &inner_binary,
                        &Value::Int(3),
                        &Value::Int(14),
                        SyntaxKind::Plus,
                    );
                }
            }
            assert_eq!(SyntaxKind::Minus, infix_bin_op.op().unwrap());
        }
    }

    #[test]
    fn happy_path_for_returning_block() {
        let program = "{let a = 3; let b =14; a+b}";
        let ast_root = ast_root_from(program);
        let block = get_block_from(&ast_root);
        assert!(matches!(block, Block::Returning(_)));
        if let Block::Returning(stmts) = block {
            assert!(stmts.len() == 3);
            assert!(matches!(stmts.get(0).unwrap(), Stmt::VarDef(_)));
            assert!(matches!(stmts.get(1).unwrap(), Stmt::VarDef(_)));
            assert!(matches!(
                stmts.get(2).unwrap(),
                Stmt::Expr(Expr::Infix(Binary::Infix(_)))
            ));
        }
    }

    #[test]
    fn happy_path_for_semi_block() {
        let program = "{a += 3; b += 14; }";
        let ast_root = ast_root_from(program);
        let block = get_block_from(&ast_root);
        assert!(matches!(block, Block::Semi(_)));
        if let Block::Returning(stmts) = block {
            assert!(stmts.len() == 2);
            assert!(matches!(
                stmts.get(0).unwrap(),
                Stmt::Expr(Expr::Infix(Binary::Infix(_)))
            ));
            assert!(matches!(
                stmts.get(1).unwrap(),
                Stmt::Expr(Expr::Infix(Binary::Infix(_)))
            ));
        }
    }

    #[test]
    fn happy_path_for_nested_block() {
        let program = "{{3+14}-4}";
        let ast_root = ast_root_from(program);
        let block = get_block_from(&ast_root);

        assert!(matches!(block, Block::Returning(_)));
        if let Block::Returning(stmts) = block {
            assert!(stmts.len() == 1);
            assert!(matches!(
                stmts.get(0).unwrap(),
                Stmt::Expr(Expr::Infix(Binary::Infix(_)))
            ));
        }
    }

    #[test]
    fn happy_path_for_indexing() {
        let program = "arr[arr.len()-1]";
        let ast_root = ast_root_from(program);
        let container_ref = ast_root.get_root().first_child().unwrap();
        let indexing = cast_node_into_type::<Expr>(&container_ref.first_child().unwrap());

        assert!(matches!(indexing, Expr::Indexing(_)));
        if let Expr::Indexing(indexing) = indexing {
            assert!(matches!(
                indexing.index().expect("should have been ok"),
                Expr::Infix(Binary::Infix(_))
            ));
        }
    }
}
