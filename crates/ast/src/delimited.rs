use syntax::{bitset::SyntaxKindBitSet, language::SyntaxNode, syntax_kind::SyntaxKind};
use thin_vec::ThinVec;

use crate::{
    ast::ASTResult,
    errors::ASTError,
    expression::Expr,
    lang_elems::{
        CAN_BE_EMPTY, ensure_node_kind_is, get_children_as, get_single_children_as_expr,
        vector_of_children_and_tokens_as,
    },
    statement::Stmt,
};

#[derive(Clone, Debug, PartialEq)]
pub struct Tuple(pub(crate) SyntaxNode);
impl Tuple {
    pub fn to_remove_from_tuples() -> SyntaxKindBitSet {
        [
            SyntaxKind::Comma,
            SyntaxKind::LParen,
            SyntaxKind::RParen,
            SyntaxKind::Whitespace,
        ]
        .as_ref()
        .into()
    }
    pub fn elements(&self) -> ASTResult<ThinVec<Expr>> {
        let elements =
            vector_of_children_and_tokens_as(&self.0, Self::to_remove_from_tuples(), |ch| {
                Expr::try_from(ch)
            })?;
        Ok(elements)
    }
}

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
        let stmts = get_children_as::<Stmt>(block_node, CAN_BE_EMPTY)?;
        if matches!(stmts.last(), Some(Stmt::Expr(_))) {
            Ok(Self::Returning(stmts))
        } else {
            Ok(Self::Semi(stmts))
        }
    }
}

impl Block {
    pub fn statements(&self) -> &[Stmt] {
        match self {
            Self::Returning(stmts) => stmts.as_slice(),
            Self::Semi(stmts) => stmts.as_slice(),
        }
    }
}

#[cfg(test)]
mod test {

    use super::*;
    use crate::{
        ast::Root,
        ast_root_from,
        literal::{Literal, Value},
        operation::{Binary, test::assert_infix_bin_op_with},
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
    fn happy_path_for_nested_tuples() {
        let program = "((3,14),5)";
        let ast_root = ast_root_from(program);
        let tuple_node = ast_root.get_root().first_child().unwrap();
        let tuple = Tuple(tuple_node);
        let elements = tuple.elements().expect("should have been ok");
        assert!(elements.len() == 2);
        let first_tuple = elements.get(0).unwrap();
        assert!(matches!(first_tuple, Expr::Tuple(_)));
        if let Expr::Tuple(tuple) = first_tuple {
            let elements = tuple.elements().expect("should have been ok");
            assert!(elements.len() == 2);
            assert!(matches!(
                elements.get(0).unwrap(),
                Expr::Literal(Literal(Value::Int(3)))
            ));
            assert!(matches!(
                elements.get(1).unwrap(),
                Expr::Literal(Literal(Value::Int(14)))
            ));
        }
        assert!(matches!(
            elements.get(1).unwrap(),
            Expr::Literal(Literal(Value::Int(5)))
        ));
    }

    #[test]
    fn happy_path_for_unit_tuple() {
        let program = "()";
        let ast_root = ast_root_from(program);
        let unit_node = ast_root.get_root().first_child().unwrap();
        let unit = Expr::try_from(unit_node).expect("should have been ok");
        assert_eq!(unit, Expr::Unit);
    }

    #[test]
    fn happy_path_for_returning_block() {
        let program = "{let a = 3; let b =14; a+b}";
        let ast_root = ast_root_from(program);
        let block = get_block_from(&ast_root);
        assert!(matches!(block, Block::Returning(_)));
        if let Block::Returning(stmts) = block {
            assert!(stmts.len() == 3);
            assert!(matches!(stmts.get(0).unwrap(), Stmt::LetBinding(_)));
            assert!(matches!(stmts.get(1).unwrap(), Stmt::LetBinding(_)));
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
}
