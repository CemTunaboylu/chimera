use syntax::{
    is_a_binary_operator,
    language::{SyntaxNode, SyntaxToken},
};
use thin_vec::ThinVec;

use crate::{
    ast::ASTResult,
    errors::ASTError,
    expression::Expr,
    lang_elems::{get_children_as, get_token_with},
};

#[derive(Debug, PartialEq)]
pub struct PreComputed {
    exprs: ThinVec<Expr>,
    node: SyntaxNode,
    op: Option<SyntaxToken>,
}

impl Clone for PreComputed {
    fn clone(&self) -> Self {
        Self {
            exprs: self.exprs.iter().map(|e| e.clone()).collect::<ThinVec<_>>(),
            node: self.node.clone(),
            op: self.op.clone(),
        }
    }
}

impl PreComputed {
    fn get_op(&self) -> Option<&SyntaxToken> {
        self.op.as_ref()
    }
    fn get_nth_expr(&self, n: usize) -> Option<&Expr> {
        self.exprs.get(n)
    }
    pub(crate) fn get_node(&self) -> &SyntaxNode {
        &self.node
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Binary {
    Infix(PreComputed),
}

impl Binary {
    pub fn new(infix_bin_op_node: &SyntaxNode) -> ASTResult<Self> {
        let exprs = get_children_as::<Expr>(&infix_bin_op_node)?;
        if exprs.len() != 2 {
            return Err(ASTError::new(
                infix_bin_op_node.text_range().into(),
                "expected 2 expressions for lhs and rhs",
                infix_bin_op_node,
            ));
        }
        let op = get_token_with(&infix_bin_op_node, |token: &SyntaxToken| {
            is_a_binary_operator(&token.kind())
        });
        Ok(Self::Infix(PreComputed {
            exprs,
            node: infix_bin_op_node.clone(),
            op,
        }))
    }
    pub(crate) fn get_pre_computed(&self) -> &PreComputed {
        match self {
            Self::Infix(pre_computed) => pre_computed,
        }
    }
    pub fn lhs(&self) -> Option<&Expr> {
        let pre_computed = self.get_pre_computed();
        pre_computed.get_nth_expr(0)
    }
    pub fn rhs(&self) -> Option<&Expr> {
        let pre_computed = self.get_pre_computed();
        pre_computed.get_nth_expr(1)
    }
    pub fn op(&self) -> Option<&SyntaxToken> {
        let pre_computed = self.get_pre_computed();
        pre_computed.get_op()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Unary {
    Prefix(PreComputed),
    Postfix(PreComputed),
}

fn prepare_pre_computed(node: &SyntaxNode, variant: fn(PreComputed) -> Unary) -> ASTResult<Unary> {
    let exprs = get_children_as::<Expr>(&node)?;
    let op = get_token_with(node, |token: &SyntaxToken| token.kind().is_unary_operator());
    let p = PreComputed {
        exprs,
        node: node.clone(),
        op,
    };
    Ok(variant(p))
}

impl Unary {
    pub fn prefix(node: &SyntaxNode) -> ASTResult<Self> {
        prepare_pre_computed(node, Self::Prefix)
    }
    pub fn postfix(node: &SyntaxNode) -> ASTResult<Self> {
        prepare_pre_computed(node, Self::Postfix)
    }
    fn get_pre_computed(&self) -> &PreComputed {
        match self {
            Self::Prefix(pre_computed) => pre_computed,
            Self::Postfix(pre_computed) => pre_computed,
        }
    }

    pub fn op(&self) -> Option<SyntaxToken> {
        let pre = self.get_pre_computed();
        pre.op.clone()
    }

    pub fn operand(&self) -> Option<&Expr> {
        let pre = self.get_pre_computed();
        pre.exprs.first()
    }
}

#[cfg(test)]
pub(crate) mod test {

    use std::ops::Range;

    use super::*;
    use crate::{
        ast::{Root, tests::ast_root_from},
        function::FnCall,
        literal::{Literal, Value},
        variable::VarRef,
    };

    fn assert_expr_literal_value_eq(expr: &Expr, exp: &Value) {
        match expr {
            Expr::Literal(literal) => {
                assert_eq!(exp, &literal.value())
            }
            _ => unreachable!(),
        }
    }

    pub(crate) fn assert_infix_bin_op_with(
        infix_bin_op: &Binary,
        lhs_val: &Value,
        rhs_val: &Value,
        op: &str,
    ) {
        let lhs = infix_bin_op.lhs().unwrap();
        assert_expr_literal_value_eq(lhs, lhs_val);
        let rhs = infix_bin_op.rhs().unwrap();
        assert_expr_literal_value_eq(rhs, rhs_val);
        let token = infix_bin_op.op().unwrap();
        assert_eq!(op, token.text());
    }

    fn new_bin_from(ast_root: &Root) -> Binary {
        Binary::new(ast_root.get_root().first_child().as_ref().unwrap())
            .expect("should have been ok")
    }

    fn new_unary_from(ast_root: &Root, f: fn(&SyntaxNode) -> ASTResult<Unary>) -> Unary {
        f(ast_root.get_root().first_child().as_ref().unwrap()).expect("should have been ok")
    }

    #[test]
    fn happy_path_for_infix() {
        let program = "3+14";
        let ast_root = ast_root_from(program);
        let infix_bin_op = new_bin_from(&ast_root);
        assert_infix_bin_op_with(&infix_bin_op, &Value::Int(0..1), &Value::Int(2..4), "+");
    }

    #[test]
    fn happy_path_for_nested_infix_() {
        let program = "(3+14)*(4-25)";
        let ast_root = ast_root_from(program);
        let infix_bin_op = new_bin_from(&ast_root);
        let lhs = infix_bin_op.lhs().unwrap();
        let rhs = infix_bin_op.rhs().unwrap();

        let to_assert = [lhs, rhs];
        let expected_value_tuples = [
            (Value::Int(1..2), Value::Int(3..5), "+"),
            (Value::Int(8..9), Value::Int(10..12), "-"),
        ];

        for (case, val_tup) in to_assert.iter().zip(expected_value_tuples.iter()) {
            assert!(matches!(case, Expr::Paren(_)));
            if let Expr::Paren(paren) = case {
                let expr = paren
                    .expr()
                    .expect(format!("{:?} should have been ok", case).as_str());
                assert!(matches!(expr, Expr::Infix(_)));
                if let Expr::Infix(bin) = expr {
                    assert_infix_bin_op_with(&bin, &val_tup.0, &val_tup.1, &val_tup.2);
                }
            }
        }
        let op = infix_bin_op.op().unwrap();
        assert_eq!("*", op.text());
    }

    #[test]
    fn happy_path_for_nested_unary_prefix() {
        let program = "--3";
        let ast_root = ast_root_from(program);
        let unary_prefix_op = new_unary_from(&ast_root, Unary::prefix);
        let operand = unary_prefix_op.operand().unwrap();
        let op = unary_prefix_op.op().unwrap();
        assert_eq!("-", op.text());
        assert!(matches!(operand, Expr::Unary(Unary::Prefix(_))));
        if let Expr::Unary(prefix) = operand {
            assert_eq!("-", prefix.op().unwrap().text());
            let operand = prefix.operand().unwrap();
            assert!(matches!(operand, Expr::Literal(Literal(Value::Int(_)))));
            if let Expr::Literal(Literal(Value::Int(range))) = operand {
                let exp_range: Range<usize> = 2..3;
                assert_eq!(&exp_range, range);
            }
        }
    }
    #[test]
    fn happy_path_for_nested_unary_postfix() {
        let program = "opt_opt??";
        let ast_root = ast_root_from(program);
        let unary_postfix_op = new_unary_from(&ast_root, Unary::postfix);
        let operand = unary_postfix_op.operand().unwrap();
        let op = unary_postfix_op.op().unwrap();
        assert_eq!("?", op.text());
        assert!(matches!(operand, Expr::Unary(Unary::Postfix(_))));
        if let Expr::Unary(postfix) = operand {
            assert_eq!("?", postfix.op().unwrap().text());
            let operand = postfix.operand().unwrap();
            assert!(matches!(operand, Expr::VarRef(VarRef(_))));
            if let Expr::VarRef(VarRef(smol_str)) = operand {
                assert_eq!("opt_opt", smol_str.as_str());
            }
        }
    }

    #[test]
    fn all_ops_nested_acc_to_precedence() {
        // will be equal to (-1)*(9?)
        let program = "-1*9?";
        let ast_root = ast_root_from(program);
        let infix_bin_op = new_bin_from(&ast_root);

        let op = infix_bin_op.op().unwrap();
        assert_eq!("*", op.text());

        let prefix = infix_bin_op.lhs().unwrap();
        assert!(matches!(prefix, Expr::Unary(Unary::Prefix(_))));

        if let Expr::Unary(prefix) = prefix {
            let operand = prefix.operand().unwrap();
            let op = prefix.op().unwrap();
            assert_eq!("-", op.text());
            assert!(matches!(operand, Expr::Literal(Literal(Value::Int(_)))));
            if let Expr::Literal(Literal(value)) = operand {
                assert_eq!(Value::Int(1..2), *value);
            }
        }

        let postfix = infix_bin_op.rhs().unwrap();
        assert!(matches!(postfix, Expr::Unary(Unary::Postfix(_))));

        if let Expr::Unary(postfix) = postfix {
            let operand = postfix.operand().unwrap();
            let op = postfix.op().unwrap();
            assert_eq!("?", op.text());
            assert!(matches!(operand, Expr::Literal(Literal(Value::Int(_)))));
            if let Expr::Literal(Literal(value)) = operand {
                assert_eq!(Value::Int(3..4), *value);
            }
        }
    }

    #[test]
    fn tensor_struct_construction() {
        let program = "Tensor<3,3,3><f32>::new()";
        let ast_root = ast_root_from(program);
        let infix_bin_op = new_bin_from(&ast_root);

        let op = infix_bin_op.op().unwrap();
        assert_eq!("::", op.text());

        let tensor = infix_bin_op.lhs().unwrap();
        assert!(matches!(tensor, Expr::TensorStruct(_)));

        if let Expr::Literal(Literal(Value::Tensor(t))) = tensor {
            assert_eq!(1..20, *t);
        }

        let method_call = infix_bin_op.rhs().unwrap();
        assert!(matches!(method_call, Expr::FnCall(_)));

        if let Expr::FnCall(call) = method_call {
            assert_eq!("new", call.name());
            assert!(call.arguments().is_empty());
        }
    }
}
