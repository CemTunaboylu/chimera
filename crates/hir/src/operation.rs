use smol_str::SmolStr;

use crate::{
    HIRResult, err_if_none,
    hir::{ExprIdx, HIRBuilder},
};

use ast::operation::{Binary as ASTBinary, Unary as ASTUnary};

#[derive(Clone, Debug, PartialEq)]
pub struct Operand(ExprIdx);

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

impl From<&SmolStr> for BinaryOp {
    fn from(value: &SmolStr) -> Self {
        let op = match value.as_str() {
            "+" => Self::Add,
            "/" => Self::Div,
            "." => Self::Dot,
            "=" => Self::Eq,
            "*" => Self::Mul,
            "-" => Self::Sub,
            _ => Self::No,
        };
        op
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum UnaryOp {
    Neg,
    Not,
    CondUnwrap,
    No,
}

impl From<&SmolStr> for UnaryOp {
    fn from(value: &SmolStr) -> Self {
        let op = match value.as_str() {
            "-" => Self::Neg,
            "!" => Self::Not,
            "?" => Self::CondUnwrap,
            _ => Self::No,
        };
        op
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct BinaryInfix {
    op: BinaryOp,
    lhs: Operand,
    rhs: Operand,
}

impl BinaryInfix {
    pub fn lhs(&self) -> &Operand {
        &self.lhs
    }
    pub fn rhs(&self) -> &Operand {
        &self.rhs
    }
    pub fn op(&self) -> &BinaryOp {
        &self.op
    }
    fn validate_ast(bin: &ASTBinary) -> HIRResult<()> {
        _ = err_if_none(bin.lhs(), "a left operand")?;
        _ = err_if_none(bin.rhs(), "a right operand")?;
        _ = err_if_none(bin.op(), "an operation")?;
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct UnaryOperation {
    op: UnaryOp,
    operand: Operand,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Unary {
    Prefix(UnaryOperation),
    Postfix(UnaryOperation),
}

impl Unary {
    fn validate_ast(un: &ASTUnary) -> HIRResult<()> {
        _ = err_if_none(un.operand(), "an operand")?;
        _ = err_if_none(un.op(), "an operation")?;
        Ok(())
    }
    fn get_inner_unary_operation(&self) -> &UnaryOperation {
        match self {
            Unary::Prefix(unary_operation) => unary_operation,
            Unary::Postfix(unary_operation) => unary_operation,
        }
    }

    // TODO: addOp trait
    pub fn op(&self) -> &UnaryOp {
        let unary_op = self.get_inner_unary_operation();
        &unary_op.op
    }

    pub fn operand(&self) -> &Operand {
        let unary_op = self.get_inner_unary_operation();
        &unary_op.operand
    }
}

impl HIRBuilder {
    pub fn lower_binary_operation(&mut self, value: &ASTBinary) -> HIRResult<BinaryInfix> {
        _ = BinaryInfix::validate_ast(&value)?;
        let lhs = Operand(self.lower_expr_as_idx(value.lhs().unwrap())?);
        let rhs = Operand(self.lower_expr_as_idx(value.rhs().unwrap())?);
        let op = BinaryOp::from(value.op().unwrap());
        Ok(BinaryInfix { op, lhs, rhs })
    }
    pub fn lower_unary_operation(&mut self, value: &ASTUnary) -> HIRResult<Unary> {
        _ = Unary::validate_ast(&value)?;
        let operand = Operand(self.lower_expr_as_idx(value.operand().unwrap())?);
        let op = UnaryOp::from(value.op().unwrap());
        let unary_op = UnaryOperation { op, operand };
        let u = match value {
            ASTUnary::Prefix(_) => Unary::Prefix(unary_op),
            ASTUnary::Postfix(_) => Unary::Postfix(unary_op),
        };
        Ok(u)
    }
}

#[cfg(test)]
pub(crate) mod test {

    use crate::hir::tests::ast_root_from;

    use parameterized_test::create;

    create! {
        binary_operation_test,
        (program, exp_ids), {
        let ast_root = ast_root_from(program);
        let mut stmts = ast_root.statements();
        let ast_op = stmts.next().unwrap();
        }
    }
    binary_operation_test! {
        simple_add: ("3+1",  [1u32,2u32].as_ref()),
    }
}
