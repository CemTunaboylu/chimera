use hir_macro::with_context;
use miette::Report;
use syntax::syntax_kind::SyntaxKind;

use crate::{
    HIRResult, builder::HIRBuilder, context::UsageContext, err_if_none, errors::HIRError,
    scope::ExprIdx,
};

use ast::{
    expression::Expr as ASTExpr,
    operation::{Binary as ASTBinary, Unary as ASTUnary},
};

#[derive(Clone, Debug, PartialEq)]
pub struct Operand(pub ExprIdx);

#[derive(Clone, Debug, PartialEq)]
pub enum BinaryOp {
    Add,
    Assign,
    AssgmtWith(Box<BinaryOp>),
    BitAnd,
    BitOr,
    BoolAnd,
    BoolOr,
    Div,
    Dot,
    EqEq,
    Ge,
    Gt,
    LShift,
    Le,
    Lt,
    Mod,
    Mul,
    Namespaced,
    NotEq,
    RShift,
    Range,
    Sub,
    TypeHint, // <identifier> : <type keyword>
    Xor,
}

fn not_an_op(kind: SyntaxKind) -> Report {
    HIRError::with_msg(format!("Expected a binary operator, '{:?}' is not", kind)).into()
}

impl BinaryOp {
    pub fn from_syntax_kind(kind: SyntaxKind) -> HIRResult<Self> {
        use SyntaxKind::*;
        let op = match kind {
            And => Self::BitAnd,
            AndAnd => Self::BoolAnd,
            AndEq => Self::AssgmtWith(Box::new(Self::BitAnd)),
            Colon => Self::TypeHint,
            ColonColon => Self::Namespaced,
            Dot => Self::Dot,
            Eq => Self::Assign,
            EqEq => Self::EqEq,
            Gt => Self::Gt,
            Ge => Self::Ge,
            LShift => Self::LShift,
            LShiftEq => Self::AssgmtWith(Box::new(Self::LShift)),
            Lt => Self::Lt,
            Le => Self::Le,
            Minus => Self::Sub,
            MinusEq => Self::AssgmtWith(Box::new(Self::Sub)),
            NotEq => Self::NotEq,
            OrEq => Self::AssgmtWith(Box::new(Self::BitOr)),
            Or => Self::BitOr,
            OrOr => Self::BoolOr,
            Percent => Self::Mod,
            PercentEq => Self::AssgmtWith(Box::new(Self::Mod)),
            Plus => Self::Add,
            PlusEq => Self::AssgmtWith(Box::new(Self::Add)),
            RShift => Self::RShift,
            RShiftEq => Self::AssgmtWith(Box::new(Self::RShift)),
            Slash => Self::Div,
            SlashEq => Self::AssgmtWith(Box::new(Self::Div)),
            Star => Self::Mul,
            StarEq => Self::AssgmtWith(Box::new(Self::Mul)),
            Under => Self::Range,
            Xor => Self::Xor,
            no => {
                return Err(not_an_op(no));
            }
        };
        Ok(op)
    }

    pub fn is_bit_op(&self) -> bool {
        use BinaryOp::*;
        matches!(self, BitAnd | BitOr | LShift | RShift | Xor)
    }
    pub fn is_bool_op(&self) -> bool {
        use BinaryOp::*;
        matches!(self, BoolAnd | BoolOr)
    }
    pub fn is_comparison(&self) -> bool {
        use BinaryOp::*;
        matches!(self, EqEq | Ge | Gt | Le | Lt | NotEq)
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum UnaryOp {
    /// star
    Deref,
    /// and
    Ref,
    /// minus
    Neg,
    /// excl
    Not,
    CondUnwrap,
}

impl UnaryOp {
    fn from_syntax_kind(kind: SyntaxKind) -> HIRResult<UnaryOp> {
        use SyntaxKind::*;
        let op = match kind {
            And => Self::Ref,
            Excl => Self::Not,
            Minus => Self::Neg,
            QMark => Self::CondUnwrap,
            Star => Self::Deref,
            no => {
                return Err(not_an_op(no));
            }
        };
        Ok(op)
    }
}

impl Into<UsageContext> for &UnaryOp {
    fn into(self) -> UsageContext {
        match self {
            UnaryOp::Deref => UsageContext::Deref,
            UnaryOp::Ref => UsageContext::Ref,
            UnaryOp::Neg | UnaryOp::Not => UsageContext::Read,
            // an unwrap moves the value
            // in case of &opt? -> opt.as_ref().unwrap(), thus a move only takes the pointer
            UnaryOp::CondUnwrap => UsageContext::Moved,
        }
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
        err_if_none(bin.lhs(), "a left operand")?;
        err_if_none(bin.rhs(), "a right operand")?;
        err_if_none(bin.op().as_ref(), "an operation")?;
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
        err_if_none(un.operand(), "an operand")?;
        err_if_none(un.op().as_ref(), "an operation")?;
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
    #[inline]
    pub fn lower_operand_with_context(
        &mut self,
        o: &ASTExpr,
        ctx: UsageContext,
    ) -> HIRResult<Operand> {
        self.push_usage_context(ctx);
        let l_o = Operand(self.lower_expr_as_idx(o)?);
        self.pop_usage_context();
        Ok(l_o)
    }
    pub fn lower_binary_operation(&mut self, value: &ASTBinary) -> HIRResult<BinaryInfix> {
        BinaryInfix::validate_ast(value)?;
        let op = BinaryOp::from_syntax_kind(value.op().expect("binary operation"))?;
        let ctx = self.get_context();
        let ast_lhs = value.lhs().expect("left-hand-side operand");
        let ctx = if op == BinaryOp::Assign {
            if ctx.is_none_or(|l_ctx| !l_ctx.is_of_usage(UsageContext::Init)) {
                UsageContext::Read
            } else {
                UsageContext::Init
            }
        } else {
            UsageContext::Read
        };
        let lhs = self.lower_operand_with_context(ast_lhs, ctx)?;
        let rhs = self.lower_operand_with_context(value.rhs().unwrap(), UsageContext::Read)?;
        Ok(BinaryInfix { op, lhs, rhs })
    }

    #[with_context(UsageContext::Read)]
    pub fn lower_unary_operation(&mut self, value: &ASTUnary) -> HIRResult<Unary> {
        Unary::validate_ast(value)?;
        let op = UnaryOp::from_syntax_kind(value.op().expect("unary operation"))?;
        let ast_value = value.operand().expect("unary operand");
        let ctx: UsageContext = (&op).into();
        let operand = self.lower_operand_with_context(ast_value, ctx)?;
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

    use crate::builder::tests::ast_root_from;

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
