use thin_vec::{ThinVec, thin_vec};

use std::fmt::Debug;

use crate::{
    HIRResult,
    builder::HIRBuilder,
    clone_with_err,
    delimited::{Block, Indexing},
    errors::HIRError,
    expect_non_baggage,
    expression::Expr,
    function::FnArg,
    literal::Value,
    operation::{BinaryOp, UnaryOp},
    resolution::{Baggage, Reference},
    self_ref::SelfRef,
};

use super::{
    inference::TypeKey,
    statement::HMStmt,
    types::{Maybe, Type},
};

#[derive(Debug, Clone)]
pub struct Conditional {
    pub(crate) condition: HMExpr,
    pub(crate) body: HMExpr,
}

#[derive(Debug, Clone)]
// TODO: literals?
pub enum HMExpr {
    Bool,
    Buffer {
        data_type: Maybe<Type>,
        shape: ThinVec<usize>,
    },
    // TODO: Not sure of the returns part, see block_with_conditional_returns, propagating the return
    // can be a pain in the ass, revisit this
    Block {
        returns: ThinVec<usize>,
        statements: ThinVec<HMStmt>,
    },
    BinaryOp {
        op: BinaryOp,
        lhs: Box<HMExpr>,
        rhs: Box<HMExpr>,
    },
    Char,
    ContainerRef {
        reference: TypeKey,
        indexing: ThinVec<HMExpr>,
    },
    FnCall {
        fn_to_call: Box<HMStmt>,
        args: ThinVec<HMStmt>,
    },
    F32,
    I32,
    // Indexing -> must return either a Range or usize
    // Literal
    Mut(Box<HMExpr>),
    // Param
    // Paren is just an expression
    // Ref
    SelfRef, // only corresponds to Self, self is resolved separately under Var("self")
    Str,
    StructAsType(TypeKey),
    StructInit {
        key: TypeKey,
        fields: ThinVec<(TypeKey, HMExpr)>,
    },
    Tensor {
        // TODO: revisit this, can put option logic in Maybe as Maybe::Unchecked(thin_vec![])
        data_type: Option<Maybe<Type>>,
        shape: ThinVec<Option<usize>>,
    },
    Tuple(ThinVec<HMExpr>),
    Unary(UnaryOp, Box<HMExpr>),
    Var(TypeKey),
}

fn get_resolved_materials<Any: Debug>(reference: &Reference<Any>) -> HIRResult<(TypeKey, Baggage)> {
    match reference {
        Reference::Unresolved(_) => {
            Err(HIRError::with_msg(format!("{:?} is not resolved", reference)).into())
        }
        Reference::Resolved {
            at: _,
            baggage,
            name_idx,
            obj_idx: _,
        } => Ok((*name_idx, baggage.clone())),
    }
}

impl HIRBuilder {
    pub fn try_into_hm_block(&self, block: &Block) -> HIRResult<HMExpr> {
        let mut statements = ThinVec::new();
        for statement in block.statements.iter() {
            statements.push(self.try_into_hm_stmt(statement)?);
        }
        Ok(HMExpr::Block {
            returns: block.returns.clone(),
            statements,
        })
    }

    pub fn try_into_hm_expr(&self, expr: &Expr) -> HIRResult<HMExpr> {
        match expr {
            Expr::Block(block) => self.try_into_hm_block(block),
            Expr::ContainerRef(reference) => {
                let (type_key, baggage) = get_resolved_materials(reference)?;
                let arr_indexing = match baggage {
                    Baggage::Index(thin_vec) => thin_vec.clone(),
                    _ => thin_vec![],
                };
                let to_hm = |ix: &Indexing, hir: &HIRBuilder| {
                    let expr = hir.get_expr(ix.0);
                    hir.try_into_hm_expr(expr)
                };
                let indexing = clone_with_err(arr_indexing.as_slice(), &self, to_hm)?;
                Ok(HMExpr::ContainerRef {
                    reference: type_key,
                    indexing,
                })
            }
            Expr::FnCall(reference) => {
                let (type_key, baggage) = get_resolved_materials(reference)?;
                let baggages = match baggage {
                    Baggage::Arg(thin_vec) => thin_vec.clone(),
                    _ => thin_vec![],
                };

                let to_hm = |fn_arg: &FnArg, hir: &HIRBuilder| {
                    let expr = hir.get_expr(fn_arg.0);
                    hir.try_into_hm_expr(expr)
                        .map(|hm_expr| HMStmt::Expr(hm_expr))
                };
                let args = clone_with_err(baggages.as_slice(), &self, to_hm)?;
                Ok(HMExpr::FnCall {
                    fn_to_call: Box::new(HMStmt::Expr(HMExpr::Var(type_key))),
                    args,
                })
            }
            Expr::Indexing(indexing) => {
                let expr = self.get_expr(indexing.0);
                self.try_into_hm_expr(expr)
            }
            Expr::Infix(binary_infix) => {
                let lhs_operand = binary_infix.lhs();
                let lhs = Box::new(self.try_into_hm_expr(self.get_expr(lhs_operand.0))?);

                let rhs_operand = binary_infix.rhs();
                let rhs = Box::new(self.try_into_hm_expr(self.get_expr(rhs_operand.0))?);

                let op = binary_infix.op().clone();
                Ok(HMExpr::BinaryOp { op, lhs, rhs })
            }
            Expr::Literal(literal) => HMExpr::try_from(&literal.0).map_err(|e| e.into()),
            Expr::Missing => Err(HIRError::with_msg(format!("Missing expression")).into()),
            Expr::Mut(inner) => {
                let inner_as_hm_expr = self.try_into_hm_expr(self.get_expr(inner.0))?;
                Ok(HMExpr::Mut(Box::new(inner_as_hm_expr)))
            }
            Expr::Paren(paren) => self.try_into_hm_expr(self.get_expr(paren.0)),
            Expr::SelfRef(self_ref) => match self_ref {
                SelfRef::Instance => todo!(), // TODO: Var("self"), we need self's idx
                SelfRef::Struct => Ok(HMExpr::SelfRef),
            },
            Expr::StructRef(reference) => {
                let (type_key, baggage) = get_resolved_materials(reference)?;
                _ = expect_non_baggage(&baggage, type_key)?;
                Ok(HMExpr::StructAsType(type_key))
            }
            Expr::StructInit(struct_init) => todo!(), // TODO: will be modified as a literal, delete this after its finished
            Expr::Unary(unary) => {
                let hm = Box::new(self.try_into_hm_expr(self.get_expr(unary.operand().0))?);
                let op = unary.op().clone();
                Ok(HMExpr::Unary(op, hm))
            }
            Expr::VarRef(reference) => {
                let (type_key, baggage) = get_resolved_materials(reference)?;
                _ = expect_non_baggage(&baggage, type_key)?;
                Ok(HMExpr::Var(type_key))
            }
        }
    }
}

impl TryFrom<&Value> for HMExpr {
    type Error = HIRError;
    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        match value {
            Value::Bool(_) => Ok(HMExpr::Bool),
            Value::Char(_) => Ok(HMExpr::Char),
            Value::Float(_) => Ok(HMExpr::F32),
            Value::Int(_) => Ok(HMExpr::I32),
            Value::Str(_) => Ok(HMExpr::Str),
            Value::Buffer {
                idx: _,
                shape,
                data_type,
            } => Ok(HMExpr::Buffer {
                data_type: data_type.clone(),
                shape: shape
                    .get()
                    .expect("a valid shape from Buffer value")
                    .clone(),
            }),
            Value::Tensor {
                idx: _,
                shape,
                data_type,
            } => Ok(HMExpr::Tensor {
                data_type: data_type.clone(),
                shape: shape.clone(),
            }),
        }
    }
}
