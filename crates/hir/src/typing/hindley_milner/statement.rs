use smol_str::SmolStr;
use thin_vec::ThinVec;

use crate::{
    HIRResult, builder::HIRBuilder, clone_with_err, control_flow::Conditional as HIRConditional,
    jump::Jump, loops::Loop as HIRLoop, statement::Stmt,
};

use super::{
    expression::{Conditional, HMExpr},
    inference::TypeKey,
    types::Type,
};

#[derive(Debug, Clone)]
pub enum Loop {
    For {
        identifiers: ThinVec<HMExpr>,
        looping_on: HMExpr,
        block: HMExpr,
    },
    While {
        condition: HMExpr,
        block: HMExpr,
    },
}

#[derive(Debug, Clone)]
pub enum HMStmt {
    Expr(HMExpr),
    /*
        if cond1 { body1 }
        else if cond2 { body2 }
        else { body3 }

        becomes

        ControlFlow {
            branches_with_condition_and_blocks: thin_vec![
                (cond1, body1),
                (cond2, body2),
            ],
            else_block: Box::new(body3),
        }
    */
    ControlFlow {
        branches: ThinVec<Conditional>, // ThinVec<(condition expression, body of the conditional )>
        else_block: Option<Box<HMExpr>>,
    },
    FnDef {
        key: TypeKey,
        parameters: ThinVec<(TypeKey, Type)>,
        return_type: Option<Type>,
        body: Box<HMStmt>,
    },
    // impl
    Jump(Option<Box<HMExpr>>),

    // loop
    Loop(Loop),
    Lambda {
        params: ThinVec<TypeKey>,
        body: Box<HMStmt>,
    },
    Let {
        key: TypeKey,
        val: Box<HMStmt>,
        body: Box<HMStmt>,
    },
    StructDef {
        key: TypeKey,
        fields: ThinVec<(TypeKey, Type)>,
    },
}

impl HIRBuilder {
    pub fn try_into_hm_stmt(&self, stmt: &Stmt) -> HIRResult<HMStmt> {
        match stmt {
            Stmt::ControlFlow(control_flow) => {
                // note: conditionals are guaranteed to start with an if statement, AST ensures that
                let mut split = control_flow
                    .0
                    .split(|c| matches!(c, HIRConditional::Else(_)));
                let to_hm = |hc: &HIRConditional, hir: &HIRBuilder| {
                    let (condition, block) = match hc {
                        HIRConditional::If(condition, block) => (condition, block),
                        HIRConditional::Elif(condition, block) => (condition, block),
                        HIRConditional::Else(_block) => unreachable!(), // sliced it
                    };
                    let expr = hir.get_expr(condition.0);
                    let hm_condition = hir.try_into_hm_expr(expr)?;
                    let hm_block = hir.try_into_hm_block(block)?;
                    Ok(Conditional {
                        condition: hm_condition,
                        body: hm_block,
                    })
                };
                let branches = clone_with_err(split.next().unwrap(), self, to_hm)?;
                let else_block = if let Some(&[HIRConditional::Else(block)]) = split.next().as_ref()
                {
                    let hm_block = self.try_into_hm_block(block)?;
                    Some(Box::new(hm_block))
                } else {
                    None
                };
                Ok(HMStmt::ControlFlow {
                    branches,
                    else_block,
                })
            }
            Stmt::Expr(idx) => {
                let expr = self.get_expr(*idx);
                self.try_into_hm_expr(expr).map(HMStmt::Expr)
            }
            Stmt::FnDef(idx) => todo!(), // TODO: does not have getter yet
            Stmt::Impl(_) => todo!(),
            Stmt::Jump(jump) => match jump {
                Jump::Continue | Jump::Break(None) => Ok(HMStmt::Jump(None)),
                Jump::Break(Some(idx)) => {
                    let hm_expr = self.try_into_hm_expr(self.get_expr(*idx))?;
                    Ok(HMStmt::Jump(Some(Box::new(hm_expr))))
                }
            },
            Stmt::Loop(lp) => match lp {
                HIRLoop::While(condition, block) => {
                    let c = self.try_into_hm_expr(self.get_expr(condition.0))?;
                    let b = self.try_into_hm_block(block)?;
                    Ok(HMStmt::Loop(Loop::While {
                        condition: c,
                        block: b,
                    }))
                }
                HIRLoop::For(identifiers, _in, block) => {
                    let into_hm = |identifier: &SmolStr, hir: &HIRBuilder| {
                        // TODO: identifiers are actually a variable definition
                        //  thus I may need to convert it into a let
                        todo!()
                    };
                    // let hm_idents = clone_with_err(identifiers.0.as_slice(), &self, into_hm)?;
                    todo!()
                }
            },
            Stmt::Return(_) => todo!(),
            Stmt::Semi(semi) => todo!(),
            Stmt::StructDef(idx) => todo!(),
            Stmt::VarDef(idx) => todo!(),
        }
    }
}
