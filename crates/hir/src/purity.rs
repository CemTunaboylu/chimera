use la_arena::Idx;

use crate::{
    builder::HIRBuilder,
    control_flow::{Conditional, ControlFlow},
    delimited::Block,
    function::{Call, FnDef, On},
    impl_block::Impl,
    indexing::Indexing,
    jump::Jump,
    let_binding::LetBinding,
    literal::Value,
    loops::Loop,
    operation::{BinaryInfix, BinaryOp, Unary, UnaryOp},
    parameter::Param,
    scope::{ExprIdx, FnSelector, ScopeIdx, Selector, VarSelector},
    statement::Stmt,
    typing::hindley_milner::types::{Maybe, Status, Type},
};

#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq, PartialOrd)]
pub enum Purity {
    #[default]
    Impure,
    Pure,
    Unknown,
}
impl Purity {
    pub fn is_pure(&self) -> bool {
        matches!(self, Purity::Pure)
    }
}

impl HIRBuilder {
    pub fn is_binary_infix_pure(&self, infix: &BinaryInfix) -> bool {
        use BinaryOp::*;
        match infix.op() {
            Assign | AssgmtWith(_) => false,
            Dot | Namespaced => {
                let lhs = infix.lhs();
                let rhs = infix.rhs();
                self.is_pure(lhs.0) && self.is_pure(rhs.0)
            }
            _ => true,
        }
    }
    pub fn is_block_pure(&self, block: &Block) -> bool {
        block.is_pure
    }
    pub fn is_call_pure(&self, call: &Call) -> bool {
        match &call.on {
            On::Binding(idx) => {
                let scope = self.get_current_scope();
                let metadata = &scope.metadata.fns;
                metadata
                    .get(&idx)
                    .is_some_and(|m| m.common.purity.is_pure())
            }
            On::Literal(literal) => self.is_value_pure(&literal.0),
        }
    }
    pub fn is_controlflow_pure(&self, control_flow: &ControlFlow) -> bool {
        control_flow.0.iter().all(|cond| match cond {
            Conditional::If(condition, block) => block.is_pure && self.is_pure(condition.0),
            Conditional::Elif(condition, block) => block.is_pure && self.is_pure(condition.0),
            Conditional::Else(block) => block.is_pure,
        })
    }
    pub fn is_expr_slice_pure(&self, exprs: &[ExprIdx]) -> bool {
        exprs.iter().all(|expr_idx| self.is_pure(*expr_idx))
    }
    pub fn is_fn_def_pure(&self, fn_def_idx: &Idx<FnDef>) -> bool {
        let scope = self.get_current_scope();
        let metadata = &scope.metadata.fns;
        metadata
            .get(fn_def_idx)
            .is_some_and(|m| m.common.purity.is_pure())
    }
    pub fn is_impl_pure(&self, imp: &Impl) -> bool {
        let scope_idx = imp.scope_idx;
        let scope = &self.scopes[scope_idx];
        imp.methods.iter().all(|fn_def_idx| {
            let metadata = &scope.metadata.fns;
            metadata
                .get(fn_def_idx)
                .is_some_and(|m| m.common.purity.is_pure())
        })
    }
    pub fn is_indexing_pure(&self, indexing: &Indexing) -> bool {
        indexing.indices.iter().all(|s| self.is_pure(s.index))
    }
    pub fn is_let_binding_pure(&self, indices: &[Idx<LetBinding>]) -> bool {
        let scope = self.get_current_scope();
        let allocator = VarSelector::select_alloc(scope);
        indices.iter().all(|lbi| {
            let binding = &allocator.definitions[*lbi];
            self.is_pure(binding.spanned_expr_idx.index)
        })
    }
    pub fn is_maybe_pure(&self, maybe: &Maybe<Type>) -> bool {
        match maybe {
            Maybe::Checked(t) => self.is_type_pure(t),
            Maybe::None => false,
            Maybe::Unchecked(_types) => false,
        }
    }
    pub fn is_types_pure(&self, ty: &[Type]) -> bool {
        ty.iter().all(|t| self.is_type_pure(t))
    }
    pub fn is_type_pure(&self, ty: &Type) -> bool {
        use Type::*;
        match ty {
            Buffer { data_type, .. } => self.is_maybe_pure(data_type),
            FnSig {
                param_types,
                return_type,
            } => self.is_type_pure(return_type) && self.is_types_pure(param_types),
            Ptr { of, is_mut } => !*is_mut && self.is_type_pure(of),
            // ! FIXME: PurityEnv should have a map to identify impure types
            StructAsType(status) => match status {
                Status::Pending(idx) => todo!(),
                Status::Resolved(idx) => todo!(),
            },
            Struct { fields, .. } => self.is_types_pure(fields),
            Tensor { data_type, .. } => self.is_maybe_pure(data_type),
            Tuple(types) => self.is_types_pure(types),
            _ => true,
        }
    }
    pub fn is_value_pure(&self, value: &Value) -> bool {
        match value {
            Value::Buffer { idx, data_type, .. } => {
                let scope = self.get_current_scope();
                let metadata = &scope.metadata.buffers;
                let is_buffer_itself_pure = metadata
                    .get(&idx)
                    .is_some_and(|m| m.common.purity.is_pure());
                is_buffer_itself_pure && self.is_maybe_pure(data_type)
            }
            Value::Lambda(callable) => {
                callable.body.is_pure
                    && callable.parameters.iter().all(|p| match p {
                        Param::Generic { is_mut, .. } => !*is_mut,
                        Param::Named {
                            is_mut, param_type, ..
                        } => !*is_mut && self.is_type_pure(param_type),
                        Param::SelfRef { is_mut, .. } => !*is_mut,
                    })
            }
            Value::Struct(struct_literal) => struct_literal
                .internal_with_field_values
                .data
                .iter()
                .all(|(_, expr_idx)| self.is_pure(*expr_idx)),
            Value::Tensor { idx, data_type, .. } => {
                let scope = self.get_current_scope();
                let metadata = &scope.metadata.buffers;
                let is_tensor_itself_pure = metadata
                    .get(&idx)
                    .is_some_and(|m| m.common.purity.is_pure());
                is_tensor_itself_pure && self.is_maybe_pure(data_type)
            }
            _ => true,
        }
    }
    pub fn is_unary_op_pure(&self, unary: &Unary) -> bool {
        use UnaryOp::*;
        match unary.op() {
            Ref => self.is_pure(unary.operand().0),
            _ => true,
        }
    }
    pub fn is_pure_with_scope(&self, scope_idx: ScopeIdx, expr_idx: ExprIdx) -> bool {
        self.expr_purity.contains(&(scope_idx, expr_idx))
    }
    pub fn is_pure(&self, expr_idx: ExprIdx) -> bool {
        let scope_idx = self.current_scope_cursor;
        self.expr_purity.contains(&(scope_idx, expr_idx))
    }
    pub fn set_as_pure(&mut self, expr_idx: ExprIdx) {
        let scope_idx = self.current_scope_cursor;
        self.expr_purity.insert((scope_idx, expr_idx));
    }
    pub fn is_stmt_pure(&self, stmt: &Stmt) -> bool {
        match stmt {
            Stmt::ControlFlow(control_flow) => self.is_controlflow_pure(control_flow),
            Stmt::Expr(idx) => self.is_pure(*idx),
            Stmt::FnDef(idx) => self.is_fn_def_pure(idx),
            Stmt::Impl(imp) => self.is_impl_pure(imp),
            Stmt::Jump(jump) => match jump {
                Jump::Continue => true,
                Jump::Break(idx) => idx.is_some_and(|expr_idx| self.is_pure(expr_idx)),
            },
            Stmt::Loop(l) => match l {
                Loop::While(condition, block) => block.is_pure && self.is_pure(condition.0),
                Loop::For(_identifiers, in_expr, block) => block.is_pure && self.is_pure(in_expr.0),
            },
            Stmt::Return(ret) => self.is_pure(ret.0),
            Stmt::Semi(semi) => self.is_pure(semi.0),
            Stmt::StructDef(_idx) => true,
            Stmt::LetBinding(thin_vec) => self.is_let_binding_pure(thin_vec.as_slice()),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use ast::{ast_root_from, cast_node_into_type, expression::Expr as ASTExpr};
    use parameterized_test::create;

    create! {
        expression_purity_test,
        (program, expr_purity), {
            let ast_root = ast_root_from(program);
            let expr_node = ast_root.get_root().first_child().unwrap();
            let ast_expr= cast_node_into_type::<ASTExpr>(&expr_node);
            let mut hir = HIRBuilder::new(ast_root);

            let expr_idx = hir.lower_expr_as_idx(&ast_expr).expect("should have been lowered");
            assert_eq!(expr_purity, hir.is_pure(expr_idx));
        }
    }

    const PURE: bool = true;
    const IMPURE: bool = false;

    expression_purity_test! {
        impure_block: ("{s=e.x;}", IMPURE),
        pure_block: ("{a+i;}", PURE),
        impure_lit_call: ("|&mut character|{character.corrupt_with(money);}", IMPURE),
        impure_infix: ("s=e.x", IMPURE),
        pure_infix: ("se.x", PURE),
        impure_mut: ("mut sex", IMPURE),
        pure_self: ("self", PURE),
        pure_tuple: ("(0,0,0)", PURE),
        pure_unary: ("&self", PURE),
        impure_unary: ("&mut self", IMPURE),
    }
}
