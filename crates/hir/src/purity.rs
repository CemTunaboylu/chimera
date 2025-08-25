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
    scope::{
        ExprIdx, FnSelector, ScopeIdx, Scoped, ScopedExprIdx, ScopedStmtIdx, Selector, StmtIdx,
        VarSelector,
    },
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
    pub fn fn_def_purity(&self, fn_def: &FnDef) -> Purity {
        if !self.is_block_pure(&fn_def.callable.body) {
            Purity::Impure
        } else if !self.are_parameters_pure(fn_def.callable.parameters.as_slice()) {
            Purity::Impure
        } else {
            Purity::Pure
        }
    }
    pub fn is_binary_infix_pure(&self, infix: &BinaryInfix) -> bool {
        use BinaryOp::*;
        match infix.op() {
            Assign | AssgmtWith(_) => false,
            Dot | Namespaced => {
                let lhs = infix.lhs();
                let rhs = infix.rhs();
                self.is_pure(&lhs.0) && self.is_pure(&rhs.0)
            }
            _ => true,
        }
    }
    pub fn is_block_pure(&self, block: &Block) -> bool {
        block.is_pure
    }
    pub fn is_call_pure(&self, call: &Call) -> bool {
        // * Note: if any of the arguments are impure, the call becomes impure.
        if !call.arguments.iter().all(|arg| self.is_pure(&arg.0)) {
            return false;
        }
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
            Conditional::If(condition, block) => block.is_pure && self.is_pure(&condition.0),
            Conditional::Elif(condition, block) => block.is_pure && self.is_pure(&condition.0),
            Conditional::Else(block) => block.is_pure,
        })
    }
    pub fn is_expr_slice_pure(&self, scope_idx: ScopeIdx, exprs: &[ExprIdx]) -> bool {
        exprs
            .iter()
            .all(|expr_idx| self.is_pure(&Scoped::new(scope_idx, *expr_idx)))
    }
    pub fn is_fn_def_of_idx_pure(&self, fn_def_idx: &Idx<FnDef>) -> bool {
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
        let is_reference_pure = self.is_pure(&indexing.scoped_reference());
        let are_indices_pure = indexing
            .indices
            .iter()
            .map(|spanned| Scoped::new(indexing.scope_idx, spanned.index))
            .all(|scoped| self.is_pure(&scoped));
        is_reference_pure && are_indices_pure
    }
    pub fn is_let_binding_pure(&self, indices: &[Idx<LetBinding>]) -> bool {
        let scope = self.get_current_scope();
        let allocator = VarSelector::select_alloc(scope);
        indices.iter().all(|lbi| {
            let binding = &allocator.definitions[*lbi];
            let scoped = Scoped::new(binding.scope_idx, binding.spanned_expr_idx.index);
            !binding.identifier.is_mut && self.is_pure(&scoped)
        })
    }
    pub fn is_maybe_pure(&self, maybe: &Maybe<Type>) -> bool {
        match maybe {
            Maybe::Checked(t) => self.is_type_pure(t),
            Maybe::None => false,
            Maybe::Unchecked(_types) => false,
        }
    }
    pub fn are_parameters_pure(&self, params: &[Param]) -> bool {
        params.iter().all(|p| match p {
            Param::Generic { is_mut, .. } => !*is_mut,
            Param::Named {
                is_mut, param_type, ..
            } => !*is_mut && self.is_type_pure(param_type),
            Param::SelfRef { is_mut, .. } => !*is_mut,
        })
    }
    pub fn are_types_pure(&self, ty: &[Type]) -> bool {
        ty.iter().all(|t| self.is_type_pure(t))
    }
    pub fn is_type_pure(&self, ty: &Type) -> bool {
        use Type::*;
        match ty {
            Buffer { data_type, .. } => self.is_maybe_pure(data_type),
            FnSig {
                param_types,
                return_type,
            } => self.is_type_pure(return_type) && self.are_types_pure(param_types),
            Ptr { of, is_mut } => !*is_mut && self.is_type_pure(of),
            // ! FIXME: PurityEnv should have a map to identify impure types
            StructAsType(status) => match status {
                Status::Pending(idx) => false,
                Status::Resolved(idx) => false,
            },
            Struct { fields, .. } => self.are_types_pure(fields),
            Tensor { data_type, .. } => self.is_maybe_pure(data_type),
            Tuple(types) => self.are_types_pure(types),
            _ => true,
        }
    }
    pub fn is_unary_op_pure(&self, unary: &Unary) -> bool {
        use UnaryOp::*;
        match unary.op() {
            Ref => self.is_pure(&unary.operand().0),
            _ => true,
        }
    }
    pub fn is_value_pure(&self, value: &Value) -> bool {
        match value {
            Value::Buffer { idx, data_type, .. } => {
                let scope = self.get_current_scope();
                let metadata = &scope.metadata.buffers;
                let is_buffer_itself_pure = metadata
                    .get(&idx.elm)
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
                .map(|(_, expr_idx)| {
                    Scoped::new(
                        struct_literal.internal_with_field_values.scope_idx,
                        *expr_idx,
                    )
                })
                .all(|scoped_expr_idx| self.is_pure(&scoped_expr_idx)),
            Value::Tensor { idx, data_type, .. } => {
                let scope = self.get_current_scope();
                let metadata = &scope.metadata.buffers;
                let is_tensor_itself_pure = metadata
                    .get(&idx.elm)
                    .is_some_and(|m| m.common.purity.is_pure());
                is_tensor_itself_pure && self.is_maybe_pure(data_type)
            }
            _ => true,
        }
    }
    pub fn is_pure(&self, scoped_expr_idx: &ScopedExprIdx) -> bool {
        self.expr_purity.contains(scoped_expr_idx)
    }
    pub fn set_as_pure(&mut self, expr_idx: ExprIdx) {
        let scope_idx = self.current_scope_cursor;
        let scoped = Scoped::new(scope_idx, expr_idx);
        self.expr_purity.insert(scoped);
    }
    pub fn is_stmt_pure(&self, stmt: &Stmt) -> bool {
        match stmt {
            Stmt::ControlFlow(control_flow) => self.is_controlflow_pure(control_flow),
            Stmt::Expr(idx) => self.is_pure(idx),
            Stmt::FnDef(idx) => self.is_fn_def_of_idx_pure(idx),
            Stmt::Impl(imp) => self.is_impl_pure(imp),
            Stmt::Jump(jump) => match jump {
                Jump::Continue => true,
                Jump::Break(idx) => idx.is_some_and(|expr_idx| self.is_pure(&expr_idx)),
            },
            Stmt::Loop(l) => match l {
                Loop::While(condition, block) => block.is_pure && self.is_pure(&condition.0),
                Loop::For(_identifiers, in_expr, block) => {
                    block.is_pure && self.is_pure(&in_expr.0)
                }
            },
            Stmt::Return(ret) => self.is_pure(&ret.0),
            Stmt::Semi(semi) => self.is_pure(&semi.0),
            Stmt::StructDef(_idx) => true,
            Stmt::LetBinding(thin_vec) => self.is_let_binding_pure(thin_vec.as_slice()),
        }
    }

    pub fn is_stmt_of_idx_pure(&self, stmt_idx: &ScopedStmtIdx) -> bool {
        let scope = &self.scopes[stmt_idx.scope_idx];
        let stmt = &scope.statements[stmt_idx.elm];
        self.is_stmt_pure(stmt)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use crate::test::program_into_hir_and_node;

    use crate::{
        expression::Expr,
        resolution::{Baggage, Reference},
    };
    use ast::{
        ast_root_from_assert_no_err, cast_node_into_type, expression::Expr as ASTExpr,
        statement::Stmt as ASTStmt,
    };
    use parameterized_test::create;

    create! {
        expression_purity_test,
        (program, expr_purity), {
            let (expr_node, mut hir) = program_into_hir_and_node(program);
            let expr_node = expr_node.first_child().unwrap();
            let ast_expr= cast_node_into_type::<ASTExpr>(&expr_node);

            let expr_idx = hir.lower_expr_as_idx(&ast_expr).expect("should have been lowered");
            assert_eq!(expr_purity, hir.is_pure(&expr_idx));
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
        pure_struct_indexing: ("Struct{}.a[0]", PURE),
    }

    fn resolved_purity_of_fn_call(hir: &HIRBuilder, fn_expr_idx: ScopedExprIdx) -> bool {
        let expr = hir.get_expr(&fn_expr_idx);
        let unresolved_call_reference = if let Expr::FnCall(unresolved_call_reference) = expr {
            unresolved_call_reference
        } else {
            panic!("should have been a Call");
        };

        let resolved_fn_def_reference = hir
            .resolve_fn_call(unresolved_call_reference)
            .expect("should have been resolved");
        let (obj_idx, fn_args) = if let Reference::Resolved {
            baggage, obj_idx, ..
        } = resolved_fn_def_reference
        {
            (
                obj_idx,
                if let Baggage::Arg(fn_args) = baggage {
                    fn_args
                } else {
                    panic!("should have a baggage of Args");
                },
            )
        } else {
            panic!("should have a resolved reference");
        };
        let resolved_call = Call {
            on: On::Binding(obj_idx),
            arguments: fn_args,
        };
        hir.is_call_pure(&resolved_call)
    }

    fn resolved_purity_of_var_ref(hir: &HIRBuilder, var_ref_expr_idx: ScopedExprIdx) -> bool {
        let expr = hir.get_expr(&var_ref_expr_idx);
        let unresolved_reference = if let Expr::VarRef(unresolved_reference) = expr {
            unresolved_reference
        } else {
            panic!("should have been a VarRef");
        };

        let resolved_variable_reference = hir
            .resolve_var_ref(&unresolved_reference)
            .expect("should have been resolved");
        let let_binding_idx = if let Reference::Resolved {
            baggage, obj_idx, ..
        } = resolved_variable_reference
        {
            assert_eq!(baggage, Baggage::None);
            obj_idx
        } else {
            panic!("should have a resolved reference");
        };
        hir.is_let_binding_pure([let_binding_idx].as_slice())
    }

    create! {
        call_expression_purity_test,
        (definition_program, referencing_program, resolver, exp_purity), {
            let (_, mut hir) = program_into_hir_and_node("");

            let def_ast_root = ast_root_from_assert_no_err(definition_program);
            let def_node = def_ast_root.get_root().first_child().unwrap();
            let ast_stmt = cast_node_into_type::<ASTStmt>(&def_node);

            hir.lower_statement(&ast_stmt).expect("statement should have been lowered");
            assert!(hir.errors.is_empty());

            let ref_ast_root = ast_root_from_assert_no_err(referencing_program);
            let ref_node = ref_ast_root.get_root().first_child().unwrap();
            let ast_expr = cast_node_into_type::<ASTExpr>(&ref_node);

            let expr_idx = hir.lower_expr_as_idx(&ast_expr).expect("expression should have been lowered");

            // first we need to resolve
            let resolved_purity = resolver(&hir, expr_idx);

            assert_eq!(exp_purity, resolved_purity);
        }
    }
    call_expression_purity_test! {
        impure_fn_call: ("fn impure(s: &mut str) {s[0] = '_';}", "impure(s)", resolved_purity_of_fn_call, IMPURE),
        pure_fn_call: ("fn pure(s: &str) {}", "pure(s)", resolved_purity_of_fn_call, PURE),
        impure_parameter_call: ("fn pure(s: &str) {}", "pure(s += \"impurty\")", resolved_purity_of_fn_call, IMPURE),
        // Note: added spaces to mimick var to be the next statement to pass span checks
        impure_struct: ("let mut var = Struct{};", "                                var", resolved_purity_of_var_ref, IMPURE),
    }
}
