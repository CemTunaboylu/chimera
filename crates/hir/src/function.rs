use hir_macro::{scoped, with_context};
use thin_vec::ThinVec;

use ast::{
    function::{
        Call as ASTCall, Callable as ASTCallable, FnArg as ASTFnArg, FnDef as ASTFnDef, On as ASTOn,
    },
    parameter::Param as ASTParam,
};

use crate::{
    HIRResult,
    builder::HIRBuilder,
    climbing::climb,
    context::UsageContext,
    delimited::Block,
    literal::Literal,
    mut_clone_with_err,
    parameter::Param,
    resolution::{Baggage, Reference, ResolutionType, Unresolved, resolve},
    scope::{
        ExprIdx, FnDefIdx, FnSelector, NameIndexed, ScopeIdx, ScopeKind, StrIdx, placeholder_idx,
    },
    typing::hindley_milner::types::Type,
};

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub struct RetType(pub Type);

// TODO: what to put in the arena now?
#[derive(Clone, Debug, Default, Eq, Hash, PartialEq, PartialOrd)]
pub struct Callable {
    pub parameters: ThinVec<Param>,
    pub return_type: Option<RetType>,
    pub body: Block,
}
#[derive(Clone, Debug, Hash, Eq, PartialEq)]
// TODO: add metadata
pub struct FnDef {
    pub name_index: StrIdx,
    pub callable: Callable,
    pub scope_idx: ScopeIdx,
}

impl NameIndexed for FnDef {
    fn set_name_index(&mut self, ix: StrIdx) {
        self.name_index = ix;
    }
    fn get_name_index(&self) -> StrIdx {
        self.name_index
    }
}

impl Default for FnDef {
    fn default() -> Self {
        Self {
            name_index: placeholder_idx(),
            callable: Default::default(),
            scope_idx: placeholder_idx(),
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub struct FnArg(pub ExprIdx);

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum On {
    Binding(FnDefIdx),
    Literal(Literal),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Call {
    pub on: On,
    pub arguments: ThinVec<FnArg>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum MayNeedResolution {
    Yes(Unresolved),
    No(Call),
}

impl HIRBuilder {
    pub fn lower_fn_args(&mut self, fn_args: &[ASTFnArg]) -> HIRResult<ThinVec<FnArg>> {
        let mut arguments = ThinVec::with_capacity(fn_args.len());

        for arg in fn_args {
            let fn_arg = self.lower_fn_arg(arg)?;
            arguments.push(fn_arg);
        }

        Ok(arguments)
    }
    pub fn lower_call(&mut self, fn_call: &ASTCall) -> HIRResult<MayNeedResolution> {
        let arguments = self.lower_fn_args(fn_call.arguments.as_slice())?;
        let may_need_resolution = match &fn_call.on {
            ASTOn::Binding(name) => MayNeedResolution::Yes(Unresolved::baggaged(
                name.clone(),
                Baggage::Arg(arguments),
                ResolutionType::Fn,
            )),
            ASTOn::Literal(literal) => {
                let literal = self.lower_literal(&literal)?;
                let on = On::Literal(literal);
                MayNeedResolution::No(Call { on, arguments })
            }
        };
        Ok(may_need_resolution)
    }
    pub fn resolve_fn_call(&self, unresolved: &Reference<FnDef>) -> HIRResult<Reference<FnDef>> {
        let scope_climbing_iter = climb(self.current_scope_cursor, &self.scopes);
        resolve::<FnDef, FnSelector>(scope_climbing_iter, unresolved)
    }

    #[with_context(UsageContext::Return)]
    pub fn lower_return_type(&mut self, callable: &ASTCallable) -> HIRResult<Option<RetType>> {
        let mut return_type = None;
        if let Some(ret_type) = callable.return_type() {
            if let Some(t) = ret_type.return_type() {
                let low_type = self.lower_type(&t)?;
                return_type = Some(RetType(low_type));
            }
        }
        Ok(return_type)
    }
    #[scoped(ScopeKind::Function)]
    pub fn lower_fn_params_and_body(
        &mut self,
        callable: &ASTCallable,
    ) -> HIRResult<(Block, ThinVec<Param>)> {
        let body = self.lower_block(callable.body())?;
        let parameters = mut_clone_with_err(
            callable.parameters().as_slice(),
            self,
            |p: &ASTParam, hir: &mut HIRBuilder| hir.lower_parameter(p),
        )?;
        Ok((body, parameters))
    }
    pub fn lower_callable(&mut self, callable: &ASTCallable) -> HIRResult<Callable> {
        let (body, parameters) = self.lower_fn_params_and_body(callable)?;
        let return_type = self.lower_return_type(callable)?;
        Ok(Callable {
            body,
            parameters,
            return_type,
        })
    }

    pub fn lower_fn_def(&mut self, fn_def: &ASTFnDef) -> HIRResult<FnDefIdx> {
        let name = fn_def.name().clone();
        let scope_idx = self.current_scope_cursor;

        let callable = self.lower_callable(&fn_def.callable)?;

        let low_fn_def = FnDef {
            callable,
            name_index: placeholder_idx(),
            scope_idx,
        };

        self.allocate::<FnDef, FnSelector>(name, low_fn_def)
    }
    #[with_context(UsageContext::FnArg)]
    pub fn lower_fn_arg(&mut self, fn_arg: &ASTFnArg) -> HIRResult<FnArg> {
        let expr_id = self.lower_expr_as_idx(&fn_arg.0)?;
        Ok(FnArg(expr_id))
    }
}

#[cfg(test)]
mod tests {
    use ast::{cast_node_into_type, literal::Literal as ASTLiteral};

    use smol_str::SmolStr;
    use thin_vec::thin_vec;

    use super::*;
    use crate::{
        builder::tests::ast_root_from, literal::Value, parameter::By, scope::into_idx,
        statement::Stmt, typing::hindley_milner::types::Status,
    };

    #[test]
    fn fn_def() {
        let program = "fn foo(i: &Structure) -> bool { i.can_foo() }";

        let ast_root = ast_root_from(program);
        let ast_fn_def =
            cast_node_into_type::<ASTFnDef>(ast_root.get_root().first_child().as_ref().unwrap());

        let mut hir_builder = HIRBuilder::new(ast_root);
        let fn_def_idx = hir_builder
            .lower_fn_def(&ast_fn_def)
            .expect("should have been ok");

        let scope_idx = hir_builder.current_scope_cursor;
        let scope = hir_builder.get_current_scope();
        let fn_defs = &scope.fn_allocator.definitions;
        let fn_names = &scope.fn_allocator.names;

        let fn_def = &fn_defs[fn_def_idx];
        let fn_name = &fn_names[fn_def.name_index];

        assert_eq!("foo", fn_name);
        assert_eq!(scope_idx, fn_def.scope_idx);

        let params = fn_def.callable.parameters.as_slice();
        if let &[Param::Named(n, By::Ref, Type::StructAsType(Status::Pending(_)))] = &params {
            assert_eq!(&SmolStr::from("i"), n);
        } else {
            unreachable!()
        }

        assert_eq!(
            &RetType(Type::Bool),
            fn_def.callable.return_type.as_ref().unwrap()
        );
    }

    #[test]
    fn lambda() {
        let program = "|i| -> bool { true }";

        let ast_root = ast_root_from(program);
        let ast_call =
            cast_node_into_type::<ASTLiteral>(ast_root.get_root().first_child().as_ref().unwrap());

        let mut hir_builder = HIRBuilder::new(ast_root);
        let lambda_literal = hir_builder
            .lower_literal(&ast_call)
            .expect("should have been ok");

        if let Literal(Value::Lambda(callable)) = lambda_literal {
            if let &[Param::Generic(n)] = &callable.parameters.as_slice() {
                assert_eq!(&SmolStr::from("i"), n);
            } else {
                unreachable!()
            }
            assert_eq!(&RetType(Type::Bool), callable.return_type.as_ref().unwrap());
            let Block {
                scope_idx,
                returns,
                statements,
                ..
            } = callable.body;
            // note: scopeIdx(2) since lower_fn_params_and_body starts a scope and then Block starts its own
            assert_eq!(into_idx(2), scope_idx);
            assert_eq!(&[0], returns.as_slice());
            assert_eq!(&[Stmt::Expr(into_idx(1))], statements.as_slice());
        } else {
            unreachable!()
        }
    }

    #[test]
    fn fn_call() {
        let program = "foo(i)";

        let ast_root = ast_root_from(program);
        let ast_call =
            cast_node_into_type::<ASTCall>(ast_root.get_root().first_child().as_ref().unwrap());

        let mut hir_builder = HIRBuilder::new(ast_root);
        let unresolved_call: Unresolved = match hir_builder
            .lower_call(&ast_call)
            .expect("should have been ok")
        {
            MayNeedResolution::Yes(unresolved) => unresolved,
            MayNeedResolution::No(_call) => unreachable!(),
        };

        assert_eq!(SmolStr::from("foo"), unresolved_call.name);
        assert_eq!(
            Baggage::Arg(thin_vec![FnArg(into_idx(1))]),
            unresolved_call.baggage
        );
        assert_eq!(ResolutionType::Fn, unresolved_call.for_type);
    }

    #[test]
    fn direct_call_on_lambda_literal() {
        let program = "|i| -> bool { true }(1)";

        let ast_root = ast_root_from(program);
        let ast_call =
            cast_node_into_type::<ASTCall>(ast_root.get_root().first_child().as_ref().unwrap());

        let mut hir_builder = HIRBuilder::new(ast_root);
        let call_lambda_literal = match hir_builder
            .lower_call(&ast_call)
            .expect("should have been ok")
        {
            MayNeedResolution::Yes(_) => unreachable!(),
            MayNeedResolution::No(call) => call,
        };
        if let On::Literal(Literal(Value::Lambda(callable))) = call_lambda_literal.on {
            if let &[Param::Generic(n)] = &callable.parameters.as_slice() {
                assert_eq!(&SmolStr::from("i"), n);
            } else {
                unreachable!()
            }
            assert_eq!(&RetType(Type::Bool), callable.return_type.as_ref().unwrap());
            let Block {
                scope_idx,
                returns,
                statements,
                ..
            } = callable.body;
            // note: scopeIdx(2) since lower_fn_params_and_body starts a scope and then Block starts its own
            assert_eq!(into_idx(2), scope_idx);
            assert_eq!(&[0], returns.as_slice());
            assert_eq!(&[Stmt::Expr(into_idx(1))], statements.as_slice());
        } else {
            unreachable!()
        }
        assert_eq!(
            &[FnArg(into_idx(1))],
            call_lambda_literal.arguments.as_slice()
        );
    }
}
