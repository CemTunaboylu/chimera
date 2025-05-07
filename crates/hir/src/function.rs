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
    errors::HIRError,
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
pub struct RetType(Type);

// TODO: what to put in the arena now?
#[derive(Clone, Debug, Default, Eq, Hash, PartialEq, PartialOrd)]
pub struct Callable {
    pub parameters: ThinVec<Param>,
    pub return_type: Option<RetType>,
    pub body: Block,
}
#[derive(Clone, Debug, PartialEq)]
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

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct FnArg(pub ExprIdx);

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum On {
    Binding(FnDefIdx),
    Literal(Literal),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Call {
    on: On,
    arguments: ThinVec<FnArg>,
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
    // use ast::cast_node_into_type;

    // use smol_str::SmolStr;
    // use thin_vec::thin_vec;

    // use super::*;
    // use crate::{
    //     builder::tests::ast_root_from, parameter::By, typing::hindley_milner::types::Maybe,
    // };

    // #[test]
    // fn fn_def() {
    //     let program = "fn mat_mul(t1: &tensor<i32><_, 100, 90>, t2: &tensor<i32><_ ,90, 100>) -> tensor<i32><_,100,100> { t1.matmul(t2) \n}";

    //     let ast_root = ast_root_from(program);
    //     let ast_fn_def =
    //         cast_node_into_type::<ASTFnDef>(ast_root.get_root().first_child().as_ref().unwrap());

    //     let mut hir_builder = HIRBuilder::new(ast_root);
    //     let fn_def_idx = hir_builder
    //         .lower_fn_def(&ast_fn_def)
    //         .expect("should have been ok");

    //     let scope_idx = hir_builder.current_scope_cursor;
    //     let scope = hir_builder.get_current_scope();
    //     let fn_defs = &scope.fn_allocator.definitions;
    //     let fn_names = &scope.fn_allocator.names;

    //     let fn_def = &fn_defs[fn_def_idx];
    //     let fn_name = &fn_names[fn_def.name_index];

    //     assert_eq!("mat_mul", fn_name);
    //     assert_eq!(scope_idx, fn_def.scope_idx);

    //     assert_eq!(
    //         &Param::Named(
    //             SmolStr::from("t1"),
    //             By::Ref,
    //             Type::Tensor {
    //                 shape: thin_vec![None, Some(100), Some(90)],
    //                 data_type: Some(Maybe::Checked(Box::new(Type::I32))),
    //             },
    //         ),
    //         fn_def.parameters.get(0).unwrap()
    //     );
    //     assert_eq!(
    //         &Param::Named(
    //             SmolStr::from("t2"),
    //             By::Ref,
    //             Type::Tensor {
    //                 shape: thin_vec![None, Some(90), Some(100)],
    //                 data_type: Some(Maybe::Checked(Box::new(Type::I32))),
    //             },
    //         ),
    //         fn_def.parameters.get(1).unwrap()
    //     );

    //     assert_eq!(
    //         &RetType(Type::Tensor {
    //             shape: thin_vec![None, Some(100), Some(100)],
    //             data_type: Some(Maybe::Checked(Box::new(Type::I32))),
    //         }),
    //         fn_def.return_type.as_ref().unwrap()
    //     );
    // }
}
