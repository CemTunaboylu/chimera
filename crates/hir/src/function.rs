use hir_macro::{scoped, with_context};
use thin_vec::ThinVec;

use ast::function::{FnArg as ASTFnArg, FnCall as ASTFnCall, FnDef as ASTFnDef};

use crate::{
    HIRResult,
    builder::HIRBuilder,
    climbing::climb,
    context::UsageContext,
    delimited::Block,
    parameter::Param,
    resolution::{Baggage, Reference, ResolutionType, Unresolved, resolve},
    scope::{
        ExprIdx, FnDefIdx, FnSelector, NameIndexed, ScopeIdx, ScopeKind, StrIdx, placeholder_idx,
    },
    typing::hindley_milner::types::Type,
};

#[derive(Clone, Debug, PartialEq)]
pub struct RetType(Type);

#[derive(Debug)]
// TODO: add metadata
pub struct FnDef {
    pub body: Block,
    pub name_index: StrIdx,
    pub parameters: ThinVec<Param>,
    pub return_type: Option<RetType>,
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

impl Clone for FnDef {
    fn clone(&self) -> Self {
        Self {
            name_index: self.name_index.clone(),
            scope_idx: self.scope_idx,
            parameters: self
                .parameters
                .iter()
                .map(|param| param.clone())
                .collect::<ThinVec<_>>(),
            return_type: self.return_type.clone(),
            body: self.body.clone(),
        }
    }
}

impl PartialEq for FnDef {
    fn eq(&self, other: &Self) -> bool {
        self.name_index == other.name_index
            && self.scope_idx == other.scope_idx
            && self.parameters.starts_with(other.parameters.as_ref())
            && self.return_type == other.return_type
            && self.body == other.body
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct FnArg(pub ExprIdx);

#[derive(Clone, Debug, PartialEq)]
pub struct FnCall {
    index: FnDefIdx,
    arguments: ThinVec<FnArg>,
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
    pub fn lower_fn_call(&mut self, fn_call: &ASTFnCall) -> HIRResult<Unresolved> {
        let name = fn_call.name().clone();
        let arguments = self.lower_fn_args(fn_call.arguments())?;

        Ok(Unresolved::baggaged(
            name,
            Baggage::Arg(arguments),
            ResolutionType::Fn,
        ))
    }
    pub fn resolve_fn_call(&self, unresolved: &Reference<FnDef>) -> HIRResult<Reference<FnDef>> {
        let scope_climbing_iter = climb(self.current_scope_cursor, &self.scopes);
        Ok(resolve::<FnDef, FnSelector>(
            scope_climbing_iter,
            unresolved,
        )?)
    }

    pub fn lower_fn_params(&mut self, fn_def: &ASTFnDef) -> HIRResult<ThinVec<Param>> {
        let mut parameters = ThinVec::with_capacity(fn_def.parameters().len());

        for param in fn_def.parameters() {
            let p = self.lower_parameter(&param)?;
            parameters.push(p);
        }

        Ok(parameters)
    }
    #[with_context(UsageContext::Return)]
    pub fn lower_return_type(&mut self, fn_def: &ASTFnDef) -> HIRResult<Option<RetType>> {
        let mut return_type = None;
        if let Some(ret_type) = fn_def.return_type() {
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
        fn_def: &ASTFnDef,
    ) -> HIRResult<(Block, ThinVec<Param>)> {
        let body = self.lower_block(fn_def.body())?;
        let parameters = self.lower_fn_params(fn_def)?;
        Ok((body, parameters))
    }
    pub fn lower_fn_def(&mut self, fn_def: &ASTFnDef) -> HIRResult<FnDefIdx> {
        let name = fn_def.name().clone();
        let scope_idx = self.current_scope_cursor;

        let (body, parameters) = self.lower_fn_params_and_body(fn_def)?;

        let return_type = self.lower_return_type(fn_def)?;

        let low_fn_def = FnDef {
            body,
            name_index: placeholder_idx(),
            parameters,
            return_type,
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
    use ast::cast_node_into_type;

    use smol_str::SmolStr;
    use thin_vec::thin_vec;

    use super::*;
    use crate::{
        builder::tests::ast_root_from, parameter::By, typing::hindley_milner::types::Maybe,
    };

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
