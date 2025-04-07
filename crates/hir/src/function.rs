use scoped_macro::scoped;
use thin_vec::ThinVec;

use ast::function::{FnArg as ASTFnArg, FnCall as ASTFnCall, FnDef as ASTFnDef};

use crate::{
    HIRResult,
    builder::HIRBuilder,
    delimited::Block,
    parameter::Param,
    resolution::{Baggage, ResolutionType, Unresolved},
    scope::{ExprIdx, FnDefIdx, ScopeIdx, ScopeKind, StrIdx},
    types::Type,
};

#[derive(Clone, Debug, PartialEq)]
pub struct RetType(Type);

#[derive(Debug)]
// TODO: add metadata
pub struct FnDef {
    pub name_index: StrIdx,
    pub scope_idx: ScopeIdx,
    pub parameters: ThinVec<Param>,
    pub return_type: Option<RetType>,
    pub body: Block,
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

#[derive(Clone, Debug, PartialEq)]
pub struct FnArg(ExprIdx);

#[derive(Clone, Debug, PartialEq)]
pub struct FnCall {
    index: FnDefIdx,
    arguments: ThinVec<FnArg>,
}

impl HIRBuilder {
    pub fn lower_fn_call(&mut self, fn_call: &ASTFnCall) -> HIRResult<Unresolved> {
        let name = fn_call.name().clone();
        let mut arguments = ThinVec::with_capacity(fn_call.arguments().len());

        for arg in fn_call.arguments() {
            let ast_expr = &arg.0;
            let expr_id = self.lower_expr_as_idx(ast_expr)?;
            arguments.push(FnArg(expr_id));
        }

        Ok(Unresolved::baggaged(
            name,
            Baggage::Arg(arguments),
            ResolutionType::Fn,
        ))
    }
    #[scoped(ScopeKind::Function)]
    pub fn lower_fn_params_and_body(
        &mut self,
        fn_def: &ASTFnDef,
    ) -> HIRResult<(Block, ThinVec<Param>)> {
        let mut parameters = ThinVec::with_capacity(fn_def.parameters().len());

        for param in fn_def.parameters() {
            let p = self.lower_parameter(&param)?;
            parameters.push(p);
        }

        let body = self.lower_block(fn_def.body())?;
        Ok((body, parameters))
    }
    pub fn lower_fn_def(&mut self, fn_def: &ASTFnDef) -> HIRResult<FnDefIdx> {
        let name = fn_def.name().clone();
        let scope_idx = self.current_scope_cursor;

        let (body, parameters) = self.lower_fn_params_and_body(fn_def)?;

        let mut return_type = None;
        if let Some(ret_type) = fn_def.return_type() {
            let typ = ret_type.return_type();
            if let Some(t) = typ {
                let low_type = self.lower_type(&t)?;
                return_type = Some(RetType(low_type));
            }
        }

        let current_scope = self.get_current_scope_mut();

        let name_index = current_scope.fn_names.alloc(name.clone());

        let low_fn_def = FnDef {
            body,
            name_index,
            parameters,
            return_type,
            scope_idx,
        };

        let fn_def_idx = current_scope.fn_defs.alloc(low_fn_def);
        self.insert_fn_def_in_trie(&name, fn_def_idx);

        Ok(fn_def_idx)
    }
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
    use crate::{builder::tests::ast_root_from, parameter::By, tensor::Shape};

    #[test]
    fn fn_def() {
        let program = "fn mat_mul(t1: &tensor<100, 90>, t2: &tensor<90, 100>) -> tensor<100,100> { t1.matmul(t2) \n}";

        let ast_root = ast_root_from(program);
        let ast_fn_def =
            cast_node_into_type::<ASTFnDef>(ast_root.get_root().first_child().as_ref().unwrap());

        let mut hir_builder = HIRBuilder::new(ast_root);
        let fn_def_idx = hir_builder
            .lower_fn_def(&ast_fn_def)
            .expect("should have been ok");

        let scope_idx = hir_builder.current_scope_cursor;
        let scope = hir_builder.get_current_scope();
        let fn_defs = &scope.fn_defs;
        let fn_names = &scope.fn_names;

        let fn_def = &fn_defs[fn_def_idx];
        let fn_name = &fn_names[fn_def.name_index];

        assert_eq!("mat_mul", fn_name);
        assert_eq!(scope_idx, fn_def.scope_idx);

        assert_eq!(
            &Param::Named(
                SmolStr::from("t1"),
                By::Ref,
                Type::Tensor {
                    shape: Some(Shape(thin_vec![100, 90])),
                    datatype: None
                }
            ),
            fn_def.parameters.get(0).unwrap()
        );
        assert_eq!(
            &Param::Named(
                SmolStr::from("t2"),
                By::Ref,
                Type::Tensor {
                    shape: Some(Shape(thin_vec![90, 100])),
                    datatype: None
                }
            ),
            fn_def.parameters.get(1).unwrap()
        );

        assert_eq!(
            &RetType(Type::Tensor {
                shape: Some(Shape(thin_vec![100, 100])),
                datatype: None
            }),
            fn_def.return_type.as_ref().unwrap()
        );
    }
}
