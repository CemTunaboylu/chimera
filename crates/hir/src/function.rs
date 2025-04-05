use smol_str::SmolStr;
use thin_vec::ThinVec;

use ast::function::{FnArg as ASTFnArg, FnCall as ASTFnCall, FnDef as ASTFnDef};

use crate::{
    HIRResult,
    delimited::Block,
    hir::{ExprIdx, FnDefIdx, HIRBuilder, StrIdx, UnresolvedFnCallIdx},
    parameter::Param,
    resolution::ResolutionResult,
    types::Type,
};

#[derive(Clone, Debug, PartialEq)]
pub struct RetType(Type);

#[derive(Debug)]
// TODO: add metadata
pub struct FnDef {
    name_index: StrIdx,
    parameters: ThinVec<Param>,
    return_type: Option<RetType>,
    body: Block,
}

impl Clone for FnDef {
    fn clone(&self) -> Self {
        Self {
            name_index: self.name_index.clone(),
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
            && self.parameters.starts_with(other.parameters.as_ref())
            && self.return_type == other.return_type
            && self.body == other.body
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnArg(ExprIdx);

#[derive(Clone, Debug, PartialEq)]
pub struct UnresolvedFnCall {
    name: SmolStr,
    arguments: ThinVec<FnArg>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnCall {
    index: FnDefIdx,
    arguments: ThinVec<FnArg>,
}

impl HIRBuilder {
    pub fn resolve_fn_call(
        &mut self,
        unresolved_fn_call: UnresolvedFnCall,
    ) -> ResolutionResult<FnCall> {
        let UnresolvedFnCall { name, arguments } = unresolved_fn_call;
        let resolved_idx = Self::resolve_with_err(&self.fn_name_to_idx_trie, &name)?;
        Ok(FnCall {
            index: resolved_idx,
            arguments,
        })
    }
    pub fn lower_fn_call(&mut self, fn_call: &ASTFnCall) -> HIRResult<UnresolvedFnCallIdx> {
        let name = fn_call.name().clone();
        let mut arguments = ThinVec::with_capacity(fn_call.arguments().len());
        for arg in fn_call.arguments() {
            let ast_expr = &arg.0;
            let expr_id = self.lower_expr_as_idx(ast_expr)?;
            arguments.push(FnArg(expr_id));
        }

        let low_fn_call = UnresolvedFnCall { name, arguments };
        let fn_call_idx = self.fn_to_resolve.alloc(low_fn_call);

        Ok(fn_call_idx)
    }
    pub fn lower_fn_def(&mut self, fn_def: &ASTFnDef) -> HIRResult<FnDefIdx> {
        let name = fn_def.name().clone();
        let mut parameters = ThinVec::with_capacity(fn_def.parameters().len());
        for param in fn_def.parameters() {
            let p = self.lower_parameter(&param)?;
            parameters.push(p);
        }
        let mut return_type = None;
        if let Some(ret_type) = fn_def.return_type() {
            let typ = ret_type.return_type();
            if let Some(t) = typ {
                let low_type = self.lower_type(&t)?;
                return_type = Some(RetType(low_type));
            }
        }

        let body = self.lower_block(fn_def.body())?;

        let name_index = self.fn_names.alloc(name.clone());

        let low_fn_def = FnDef {
            name_index,
            parameters,
            return_type,
            body,
        };

        let fn_def_idx = self.fn_arena.alloc(low_fn_def);
        self.insert_fn_def_in_trie(&name, fn_def_idx);

        Ok(fn_def_idx)
    }
    pub fn lower_fn_arg(&mut self, fn_arg: &ASTFnArg) -> HIRResult<FnArg> {
        let expr_id = self.lower_expr_as_idx(&fn_arg.0)?;
        Ok(FnArg(expr_id))
    }
}

#[cfg(test)]
mod tests {}
