use smol_str::{SmolStr, ToSmolStr};
use syntax::{language::SyntaxNode, syntax_kind::SyntaxKind};
use thin_vec::ThinVec;

use crate::{
    ast::ASTResult,
    delimited::Block as ASTBlock,
    errors::ASTError,
    expression::Expr,
    lang_elems::{
        error_for_node, first_child_of_kind, get_children_in, get_first_child_in, get_token_of_errs,
    },
    parameter::Param,
    types::Type,
};
use SyntaxKind::*;

#[derive(Clone, Debug, PartialEq)]
pub struct RetType(SyntaxNode);

impl RetType {
    pub fn return_type(&self) -> Option<Type> {
        Type::try_from(&self.0.first_child().unwrap()).ok()
    }
}

#[derive(Debug)]
pub struct FnDef {
    name: SmolStr,
    parameters: ThinVec<Param>,
    return_type: Option<RetType>,
    body: ASTBlock,
}

impl Clone for FnDef {
    fn clone(&self) -> Self {
        Self {
            name: self.name.clone(),
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
        self.name == other.name
            && self.parameters.starts_with(other.parameters.as_ref())
            && self.return_type == other.return_type
            && self.body == other.body
    }
}

impl FnDef {
    fn get_return_type_from(fn_def_node: &SyntaxNode) -> Option<RetType> {
        Some(RetType(first_child_of_kind(
            fn_def_node,
            SyntaxKind::RetType,
        )?))
    }

    fn get_block_from(fn_def_node: &SyntaxNode) -> ASTResult<ASTBlock> {
        if let Some(block) = get_first_child_in(fn_def_node, Block) {
            ASTBlock::try_from(&block)
        } else {
            let block_to_be = fn_def_node.last_child();
            Err(ASTError::new(
                fn_def_node.text_range().into(),
                Block,
                block_to_be.as_ref(),
            ))
        }
    }
    pub fn name(&self) -> &SmolStr {
        &self.name
    }
    pub fn body(&self) -> &ASTBlock {
        &self.body
    }
    pub fn parameters(&self) -> &ThinVec<Param> {
        &self.parameters
    }
    pub fn return_type(&self) -> Option<&RetType> {
        self.return_type.as_ref()
    }
}

impl TryFrom<&SyntaxNode> for FnDef {
    type Error = ASTError;

    fn try_from(fn_def_node: &SyntaxNode) -> Result<Self, Self::Error> {
        let name = get_token_of_errs(fn_def_node, Ident)?.text().to_smolstr();
        let mut parameters = ThinVec::new();
        for param in Param::get_params_nodes_from(fn_def_node) {
            parameters.push(Param::try_from(&param)?);
        }

        let return_type = Self::get_return_type_from(fn_def_node);
        let body = Self::get_block_from(fn_def_node)?;

        Ok(Self {
            name,
            parameters,
            return_type,
            body,
        })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnArg(pub Expr);

impl FnArg {
    fn get_fn_arg_nodes_from(node: &SyntaxNode) -> ThinVec<SyntaxNode> {
        get_children_in(node, SyntaxKind::FnArg)
    }
}

impl TryFrom<&SyntaxNode> for FnArg {
    type Error = ASTError;

    fn try_from(fn_arg_node: &SyntaxNode) -> Result<Self, Self::Error> {
        if let Some(child) = fn_arg_node.first_child() {
            let expr = Expr::try_from(&child)?;
            Ok(Self(expr))
        } else {
            return Err(error_for_node(fn_arg_node, "to have a child"));
        }
    }
}
#[derive(Clone, Debug, PartialEq)]
pub struct FnCall {
    name: SmolStr,
    arguments: ThinVec<FnArg>,
}

impl FnCall {
    pub fn name(&self) -> &SmolStr {
        &self.name
    }
    pub fn arguments(&self) -> &[FnArg] {
        self.arguments.as_ref()
    }
}

impl TryFrom<&SyntaxNode> for FnCall {
    type Error = ASTError;

    fn try_from(fn_call_node: &SyntaxNode) -> Result<Self, Self::Error> {
        let name = get_token_of_errs(fn_call_node, Ident)?.text().to_smolstr();

        let mut arguments = ThinVec::new();
        for arg in FnArg::get_fn_arg_nodes_from(fn_call_node) {
            arguments.push(FnArg::try_from(&arg)?);
        }

        Ok(Self { name, arguments })
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::{
        operation::Binary,
        {ast_root_from, cast_node_into_type},
    };
    use parameterized_test::create;

    create! {
        happy_path_func_def_test,
        (program, exp_name, ret_type_some, params_count), {
            let ast_root = ast_root_from(program);
            let fn_def_node = ast_root.get_root().first_child().unwrap();
            let fn_def = cast_node_into_type::<FnDef>(&fn_def_node);
            assert_eq!(exp_name, fn_def.name());
            assert!(ret_type_some == fn_def.return_type.is_some());
            assert!(params_count == fn_def.parameters.len());
        }
    }

    const RETURNS: bool = true;
    const DOES_NOT_RETURN: bool = false;

    happy_path_func_def_test! {
        returning_function_def: (
         "fn f() -> i32 {return 0;}",
         "f", RETURNS, 0,
        ),
        returning_method_def: (
         "fn f(&mut self) -> Self {return 0;}",
         "f", RETURNS, 1,
        ),
        returning_method_def_without_return: (
         "fn f(&mut self) -> Self {0}",
         "f", RETURNS, 1,
        ),
        non_returning_function_def: (
         "fn f(a: &mut i32, b: i32) { a += b;}",
         "f", DOES_NOT_RETURN, 2,
        ),
        non_returning_method_def: (
         "fn f(&mut self) { self.called += 1;}",
         "f", DOES_NOT_RETURN, 1,
        ),
    }

    create! {
        happy_path_func_call_test,
        (program, exp_name, exp_arg_len), {
            let ast_root = ast_root_from(program);
            let fn_call_node = ast_root.get_root().first_child().unwrap();
            let fn_call = cast_node_into_type::<FnCall>(&fn_call_node);
            assert_eq!(exp_name, fn_call.name());
            assert!(exp_arg_len == fn_call.arguments.len());
        }
    }

    happy_path_func_call_test! {
        fn_call_with_bin_expr: ("foo(3+2)", "foo", 1),
        fn_call_with_complex_expr: ("am_i_happy(me.expectation().as_tensor() - reality.variable_tensor)", "am_i_happy", 1),
        fn_call_with_no_args: ("solitude()", "solitude", 0),
        fn_call_with_ref_mut: ("mutating(&mut matrix)", "mutating", 1),
    }

    #[test]
    fn detailed_testing_of_complex_call() {
        let program = "foo(me.expectation().as_tensor() - reality.variable_tensor)";
        let ast_root = ast_root_from(program);
        let fn_call_node = ast_root.get_root().first_child().unwrap();
        let fn_call: FnCall = cast_node_into_type::<FnCall>(&fn_call_node);
        let args = fn_call.arguments();
        assert!(matches!(args[0].0, Expr::Infix(Binary::Infix(_))));
        if let Expr::Infix(binary) = &args[0].0 {
            assert!(matches!(
                binary.lhs().unwrap(),
                Expr::Infix(Binary::Infix(_))
            ));
            assert!(matches!(
                binary.rhs().unwrap(),
                Expr::Infix(Binary::Infix(_))
            ));
            assert_eq!(SyntaxKind::Minus, binary.op().unwrap());
        }
    }
}
