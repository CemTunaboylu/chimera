use smol_str::{SmolStr, ToSmolStr};
use syntax::{language::SyntaxNode, syntax_kind::SyntaxKind};
use thin_vec::ThinVec;

use crate::{
    ast::ASTResult,
    delimited::Block as ASTBlock,
    errors::ASTError,
    expression::Expr,
    lang_elems::{
        children_with_tokens_without_unwanted, error_for_node, filtered_children_with_tokens,
        first_child_of_kind, get_children_in, get_first_child_in, get_token_of_errs,
    },
    literal::Literal,
    parameter::Param,
    types::Type,
};

#[derive(Clone, Debug, PartialEq)]
pub struct RetType(SyntaxNode);

impl RetType {
    pub fn return_type(&self) -> Option<Type> {
        if let Some(c) = children_with_tokens_without_unwanted(
            &self.0,
            [SyntaxKind::Whitespace, SyntaxKind::RArrow].as_slice(),
        )
        .last()
        {
            Type::try_from(c).ok()
        } else {
            None
        }
    }
    pub fn get_return_type_from(fn_def_or_lambda: &SyntaxNode) -> Option<RetType> {
        Some(RetType(first_child_of_kind(
            fn_def_or_lambda,
            SyntaxKind::RetType,
        )?))
    }
}

fn get_block_from(fn_def_or_lambda: &SyntaxNode) -> ASTResult<ASTBlock> {
    if let Some(block) = get_first_child_in(fn_def_or_lambda, SyntaxKind::Block) {
        ASTBlock::try_from(&block)
    } else {
        let block_to_be = fn_def_or_lambda.last_child();
        Err(ASTError::new(
            fn_def_or_lambda.text_range().into(),
            SyntaxKind::Block,
            block_to_be.as_ref(),
        ))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Callable {
    parameters: ThinVec<Param>,
    return_type: Option<RetType>,
    body: ASTBlock,
}

impl Callable {
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

impl TryFrom<&SyntaxNode> for Callable {
    type Error = ASTError;

    fn try_from(fn_def_or_lambda: &SyntaxNode) -> Result<Self, Self::Error> {
        let mut parameters = ThinVec::new();
        for param in Param::get_params_nodes_from(fn_def_or_lambda) {
            parameters.push(Param::try_from(&param)?);
        }

        let return_type = RetType::get_return_type_from(fn_def_or_lambda);
        let body = get_block_from(fn_def_or_lambda)?;

        Ok(Self {
            parameters,
            return_type,
            body,
        })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Lambda(pub Callable);

impl TryFrom<&SyntaxNode> for Lambda {
    type Error = ASTError;

    fn try_from(lambda: &SyntaxNode) -> Result<Self, Self::Error> {
        Ok(Self(Callable::try_from(lambda)?))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnDef {
    name: SmolStr,
    pub callable: Callable,
}

impl FnDef {
    pub fn name(&self) -> &SmolStr {
        &self.name
    }
}

impl TryFrom<&SyntaxNode> for FnDef {
    type Error = ASTError;

    fn try_from(fn_def_node: &SyntaxNode) -> Result<Self, Self::Error> {
        let name = get_token_of_errs(fn_def_node, SyntaxKind::Ident)?
            .text()
            .to_smolstr();

        Ok(Self {
            name,
            callable: Callable::try_from(fn_def_node)?,
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
            Err(error_for_node(fn_arg_node, "to have a child"))
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum On {
    Binding(SmolStr),
    Literal(Literal),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Call {
    pub on: On,
    pub arguments: ThinVec<FnArg>,
}

impl TryFrom<&SyntaxNode> for Call {
    type Error = ASTError;

    fn try_from(call_node: &SyntaxNode) -> Result<Self, Self::Error> {
        let mut arguments = ThinVec::new();
        for arg in FnArg::get_fn_arg_nodes_from(call_node) {
            arguments.push(FnArg::try_from(&arg)?);
        }

        let name_or_err = get_token_of_errs(call_node, SyntaxKind::Ident);
        let call = if let Ok(name) = name_or_err {
            let n = name.text().to_smolstr();
            Self {
                on: On::Binding(n),
                arguments,
            }
        } else if let Some(node_or_token) =
            filtered_children_with_tokens(call_node, SyntaxKind::Literal).first()
        {
            let literal = Literal::try_from(node_or_token)?;
            Self {
                on: On::Literal(literal),
                arguments,
            }
        } else {
            return Err(name_or_err.expect_err("must have been an error"));
        };

        Ok(call)
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::{
        ast_root_from, cast_node_into_type,
        delimited::Block,
        function::Lambda,
        literal::{Literal, Value},
        operation::Binary,
        parameter::{By, ParamType},
        statement::Stmt,
    };
    use parameterized_test::create;
    use thin_vec::thin_vec;

    create! {
        happy_path_func_def_test,
        (program, exp_name, ret_type_some, params_count), {
            let ast_root = ast_root_from(program);
            let fn_def_node = ast_root.get_root().first_child().unwrap();
            let fn_def = cast_node_into_type::<FnDef>(&fn_def_node);
            assert_eq!(exp_name, fn_def.name());
            assert!(ret_type_some == fn_def.callable.return_type.is_some());
            assert!(params_count == fn_def.callable.parameters.len());
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
        returning_structure_as_param: (
        "fn foo(i: &Structure) -> bool { i.can_foo() }",
         "foo", RETURNS, 1,
        ),
    }
    create! {
        happy_path_lambda_test,
        (program, ret_type_some, params_count), {
            let ast_root = ast_root_from(program);
            let literal_node = ast_root.get_root().first_child().unwrap();
            let lambda_node = literal_node.first_child().unwrap();
            let lambda= cast_node_into_type::<Lambda>(&lambda_node);
            assert!(ret_type_some == lambda.0.return_type.is_some());
            assert!(params_count == lambda.0.parameters.len());
        }
    }

    happy_path_lambda_test! {
        returning_lambda: (
         " |s: &mut Structure| -> i32 {s.lifetime()}",
         RETURNS, 1,
        ),
        non_returning_lambda: (
         "|a: &mut i32, b: i32| { a += b; }",
         DOES_NOT_RETURN, 2,
        ),
    }

    create! {
        happy_path_func_call_test,
        (program, exp_name, exp_arg_len), {
            let ast_root = ast_root_from(program);
            let call_node = ast_root.get_root().first_child().unwrap();
            let call = cast_node_into_type::<Call>(&call_node);
            assert_eq!(On::Binding(exp_name.into()), call.on);
            assert!(exp_arg_len == call.arguments.len());
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
        let call_node = ast_root.get_root().first_child().unwrap();
        let call: Call = cast_node_into_type::<Call>(&call_node);
        assert_eq!(call.on, On::Binding("foo".into()));
        let args = call.arguments;
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
        } else {
            unreachable!()
        }
    }

    #[test]
    fn detailed_testing_of_lambda_returning_lambda() {
        let program = "|a: &mut i32, b: i32| { |f| {a += b*f} }";
        let ast_root = ast_root_from(program);
        let literal_node = ast_root.get_root().first_child().unwrap();
        let lambda_node = literal_node.first_child().unwrap();
        let lambda: Lambda = cast_node_into_type::<Lambda>(&lambda_node);
        assert_eq!(
            lambda.0.parameters,
            ThinVec::from([
                Param::Named("a".into(), ParamType(By::RefMut, Type::Integer32)),
                Param::Named("b".into(), ParamType(By::Value, Type::Integer32)),
            ]),
        );
        assert!(lambda.0.return_type.is_none());
        assert!(matches!(lambda.0.body, Block::Returning(_)));
        if let Block::Returning(stmts) = lambda.0.body {
            assert!(matches!(stmts.as_slice(), &[Stmt::Expr(_)]));
            if let [Stmt::Expr(Expr::Literal(Literal(Value::Lambda(l))))] = stmts.as_ref() {
                assert_eq!(l.0.parameters, thin_vec![Param::Generic("f".into())]);
                assert!(l.0.return_type.is_none());
                assert!(matches!(l.0.body, Block::Returning(_)));
                if let Block::Returning(stmts) = &l.0.body {
                    assert!(matches!(
                        stmts.as_slice(),
                        &[Stmt::Expr(Expr::Infix(Binary::Infix(_),))]
                    ));
                } else {
                    unreachable!()
                }
            } else {
                unreachable!()
            };
        } else {
            unreachable!()
        };
    }

    #[test]
    fn detailed_testing_of_direct_call_on_lambda_literal() {
        let program = "let two = |a| {2*a}(1);";
        let ast_root = ast_root_from(program);
        let var_def_node = ast_root.get_root().first_child().unwrap();
        let eq_infix_node = var_def_node.first_child().unwrap();
        let call_node = eq_infix_node.last_child().unwrap();
        let call: Call = cast_node_into_type::<Call>(&call_node);
        assert_eq!(
            call.arguments,
            thin_vec![FnArg(Expr::Literal(Literal(Value::Int(1))))],
        );
        assert!(matches!(
            call.on,
            On::Literal(Literal(Value::Lambda(Lambda(_))))
        ));
    }
}
