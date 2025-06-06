use smol_str::{SmolStr, ToSmolStr};
use syntax::{
    bitset::SyntaxKindBitSet, is_a_type, language::SyntaxToken, syntax_kind::SyntaxKind::*,
};
use syntax::{
    language::{NodeOrToken, SyntaxNode},
    syntax_kind::SyntaxKind,
};
use thin_vec::{ThinVec, thin_vec};

use crate::{
    ast::ASTResult,
    delimited::Tuple as ASTTuple,
    errors::ASTError,
    expression::Expr,
    function::RetType as ASTRetType,
    lang_elems::{
        children_with_tokens_without_unwanted, ensure_node_kind_is_any, error_for_token,
        get_children_with_tokens_in_f, get_first_child_in, get_token_of_errs,
        vector_of_children_and_tokens_as,
    },
    literal::{Literal, Value, parse_into},
    mutable::Mut as ASTMut,
    operation::Binary,
    self_ref::SelfRef,
};

#[derive(Clone, Debug, PartialEq)]
// a wrapper to be able to call methods of the types as classes
pub struct Class(pub Type);

// possible types are primitives + custom types i.e. structs
#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Bool,
    Buffer {
        ty: Box<Type>,
        shape: ThinVec<usize>,
    },
    Char,
    Float32,
    Fn {
        parameters: ThinVec<Type>,
        return_type: Option<Box<Type>>,
    },
    Integer32,
    Pointer {
        ty: Box<Type>,
        is_mut: bool,
    },
    String,
    Struct(SmolStr),
    SelfRef(SelfRef),
    Tensor {
        ty: Option<Box<Type>>,
        // note: tensor<1, _, 64>  -> tensor<Some(1), None , Some(64)>
        shape: ThinVec<Option<Expr>>,
    },
    Tuple(ThinVec<Type>),
}

impl Type {
    fn to_remove_for_fn_type() -> SyntaxKindBitSet {
        use SyntaxKind::*;
        [Comma, KwFn, LParen, RetType, RParen, Whitespace]
            .as_ref()
            .into()
    }
}

fn extract_type_hint_from(type_hint_node: &SyntaxNode) -> ASTResult<Type> {
    let type_node = get_children_with_tokens_in_f(type_hint_node, is_a_type);
    let type_node = type_node.first().ok_or(ASTError::with_err_msg(
        type_hint_node.text_range().into(),
        "expected a type".to_string(),
    ))?;

    let type_ = match type_node {
        NodeOrToken::Node(node) => Type::try_from(node)?,
        NodeOrToken::Token(token) => Type::try_from(token)?,
    };
    Ok(type_)
}

fn filter_dim_hints(dim_hints_node: &SyntaxNode, err: bool) -> ASTResult<ThinVec<NodeOrToken>> {
    let unwanted: SyntaxKindBitSet = [Comma, Whitespace].as_ref().into();
    let children: ThinVec<NodeOrToken> = dim_hints_node.children_with_tokens().collect();
    if err && children.is_empty() {
        return Err(ASTError::with_err_msg(
            dim_hints_node.text_range().into(),
            "expected a dimension hint".to_string(),
        ));
    }
    Ok(children
        .iter()
        .filter(|c| !unwanted.contains(c.kind()))
        .cloned()
        .collect())
}

impl TryFrom<&SyntaxNode> for Type {
    type Error = ASTError;

    fn try_from(parent_node: &SyntaxNode) -> Result<Self, Self::Error> {
        let t = match parent_node.kind() {
            PrefixUnaryOp => {
                get_token_of_errs(parent_node, And)?;
                let children = children_with_tokens_without_unwanted(
                    parent_node,
                    [And, Comma, Whitespace].as_ref(),
                );
                let child = children.first().unwrap();
                let is_mut = child.kind() == Mut;
                let ty = if is_mut {
                    let grand_children = children_with_tokens_without_unwanted(
                        child.as_node().unwrap(),
                        [Comma, KwMut, Whitespace].as_ref(),
                    );
                    let grand_child = grand_children.first().unwrap();
                    Type::try_from(grand_child)?
                } else {
                    Type::try_from(child)?
                };
                Self::Pointer {
                    ty: Box::new(ty),
                    is_mut,
                }
            }
            SelfRef => Self::SelfRef(SelfRef::try_from(parent_node)?),
            Tuple => {
                let types = vector_of_children_and_tokens_as(
                    parent_node,
                    ASTTuple::to_remove_from_tuples(),
                    |ch| Type::try_from(ch),
                )?;
                Self::Tuple(types)
            }
            TyBuffer => {
                let type_hint_node = get_first_child_in(parent_node, SyntaxKind::TypeHint).ok_or(
                    ASTError::with_err_msg(
                        parent_node.text_range().into(),
                        "buffer must have a type hint".to_string(),
                    ),
                )?;
                let th = extract_type_hint_from(&type_hint_node)?;
                let type_hint = Box::new(th);
                let dim_hints_node = get_first_child_in(parent_node, SyntaxKind::DimHints).ok_or(
                    ASTError::with_err_msg(
                        parent_node.text_range().into(),
                        "buffer must have a shape hint".to_string(),
                    ),
                )?;

                let dims = filter_dim_hints(&dim_hints_node, true)?;
                let mut shape = ThinVec::with_capacity(dims.len());

                for dim_node in dims {
                    let d = Literal::try_from(&dim_node)?;
                    if let Value::Int(dim) = d.0 {
                        shape.push(dim as usize);
                    } else {
                        return Err(ASTError::with_err_msg(
                            dim_node.text_range().into(),
                            format!("expected a usize for shape, got {:?}", d),
                        ));
                    }
                }
                Self::Buffer {
                    ty: type_hint,
                    shape,
                }
            }
            TyFn => {
                let param_nodes = children_with_tokens_without_unwanted(
                    parent_node,
                    Type::to_remove_for_fn_type(),
                );
                let mut parameters = ThinVec::with_capacity(param_nodes.len());
                for param_decl in param_nodes {
                    let typed = Type::try_from(&param_decl)?;
                    parameters.push(typed);
                }
                let return_type = ASTRetType::get_return_type_from(parent_node)
                    .and_then(|ret_type| ret_type.return_type().map(Box::new));

                Self::Fn {
                    parameters,
                    return_type,
                }
            }
            TyTensor => {
                let mut type_hint = None;
                if let Some(type_hint_node) = get_first_child_in(parent_node, SyntaxKind::TypeHint)
                {
                    let th = extract_type_hint_from(&type_hint_node)?;
                    type_hint = Some(Box::new(th));
                }

                let mut shape = thin_vec![];
                if let Some(dim_hints_node) = get_first_child_in(parent_node, SyntaxKind::DimHints)
                {
                    let dims = filter_dim_hints(&dim_hints_node, false)?;
                    shape = ThinVec::with_capacity(dims.len());
                    for dim_node in dims {
                        if dim_node.kind() == Under {
                            shape.push(None);
                            continue;
                        } else if dim_node.kind() == Int {
                            let l = Literal(Value::Int(parse_into::<i32>(
                                dim_node.as_token().unwrap().text(),
                                dim_node.text_range().into(),
                            )?));
                            shape.push(Some(Expr::Literal(l)));
                            continue;
                        }
                        let d = Some(Expr::try_from(&dim_node)?);
                        shape.push(d);
                    }
                }
                Self::Tensor {
                    ty: type_hint,
                    shape,
                }
            }
            _ => {
                ensure_node_kind_is_any(
                    parent_node,
                    [StructAsType, TyBuffer, TyFn, TyTensor].as_ref(),
                )?;
                unreachable!()
            }
        };
        Ok(t)
    }
}

impl TryFrom<&SyntaxToken> for Type {
    type Error = ASTError;

    fn try_from(token: &SyntaxToken) -> Result<Self, Self::Error> {
        let t = match token.kind() {
            TyBool => Self::Bool,
            TyChar => Self::Char,
            TyF32 => Self::Float32,
            TyI32 => Self::Integer32,
            // str slice is a ref str
            TyStr => Self::String,
            StructAsType => Self::Struct(token.text().to_smolstr()),
            _ => return Err(error_for_token(token, SyntaxKind::types())),
        };

        Ok(t)
    }
}

impl TryFrom<&NodeOrToken> for Type {
    type Error = ASTError;

    fn try_from(not: &NodeOrToken) -> Result<Self, Self::Error> {
        match not {
            NodeOrToken::Node(node) => Self::try_from(node),
            NodeOrToken::Token(token) => Self::try_from(token),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Hint {
    Dim(usize),
    Type(Type),
}

pub fn parse_type_hinted(type_hint_node: &SyntaxNode) -> ASTResult<(Expr, Type)> {
    let nodes = children_with_tokens_without_unwanted(type_hint_node, [Colon, Whitespace].as_ref());
    if nodes.len() != 2 {
        return Err(ASTError::with_err_msg(
            type_hint_node.text_range().into(),
            format!("expected 2 nodes for type hint but got {:?}", nodes),
        ));
    }
    let ident = nodes.first().unwrap();
    let type_hinted = match ident.kind() {
        Mut => Expr::Mut(ASTMut::try_from(ident.as_node().unwrap())?),
        VarRef => Expr::try_from(ident)?,
        _ => {
            return Err(ASTError::with_err_msg(
                type_hint_node.text_range().into(),
                format!(
                    "expected a valid identifier to type hint but got {:?}",
                    ident
                ),
            ));
        }
    };
    let rhs = nodes.last().unwrap();
    let ty = Type::try_from(rhs)?;
    Ok((type_hinted, ty))
}

impl Hint {
    pub fn is_type(&self) -> bool {
        matches!(self, Self::Type(_))
    }
    pub fn dim_hints(dim_hints_node: &SyntaxNode) -> ASTResult<ThinVec<Self>> {
        // comma separated values
        let mut dim_hints = ThinVec::new();
        // let hint_nodes = get_children_in(dim_hints_node, SyntaxKind::DimHint);
        // let hint_nodes = hint_nodes
        //     .iter()
        //     .filter_map(|node| node.first_child().map(|child| Literal::try_from(&child)));
        // for dim_hint in hint_nodes {
        //     let dim = match dim_hint?.0 {
        //         Value::Int(dim) if dim > 0 => dim,
        //         _ => {
        //             return Err(ASTError::with_err_msg(
        //                 dim_hints_node.text_range().into(),
        //                 "dimension must be a positive integer".to_string(),
        //             ));
        //         }
        //     };
        //     dim_hints.push(Hint::Dim(dim as usize));
        // }
        Ok(dim_hints)
    }

    pub fn type_hint(typehint_node: &SyntaxNode) -> ASTResult<Self> {
        let is_a_type_or_modifier: SyntaxKindBitSet = [PrefixUnaryOp].as_ref().into();
        let types: SyntaxKindBitSet = SyntaxKind::types().as_ref().into();
        if let Some(type_node) =
            filtered_children_with_tokens(typehint_node, is_a_type_or_modifier + types).first()
        {
            let type_ = match type_node {
                NodeOrToken::Node(node) => Type::try_from(node)?,
                NodeOrToken::Token(token) => Type::try_from(token)?,
            };
            return Ok(Self::Type(type_));
        }
        return Err(ASTError::with_err_msg(
            typehint_node.text_range().into(),
            format!("expected a valid type hint but got {:?}", typehint_node),
        ));
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::{
        ast::ASTResult, ast_root_from, cast_node_into_type, cast_token_into_type, parameter::Param,
    };
    use parameterized_test::create;

    create! {
        create_type_test_from_token,
        (program), {
        let ast_root = ast_root_from(program);
        let token = ast_root.get_root().first_token().unwrap();
        cast_token_into_type::<Type>(&token);
        }
    }

    create! {
        create_type_test_from_node,
        (program), {
        let ast_root = ast_root_from(program);
        let node = ast_root.get_root().first_child().unwrap();
        cast_node_into_type::<Type>(&node);
        }
    }

    create_type_test_from_token! {
        valid_bool: "bool",
        valid_char: "char",
        valid_f32: "f32",
        valid_i32: "i32",
        valid_str: "str",
    }

    create_type_test_from_node! {
        valid_buffer: "buffer<f32><3,3,3>",
        valid_tensor_with_no_hint: "tensor",
        valid_fully_hinted_tensor: "tensor<f32><3,3,3>",
        valid_type_hinted_tensor: "tensor<f32>",
        valid_type_partially_dim_hinted_tensor: "tensor<3,_,3>",
        valid_tuple: "(i32, char)",
    }

    #[test]
    fn valid_struct_identifier() {
        let program = "fn def(arg:Structure)";
        let ast_root = ast_root_from(program);

        let fn_def_node = ast_root.get_root().first_child().unwrap();
        let param_node = fn_def_node
            .children()
            .find(|node| node.kind() == ParamDecl)
            .unwrap();
        let struct_as_type = param_node.last_token().unwrap();
        cast_token_into_type::<Type>(&struct_as_type);
    }
    #[test]
    fn tuple_with_struct_identifier() {
        let program = "fn def(arg:(char, Structure, Structure,))";
        let ast_root = ast_root_from(program);
        let fn_def_node = ast_root.get_root().first_child().unwrap();
        let param_node = fn_def_node
            .children()
            .find(|node| node.kind() == ParamDecl)
            .unwrap();
        let param = Param::try_from(&param_node).expect("should have been parsed");
        assert_eq!(param.name, "arg");
        assert_eq!(param.is_mut, false);
        assert_eq!(param.by_ref, false);
        assert_eq!(
            param.param_type,
            Some(Type::Tuple(thin_vec![
                Type::Char,
                Type::Struct("Structure".to_smolstr()),
                Type::Struct("Structure".to_smolstr()),
            ]))
        );
    }

    #[test]
    fn valid_fn_parameter() {
        let program = "fn apply(f:fn(tensor)->tensor)";
        let ast_root = ast_root_from(program);

        let fn_def_node = ast_root.get_root().first_child().unwrap();
        let param_node = Param::get_params_nodes_from(&fn_def_node);
        let param =
            Param::try_from(param_node.first().unwrap()).expect("should have been param decl");

        assert_eq!(param.name, "f");
        assert_eq!(param.is_mut, false);
        assert_eq!(param.by_ref, false);
        assert_eq!(
            param.param_type,
            Some(Type::Fn {
                parameters: thin_vec![Type::Tensor {
                    ty: None,
                    shape: thin_vec![]
                }],
                return_type: Some(Box::new(Type::Tensor {
                    ty: None,
                    shape: thin_vec![]
                })),
            })
        );
    }

    #[test]
    fn valid_tensor_identifier() {
        let program = "tensor<i32><10,10,10>";
        let ast_root = ast_root_from(program);

        let node = ast_root.get_root().first_child().unwrap();
        cast_node_into_type::<Type>(&node);
    }
    #[test]
    fn invalid_type() {
        let program = "structure";
        let ast_root = ast_root_from(program);
        let child = ast_root.get_root().first_child().unwrap();

        let result: ASTResult<Type> = (&child).try_into();
        let err = result.expect_err("should have been errored");
        assert_eq!(
            "ASTError { err_span: 0..9, expected_but_found: \"expected TyTensor or TyFn or TyBuffer or StructAsType, but got VarRef\" }",
            format!("{:?}", err)
        );
    }
    #[test]
    #[should_panic]
    fn recovered_second_dims() {
        let program = "buffer<3,3,3><3,3,3>";
        let ast_root = ast_root_from(program);
        let buffer_type_node = ast_root.get_root().first_child().unwrap();
        cast_node_into_type::<Literal>(&buffer_type_node);
    }
    #[test]
    #[should_panic]
    fn recovered_second_types() {
        let program = "tensor<f32><i32>";
        let ast_root = ast_root_from(program);
        let tensor_init_node = ast_root.get_root().first_child().unwrap();
        println!("tensor_init_node: {:?}", tensor_init_node);
        cast_node_into_type::<Literal>(&tensor_init_node);
    }
}
