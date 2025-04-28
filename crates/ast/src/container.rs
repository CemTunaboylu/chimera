use std::ops::Range;

use syntax::{language::SyntaxNode, syntax_kind::SyntaxKind};
use thin_vec::{ThinVec, thin_vec};

use crate::{
    ast::ASTResult,
    errors::ASTError,
    expression::Expr,
    lang_elems::{
        children_with_tokens_without_unwanted, err_if_empty, get_children_in, get_token_of_errs,
        unwrap_first_child_or_err,
    },
    literal::{Literal, Value, parse_into},
};

use SyntaxKind::*;

// container_literal_node is either a BufferLit or a TensorLit node
pub fn try_container_tree_from(container_literal_node: &SyntaxNode) -> ASTResult<BufferTree> {
    let child = if let Some(c) = container_literal_node.first_child() {
        c
    } else {
        // an empty buffer is still a buffer...
        return Ok(BufferTree::Init(thin_vec![]));
    };
    if child.kind() == DimValue {
        bottom_up_tree_from(container_literal_node)
    } else {
        let container_initializer = ContainerInitializer::try_from(container_literal_node)?;
        Ok(BufferTree::Uninit(container_initializer))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Shape {
    Known(ThinVec<usize>),
    MaybeUnknown(ThinVec<Option<Expr>>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum BufferTree {
    // note: nodes are the child of the DimValue node
    Init(ThinVec<SyntaxNode>),
    Uninit(ContainerInitializer),
}

impl BufferTree {
    pub fn shape(&self) -> Shape {
        match self {
            BufferTree::Init(sub_tree) => Shape::Known(thin_vec![sub_tree.len()]),
            BufferTree::Uninit(con_init) => con_init.shape.clone(),
        }
    }

    pub fn sub_tree(&self) -> Option<ThinVec<Expr>> {
        match self {
            BufferTree::Init(node_sub_tree) => Some(
                node_sub_tree
                    .iter()
                    .filter_map(|n| Expr::try_from(n).ok())
                    .collect(),
            ),
            BufferTree::Uninit(_) => None,
        }
    }
    pub fn values(&self) -> Option<ThinVec<Expr>> {
        match self {
            BufferTree::Uninit(_) => None,
            BufferTree::Init(values) => Some(
                values
                    .iter()
                    .filter_map(|n| Expr::try_from(n).ok())
                    .collect(),
            ),
        }
    }
}

pub fn buffer_tree_from(expr: &Expr) -> Option<BufferTree> {
    if let Expr::Literal(Literal(value)) = expr {
        if let Value::Buffer(buffer_tree) = value {
            return Some(buffer_tree.clone());
        } else if let Value::Tensor(buffer_tree) = value {
            return Some(buffer_tree.clone());
        }
    }
    None
}

pub fn bottom_up_tree_from(buffer_literal_node: &SyntaxNode) -> ASTResult<BufferTree> {
    let dim_values = get_children_in(buffer_literal_node, SyntaxKind::DimValue);
    let mut child_nodes_under_dim_value = thin_vec![];
    for d in dim_values {
        // note: DimValue will always have one child even if it is a buffer, it will be wrapped by BufferLit
        let child = unwrap_first_child_or_err(&d)?;
        child_nodes_under_dim_value.push(child);
    }
    Ok(BufferTree::Init(child_nodes_under_dim_value))
}

#[derive(Clone, Debug, PartialEq)]
pub struct ContainerInitializer {
    value: Box<Expr>,
    shape: Shape,
}
impl ContainerInitializer {
    pub fn value(&self) -> &Expr {
        &self.value
    }
    pub fn buffer_shape(&self) -> &Shape {
        &self.shape
    }
}
impl Shape {
    fn from_int_buffer(dim_hints_node: &SyntaxNode) -> ASTResult<Self> {
        let hints =
            children_with_tokens_without_unwanted(dim_hints_node, [Comma, Whitespace].as_ref());
        let mut shape = ThinVec::new();
        for h in hints {
            if h.kind() == Int {
                let i: usize = parse_into(h.as_token().unwrap().text(), h.text_range().into())?;
                shape.push(i as usize);
                continue;
            } else {
                return Err(ASTError::with_err_msg(
                    dim_hints_node.text_range().into(),
                    format!("expects a usize for shape, got {:?}", h),
                ));
            }
        }
        Ok(Self::Known(shape))
    }
    fn from_expr_buffer(dim_hints_node: &SyntaxNode) -> ASTResult<Self> {
        let hints =
            children_with_tokens_without_unwanted(dim_hints_node, [Comma, Whitespace].as_ref());
        let mut shape = ThinVec::new();
        for h in hints {
            if h.kind() == Under {
                shape.push(None);
                continue;
            } else if h.kind() == Int {
                let l = parse_into(h.as_token().unwrap().text(), h.text_range().into())?;
                shape.push(Some(Expr::Literal(Literal(Value::Int(l)))));
                continue;
            }
            let e = Expr::try_from(&h)?;
            shape.push(Some(e));
        }
        Ok(Self::MaybeUnknown(shape))
    }
}

fn extract_shape_from_buffer(
    dim_hints_node: &SyntaxNode,
    for_kind: SyntaxKind,
) -> ASTResult<Shape> {
    let f = match for_kind {
        KwBuffer => Shape::from_int_buffer,
        KwTensor => Shape::from_expr_buffer,
        nope => {
            return Err(ASTError::new(
                dim_hints_node.text_range().into(),
                [KwBuffer, KwTensor].as_ref(),
                nope,
            ));
        }
    };
    f(&dim_hints_node)
}
// note: initializer can be in the form : [<value>; [<shape>, [<dim_1>, <dim_2>, ...]]]
fn extract_shape_from(shape_node: &SyntaxNode, for_kind: SyntaxKind) -> ASTResult<Shape> {
    if shape_node.kind() == DimHints {
        return extract_shape_from_buffer(&shape_node, for_kind);
    }
    match for_kind {
        KwBuffer => {
            let exp_int = Literal::try_from(shape_node)?;
            if let Value::Int(i) = exp_int.0 {
                let shape = thin_vec![i as usize];
                Ok(Shape::Known(shape))
            } else {
                Err(ASTError::with_err_msg(
                    shape_node.text_range().into(),
                    format!("expects a usize for shape, got {:?}", exp_int),
                ))
            }
        }
        KwTensor => {
            let shape = thin_vec![Some(Expr::try_from(shape_node)?)];
            Ok(Shape::MaybeUnknown(shape))
        }
        // note: this should be unreachable since we check for container_type before calling this function
        // but left as is as a safety net
        nope => {
            return Err(ASTError::new(
                shape_node.text_range().into(),
                [KwBuffer, KwTensor].as_ref(),
                nope,
            ));
        }
    }
}

impl TryFrom<&SyntaxNode> for ContainerInitializer {
    type Error = ASTError;

    fn try_from(init_node: &SyntaxNode) -> Result<Self, Self::Error> {
        let container_type = get_token_of_errs(init_node, [KwBuffer, KwTensor].as_ref())?.kind();
        let span: Range<usize> = init_node.text_range().into();
        let nodes = children_with_tokens_without_unwanted(
            init_node,
            [KwBuffer, KwTensor, LBrack, RBrack, Whitespace].as_ref(),
        );
        _ = err_if_empty(
            &nodes,
            span.clone(),
            "have nodes for value to initialize and a shape",
        )?;

        let default_node = nodes
            .first()
            .ok_or(ASTError::with_err_msg(
                span.clone(),
                "have a value to initialize".to_string(),
            ))?
            .as_node()
            .expect(" value to initialize as a node");

        let default = Expr::try_from(default_node)?;

        let shape_node = nodes
            .last()
            .ok_or(ASTError::with_err_msg(span, "have a shape".to_string()))?
            .as_node()
            .expect("a shape as a node");

        let shape = extract_shape_from(shape_node, container_type)?;

        Ok(Self {
            shape,
            value: Box::new(default),
        })
    }
}

#[cfg(test)]
mod tests {

    use smol_str::SmolStr;
    use thin_vec::thin_vec;

    use super::{BufferTree, ContainerInitializer, Expr, Literal, Shape, Value};
    use crate::{ast_root_from, cast_node_into_type, function::FnCall, variable::VarRef};

    fn literal_from(program: &str) -> Literal {
        let ast_root = ast_root_from(program);
        let container_node = ast_root.get_root().first_child().unwrap();
        cast_node_into_type::<Literal>(&container_node)
    }

    #[test]
    fn tensor_init_with_dynamic_dim_hint() {
        let program = "tensor[0.0; dynamic]";
        let literal = literal_from(program);
        assert_eq!(
            literal,
            Literal(Value::Tensor(BufferTree::Uninit(ContainerInitializer {
                value: Box::new(Expr::Literal(Literal(Value::Float(0.0)))),
                shape: Shape::MaybeUnknown(thin_vec![Some(Expr::VarRef(VarRef {
                    name: SmolStr::from("dynamic"),
                    span: 12..19,
                    type_hint: None
                }))]),
            })))
        );
    }

    #[test]
    fn tensor_init_with_dynamic_value_() {
        let program = "tensor[random_value();dynamic]";
        let literal = literal_from(program);
        assert_eq!(
            literal,
            Literal(Value::Tensor(BufferTree::Uninit(ContainerInitializer {
                value: Box::new(Expr::FnCall(FnCall {
                    name: SmolStr::from("random_value"),
                    arguments: thin_vec![],
                })),
                shape: Shape::MaybeUnknown(thin_vec![Some(Expr::VarRef(VarRef {
                    name: SmolStr::from("dynamic"),
                    span: 22..29,
                    type_hint: None
                }))]),
            })))
        );
    }

    #[test]
    fn buffer_init_with_dynamic_value() {
        let program = "buffer[weird();1000]";
        let literal = literal_from(program);
        assert_eq!(
            literal,
            Literal(Value::Buffer(BufferTree::Uninit(ContainerInitializer {
                value: Box::new(Expr::FnCall(FnCall {
                    name: SmolStr::from("weird"),
                    arguments: thin_vec![],
                })),
                shape: Shape::Known(thin_vec![1000]),
            })))
        );
    }

    #[test]
    fn tensor_init_with_dynamic_value_and_buffer_as_dims() {
        let program = "tensor[value;[3,3,3]]";
        let literal = literal_from(program);
        assert_eq!(
            literal,
            Literal(Value::Tensor(BufferTree::Uninit(ContainerInitializer {
                value: Box::new(Expr::VarRef(VarRef {
                    name: SmolStr::from("value"),
                    span: 7..12,
                    type_hint: None,
                })),
                shape: Shape::MaybeUnknown(thin_vec![
                    Some(Expr::Literal(Literal(Value::Int(3)))),
                    Some(Expr::Literal(Literal(Value::Int(3)))),
                    Some(Expr::Literal(Literal(Value::Int(3)))),
                ]),
            })))
        );
    }

    #[test]
    fn buffer_initializer_with_buffer_as_dims() {
        let program = "buffer[1.0;[1000,1000]]";
        let literal = literal_from(program);
        assert_eq!(
            literal,
            Literal(Value::Buffer(BufferTree::Uninit(ContainerInitializer {
                value: Box::new(Expr::Literal(Literal(Value::Float(1.0)))),
                shape: Shape::Known(thin_vec![1000, 1000]),
            })))
        );
    }
}
