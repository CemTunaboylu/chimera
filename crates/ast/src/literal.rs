use std::{fmt::Debug, ops::Range, str::FromStr};

use smol_str::{SmolStr, ToSmolStr};
use syntax::{
    language::{SyntaxElement, SyntaxNode, SyntaxToken},
    syntax_kind::SyntaxKind,
};
use thin_vec::{ThinVec, thin_vec};

use crate::{
    ast::ASTResult,
    errors::ASTError,
    lang_elems::{get_children_in, unwrap_first_child_or_err},
};

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Bool(bool),
    Char(char),
    Float(f32),
    Int(i32),
    Str(SmolStr),
    Tensor(TensorTree),
}

impl Value {
    pub fn get_tensor_tree(&self) -> Option<&TensorTree> {
        if self.is_primitive() {
            return None;
        }
        match self {
            Self::Tensor(tree) => Some(tree),
            _ => todo!(),
        }
    }
    pub fn is_primitive(&self) -> bool {
        matches!(
            self,
            Self::Bool(_) | Self::Char(_) | Self::Float(_) | Self::Int(_)
        )
    }
}
#[derive(Clone, Debug, PartialEq)]
pub struct TensorShape(pub ThinVec<usize>);

#[derive(Clone, Debug, PartialEq)]
pub enum TensorTree {
    Node(TensorShape, ThinVec<TensorTree>),
    Leaf(ThinVec<Value>),
}

impl TensorTree {
    pub fn shape(&self) -> TensorShape {
        match self {
            TensorTree::Node(tensor_shape, _) => tensor_shape.clone(),
            TensorTree::Leaf(thin_vec) => TensorShape(thin_vec![thin_vec.len()]),
        }
    }
    pub fn sub_tree(&self) -> ThinVec<TensorTree> {
        match self {
            TensorTree::Node(_, tensor_tree) => tensor_tree.clone(),
            TensorTree::Leaf(_) => thin_vec![],
        }
    }

    pub fn values(&self) -> Option<&ThinVec<Value>> {
        match self {
            TensorTree::Node(_, _) => None,
            TensorTree::Leaf(value_tree) => Some(value_tree),
        }
    }
}

fn bottom_up_tensor_tree_from(tensor_literal_node: &SyntaxNode) -> ASTResult<TensorTree> {
    let dim_values = get_children_in(tensor_literal_node, SyntaxKind::DimValue);
    let mut values = ThinVec::new();
    let mut sub_trees = ThinVec::new();
    let mut shape = thin_vec![dim_values.len()];
    for dim_value in dim_values {
        let dim_literal = unwrap_first_child_or_err(&dim_value)?;
        match Literal::try_from(&dim_literal)?.value() {
            Value::Tensor(tensor_tree) => {
                sub_trees.push(tensor_tree);
            }
            value => {
                values.push(value);
            }
        }
    }
    if sub_trees.is_empty() {
        return Ok(TensorTree::Leaf(values));
    }
    shape.extend(sub_trees.first().unwrap().shape().0);
    Ok(TensorTree::Node(TensorShape(shape), sub_trees))
}

#[derive(Clone, Debug, PartialEq)]
pub struct Literal(pub(crate) Value);

impl Literal {
    pub fn value(&self) -> Value {
        self.0.clone()
    }
}

fn parse_into<S: FromStr>(mut s: &str, range: Range<usize>) -> ASTResult<S>
where
    <S as FromStr>::Err: Debug,
{
    for strip in ["'", "\""] {
        if s.starts_with(strip) {
            s = s.strip_prefix(strip).unwrap().strip_suffix(strip).unwrap();
        }
    }
    let result = s.parse();
    match result {
        Ok(fine) => Ok(fine),
        Err(err) => Err(ASTError::with_err_msg(
            range,
            format!("{:?} to be parseable: {:?}", s, err),
        )),
    }
}

impl TryFrom<&SyntaxToken> for Literal {
    type Error = ASTError;

    fn try_from(token: &SyntaxToken) -> Result<Self, Self::Error> {
        use SyntaxKind::*;
        let result = if matches!(token.kind(), KwTrue | KwFalse) {
            Value::Bool(matches!(token.kind(), KwTrue))
        } else {
            match token.kind() {
                CharLit => {
                    let parsed = parse_into::<char>(token.text(), token.text_range().into())?;
                    Value::Char(parsed)
                }
                Float => {
                    let parsed = parse_into::<f32>(token.text(), token.text_range().into())?;
                    Value::Float(parsed)
                }
                Int => {
                    let parsed = parse_into::<i32>(token.text(), token.text_range().into())?;
                    Value::Int(parsed)
                }
                StrLit => Value::Str(token.text().to_smolstr()),
                kind => {
                    return Err(ASTError::new(
                        token.text_range().into(),
                        [
                            SyntaxKind::CharLit,
                            SyntaxKind::Float,
                            SyntaxKind::Int,
                            SyntaxKind::KwFalse,
                            SyntaxKind::KwTrue,
                            SyntaxKind::StrLit,
                        ]
                        .as_ref(),
                        kind,
                    ));
                }
            }
        };
        Ok(Self(result))
    }
}

impl TryFrom<&SyntaxNode> for Literal {
    type Error = ASTError;

    fn try_from(literal_node: &SyntaxNode) -> Result<Self, Self::Error> {
        if let Some(child) = literal_node.first_child() {
            if child.kind() == SyntaxKind::TensorLit {
                let tensor_tree = bottom_up_tensor_tree_from(&child)?;
                return Ok(Self(Value::Tensor(tensor_tree)));
            }
        }
        let value_containing_token = literal_node
            .children_with_tokens()
            .filter(|syntax_node| syntax_node.kind().is_literal_value())
            .find_map(SyntaxElement::into_token)
            .unwrap();

        Literal::try_from(&value_containing_token)
    }
}
#[cfg(test)]
mod tests {

    use super::*;
    use crate::{ast::Root, ast_root_from, cast_node_into_type};
    use parameterized_test::create;
    use parser::parser::Parser;

    create! {
        create_literal_test,
        (program), {
        let ast_root = ast_root_from(program);
        let literal_node = ast_root.get_root().first_child().unwrap();
        cast_node_into_type::<Literal>(&literal_node);
        }
    }

    create_literal_test! {
        valid_bool_true: "true",
        valid_bool_false: "false",
        valid_char: "'c'",
        valid_f32: "3.14",
        valid_i32: "9",
        valid_string: "\"String\"",
        // valid_str_slice: "&\"slice\"",
        valid_tensor: "[]",
        tensor_2d_literal: "[[1,0,0],[0,1,0],[0,0,1]]",
        tensor_3d_literal: "[[[1,0,0],[0,1,0],[0,0,1]], [[1,0,0],[0,1,0],[0,0,1]], [[1,0,0],[0,1,0],[0,0,1]] ]",
    }

    #[test]
    fn invalid_value() {
        let program = format!("{:}", i128::max_value());
        let root = Parser::new(&program).parse();
        assert!(root.errors.is_empty());
        let ast_root = Root::try_from(root).expect("should have been ok");

        let literal_node = ast_root.get_root().first_child().unwrap();
        let literal = Literal::try_from(&literal_node);
        assert_eq!(
            r#"Err(ASTError { err_span: 0..39, expected_but_found: "\"170141183460469231731687303715884105727\" to be parseable: ParseIntError { kind: PosOverflow }" })"#,
            format!("{:?}", literal)
        )
    }
}
