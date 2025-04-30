use std::{fmt::Debug, ops::Range, str::FromStr};

use smol_str::{SmolStr, ToSmolStr};
use syntax::{
    language::{NodeOrToken, SyntaxElement, SyntaxNode, SyntaxToken},
    syntax_kind::SyntaxKind,
};

use crate::{
    ast::ASTResult,
    container::{BufferTree, try_container_tree_from},
    errors::ASTError,
    structure::StructLiteral,
};

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Bool(bool),
    Buffer(BufferTree),
    Char(char),
    Float(f32),
    // Fn
    Int(i32),
    Str(SmolStr),
    Struct(StructLiteral),
    Tensor(BufferTree),
}

impl Value {
    pub fn get_buffer_tree(&self) -> Option<&BufferTree> {
        let tree = match self {
            Self::Buffer(tree) => tree,
            Self::Tensor(tree) => tree,
            _ => return None,
        };
        Some(tree)
    }
    pub fn is_primitive(&self) -> bool {
        matches!(
            self,
            Self::Bool(_) | Self::Char(_) | Self::Float(_) | Self::Int(_)
        )
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Literal(pub(crate) Value);

impl Literal {
    pub fn value(&self) -> Value {
        self.0.clone()
    }
}

pub fn parse_into<S: FromStr>(mut s: &str, range: Range<usize>) -> ASTResult<S>
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
        if literal_node.kind() == SyntaxKind::StructLit {
            let struct_literal = StructLiteral::try_from(literal_node)?;
            return Ok(Self(Value::Struct(struct_literal)));
        }
        if let Some(child) = literal_node.first_child() {
            if child.kind() == SyntaxKind::BufferLit {
                let buffer_tree = try_container_tree_from(&child)?;
                return Ok(Self(Value::Buffer(buffer_tree)));
            } else if child.kind() == SyntaxKind::TensorLit {
                let tensor_tree = try_container_tree_from(&child)?;
                return Ok(Self(Value::Tensor(tensor_tree)));
            }
        }
        if let Some(value_containing_token) = literal_node
            .children_with_tokens()
            .filter(|syntax_node| syntax_node.kind().is_literal_value())
            .find_map(SyntaxElement::into_token)
        {
            Literal::try_from(&value_containing_token)
        } else {
            Err(ASTError::with_err_msg(
                literal_node.text_range().into(),
                "literal node should have a value".to_string(),
            ))
        }
    }
}

impl TryFrom<&NodeOrToken> for Literal {
    type Error = ASTError;

    fn try_from(node_or_token: &NodeOrToken) -> Result<Self, Self::Error> {
        match node_or_token {
            NodeOrToken::Node(node) => Self::try_from(node),
            NodeOrToken::Token(token) => Self::try_from(token),
        }
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
        valid_buffer: "[]",
        buffer_2d_literal: "[[1,0,0],[0,1,0],[0,0,1]]",
        buffer_3d_literal: "[[[1,0,0],[0,1,0],[0,0,1]], [[1,0,0],[0,1,0],[0,0,1]], [[1,0,0],[0,1,0],[0,0,1]] ]",
        valid_tensor: "tensor[value(); [x,y,z]]",
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
