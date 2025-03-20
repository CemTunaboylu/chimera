use std::ops::Range;

use syntax::{
    language::{SyntaxElement, SyntaxNode, SyntaxToken},
    syntax_kind::SyntaxKind,
};

use crate::{errors::ASTError, lang_elems::ensure_node_kind_is};

pub type ParsedValueIndex = Range<usize>;
#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Bool(bool),
    // Byte(ParsedValueIndex),
    Char(ParsedValueIndex),
    Float(ParsedValueIndex),
    Int(ParsedValueIndex),
    Str(ParsedValueIndex),
    // TODO: [ ... ]
    Tensor(ParsedValueIndex),
}

#[derive(Clone, Debug, PartialEq)]

pub struct Literal(pub(crate) Value);

impl Literal {
    pub fn value(&self) -> Value {
        self.0.clone()
    }
}
impl TryFrom<&SyntaxToken> for Literal {
    type Error = ASTError;

    fn try_from(token: &SyntaxToken) -> Result<Self, Self::Error> {
        use SyntaxKind::*;
        let result = if matches!(token.kind(), KwTrue | KwFalse) {
            Value::Bool(matches!(token.kind(), KwTrue))
        } else {
            let index = token.text_range().into();
            match token.kind() {
                CharLit => Value::Char(index),
                Float => Value::Int(index),
                Int => Value::Int(index),
                StrLit => Value::Str(index),
                kind => {
                    return Err(ASTError::new(
                        index,
                        [
                            SyntaxKind::KwTrue,
                            SyntaxKind::KwFalse,
                            SyntaxKind::StrLit,
                            SyntaxKind::CharLit,
                            SyntaxKind::Int,
                            SyntaxKind::Float,
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
                let value: ParsedValueIndex = child.text_range().into();
                return Ok(Self(Value::Tensor(value)));
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
    use crate::ast::{
        Root,
        tests::{ast_root_from, cast_node_into_type},
    };
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
    }

    #[test]
    fn invalid_value() {
        let program = format!("{:}", i128::max_value());
        // will result with only Root@0..0, the overflow will raise a LexError
        // and won't be parsed by the parser but rather recorded as an error
        let root = Parser::new(&program).parse();
        assert!(!root.errors.is_empty());
        let ast_root = Root::try_from(root).expect("should have been ok");

        let literal_node = ast_root.get_root().first_child();
        assert!(literal_node.is_none());
    }
}
