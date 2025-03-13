use std::ops::Range;

use syntax::{
    language::{SyntaxElement, SyntaxNode, SyntaxToken},
    syntax_kind::SyntaxKind,
};

use crate::errors::ASTError;

pub type ParsedValueIndex = Range<usize>;
#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Bool(bool),
    // Byte(ParsedValueIndex),
    Char(ParsedValueIndex),
    Str(ParsedValueIndex),
    Int(ParsedValueIndex),
    Float(ParsedValueIndex),
}

#[derive(Clone, Debug)]

pub struct Literal(Value);

impl Literal {
    pub fn value(&self) -> Value {
        self.0.clone()
    }
}
impl TryFrom<SyntaxToken> for Literal {
    type Error = ASTError;

    fn try_from(node: SyntaxToken) -> Result<Self, Self::Error> {
        use SyntaxKind::*;
        let result = if matches!(node.kind(), KwTrue | KwFalse) {
            Value::Bool(matches!(node.kind(), KwTrue))
        } else {
            let index = node.text_range().into();
            match node.kind() {
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
        let value_containing_token = literal_node
            .children_with_tokens()
            .filter(|syntax_node| syntax_node.kind().is_literal_value())
            .find_map(SyntaxElement::into_token)
            .unwrap();

        Literal::try_from(value_containing_token)
    }
}
#[cfg(test)]
mod tests {

    use super::*;
    use crate::ast::{
        Root,
        tests::{ast_root_from, cast_into_type},
    };
    use parameterized_test::create;
    use parser::parser::Parser;

    create! {
        create_type_test,
        (program), {
        let ast_root = ast_root_from(program);
        let literal_node = ast_root.get_root().first_child().unwrap();
        cast_into_type::<Literal>(&literal_node);
        }
    }

    create_type_test! {
        valid_bool_true: "true",
        valid_bool_false: "false",
        valid_char: "'c'",
        valid_f32: "3.14",
        valid_i32: "9",
        valid_string: "\"String\"",
        // valid_str_slice: "&\"slice\"",
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
