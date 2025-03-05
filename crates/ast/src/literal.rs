use std::ops::Range;

use syntax::{
    language::{SyntaxElement, SyntaxNode, SyntaxToken},
    syntax_kind::SyntaxKind,
};

use crate::errors::ASTError;

pub type ParsedValueIndex = Range<usize>;
#[derive(Clone, Debug, PartialEq)]
pub enum Value {
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
        let index = node.text_range().into();
        let result = match node.kind() {
            StrLit => Value::Str(index),
            CharLit => Value::Char(index),
            Int => Value::Int(index),
            Float => Value::Int(index),
            kind => {
                return Err(ASTError::new(
                    index,
                    [
                        SyntaxKind::StrLit,
                        SyntaxKind::CharLit,
                        SyntaxKind::Int,
                        SyntaxKind::Float,
                    ]
                    .as_ref(),
                    kind,
                ));
            }
        };
        Ok(Self(result))
    }
}

impl TryFrom<SyntaxNode> for Literal {
    type Error = ASTError;

    fn try_from(node: SyntaxNode) -> Result<Self, Self::Error> {
        let value_containing_token = node
            .children_with_tokens()
            .filter(|syntax_node| syntax_node.kind().is_literal_value())
            .find_map(SyntaxElement::into_token)
            .unwrap();

        Literal::try_from(value_containing_token)
    }
}
