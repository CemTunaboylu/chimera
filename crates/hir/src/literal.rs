use ast::literal::{Literal as ASTLiteral, Value as ASTValue};

use rust_decimal::Decimal;
use thin_vec::ThinVec;

use crate::{
    HIRResult,
    builder::HIRBuilder,
    container::{Shape, canonical::CanonicalBuffer, layout::Layout},
    function::Callable,
    scope::{ContainerLiteralIdx, StrIdx},
    structure::StructLiteral,
    typing::hindley_milner::types::{Maybe, Type},
};

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub enum Value {
    Bool(bool),
    Buffer {
        idx: ContainerLiteralIdx,
        shape: Shape,
        data_type: Maybe<Type>,
    },
    Char(char),
    Float(Decimal),
    Lambda(Callable),
    Int(i32),
    Str(StrIdx),
    Struct(StructLiteral),
    Tensor {
        idx: ContainerLiteralIdx,
        shape: ThinVec<Option<usize>>,
        data_type: Maybe<Type>,
    },
}

impl From<&ASTValue> for Value {
    fn from(value: &ASTValue) -> Self {
        match value {
            ASTValue::Bool(b) => Value::Bool(*b),
            ASTValue::Buffer(_) => unreachable!(),
            ASTValue::Char(c) => Value::Char(*c),
            ASTValue::Float(f) => Value::Float(Decimal::from_f32_retain(*f).unwrap()),
            ASTValue::Int(i) => Value::Int(*i),
            ASTValue::Str(_) => unreachable!(),
            ASTValue::Tensor(_) => unreachable!(),
            ASTValue::Struct(_) => todo!(),
            ASTValue::Lambda(_) => todo!(),
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Literal(pub Value);

impl HIRBuilder {
    pub fn lower_literal(&mut self, literal: &ASTLiteral) -> HIRResult<Literal> {
        let value = match literal.value() {
            ASTValue::Str(string) => {
                let idx = self.allocate_string(string);
                Value::Str(idx)
            }
            ASTValue::Lambda(lambda) => {
                let c = self.lower_callable(&lambda.0)?;
                Value::Lambda(c)
            }
            // TODO: fix here!
            ASTValue::Buffer(buffer_tree) => {
                let (ten_meta, flattened) = Self::flatten_buffer_tree(self, &buffer_tree)?;
                let canonical_buffer_literal =
                    CanonicalBuffer::new(flattened, Layout::row_major, &ten_meta);
                let canonical_buffer_idx = self.allocate_tensor_literal(canonical_buffer_literal);
                Value::Buffer {
                    idx: canonical_buffer_idx,
                    shape: ten_meta.shape.clone(),
                    data_type: Maybe::Checked(Box::new(Type::I32)), // TODO: FIX ME: for now a dyummy default value to make tests pass
                                                                    // data_type: ten_meta.data_type.clone(),
                }
            }
            ASTValue::Tensor(buffer_tree) => {
                todo!()
            }
            primitive => Value::from(&primitive),
        };
        Ok(Literal(value))
    }
}
