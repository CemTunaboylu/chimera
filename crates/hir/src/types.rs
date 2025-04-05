use smol_str::SmolStr;

use crate::{HIRResult, hir::HIRBuilder, literal::Value, self_ref::SelfRef};
use ast::types::Type as ASTType;

// possible types are primitives + custom types i.e. structs
// TODO: Tensor should have dim and type hint as well
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Type {
    Bool,
    Byte,
    Char,
    Float32,
    Integer32,
    String,
    SelfRef(SelfRef),
    Struct(SmolStr),
    Tensor,
}

impl From<&Value> for Type {
    fn from(value: &Value) -> Self {
        match value {
            Value::Bool(_) => Self::Bool,
            Value::Char(_) => Self::Char,
            Value::Float(_) => Self::Float32,
            Value::Int(_) => Self::Integer32,
            Value::Str(_) => Self::String,
            Value::Tensor(_) => Self::Tensor,
        }
    }
}

impl HIRBuilder {
    pub fn lower_type(&mut self, ast_type: &ASTType) -> HIRResult<Type> {
        let t = match ast_type {
            ASTType::Bool => Type::Bool,
            ASTType::Byte => Type::Byte,
            ASTType::Char => Type::Char,
            ASTType::Float32 => Type::Float32,
            ASTType::Integer32 => Type::Integer32,
            ASTType::String => Type::String,
            ASTType::SelfRef(self_ref) => Type::SelfRef(self.lower_self_ref(self_ref)?),
            ASTType::Struct(smol_str) => Type::Struct(smol_str.clone()),
            ASTType::Tensor(_) => todo!(),
        };
        Ok(t)
    }
}

#[cfg(test)]
mod tests {}
