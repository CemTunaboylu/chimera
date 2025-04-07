use crate::{
    HIRResult, builder::HIRBuilder, literal::Value, resolution::Reference, self_ref::SelfRef,
    structure::StructRef, tensor::Shape,
};
use ast::{tensor::Hint as ASTHint, types::Type as ASTType};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Type {
    Bool,
    Byte,
    Char,
    Float32,
    Integer32,
    String,
    SelfRef(SelfRef),
    Struct(Reference<StructRef>),
    Tensor {
        shape: Option<Shape>,
        datatype: Option<Box<Type>>,
    },
}

impl From<&Value> for Type {
    fn from(value: &Value) -> Self {
        match value {
            Value::Bool(_) => Self::Bool,
            Value::Char(_) => Self::Char,
            Value::Float(_) => Self::Float32,
            Value::Int(_) => Self::Integer32,
            Value::Str(_) => Self::String,
            Value::Tensor(_) => Self::Tensor {
                shape: None,
                datatype: None,
            },
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
            ASTType::Struct(_) => {
                let unresolved = self.lower_struct_ref(ast_type)?;
                let idx = self.allocate_for_resolution(unresolved);
                let unresolved_reference = Reference::Unresolved(idx);
                Type::Struct(unresolved_reference)
            }
            ASTType::Tensor(t) => {
                if t.is_empty() {
                    Type::Tensor {
                        shape: None,
                        datatype: None,
                    }
                } else {
                    let mut dims = t.iter();
                    let datatype = if let Some(ASTHint::Type(type_hint)) = t.first() {
                        _ = dims.next();
                        let datatype = self.lower_type(type_hint)?;
                        Some(Box::new(datatype))
                    } else {
                        None
                    };
                    Type::Tensor {
                        shape: Some(Shape(
                            dims.filter_map(|h| {
                                if let ASTHint::Dim(d) = h {
                                    Some(*d)
                                } else {
                                    None
                                }
                            })
                            .collect(),
                        )),
                        datatype,
                    }
                }
            }
        };
        Ok(t)
    }
}

#[cfg(test)]
mod tests {}
