// Extended Hindley-Milner type inference engine with Union-Find TypeStore (extensible for traits, regions, etc.)

use core::hash::Hash;
use std::collections::HashSet;

use la_arena::Idx;
use thin_vec::{ThinVec, thin_vec};

use crate::{literal::Value, resolution::Unresolved};

use super::{inference::TypeKey, store::TypeVarId};

pub fn unit_type() -> Type {
    Type::Tuple(thin_vec![])
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Hash)]
// TODO: fix this
pub enum Status {
    Pending(Idx<Unresolved>),
    Resolved(TypeKey),
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub enum Maybe<T: Hash + Eq> {
    None,
    Checked(Box<T>),
    Unchecked(ThinVec<T>),
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Hash)]
pub enum Type {
    Bool,
    Buffer {
        shape: ThinVec<usize>,
        data_type: Maybe<Type>,
    },
    Char,
    F32,
    FnSig {
        param_types: ThinVec<Type>,
        return_type: Box<Type>, // if it does not return anything, it will return unit ()
    },
    I32,
    Ref {
        of: Box<Type>,
        is_mut: bool,
    },
    SelfRef(TypeVarId), // to be explicit, we track self and Self here
    Str,
    StructAsType(Status),
    Struct {
        key: TypeKey,
        fields: ThinVec<Type>,
    },
    Tensor {
        shape: ThinVec<Option<usize>>,
        // TODO: during lowering, it will have it's own type, Maybe should be fixed
        data_type: Maybe<Type>,
    },
    Tuple(ThinVec<Type>),
    Var(TypeVarId), // type variable
}

impl From<&Value> for Type {
    fn from(value: &Value) -> Self {
        match value {
            Value::Bool(_) => Self::Bool,
            Value::Char(_) => Self::Char,
            Value::Float(_) => Self::F32,
            Value::Int(_) => Self::I32,
            Value::Str(_) => Self::Str,
            Value::Tensor {
                idx: _,
                shape,
                data_type,
            } => Type::Tensor {
                shape: shape.clone(),
                data_type: data_type.clone(),
            },
            Value::Buffer {
                idx: _,
                shape,
                data_type,
            } => Type::Buffer {
                shape: shape.get().expect("a valid hspae for buffer value").clone(),
                data_type: data_type.clone(),
            },
        }
    }
}

// For function parameters (ByRef, ByRefMut, etc.)
pub fn type_of_param(base: Type, is_ref: bool, is_mut: bool) -> Type {
    if is_ref {
        Type::Ref {
            of: Box::new(base),
            is_mut,
        }
    } else {
        base
    }
}

// TODO: what else should I be able to free like this?
pub fn free_type_vars(ty: &Type) -> HashSet<TypeVarId> {
    match ty {
        Type::Var(v) => [*v].into_iter().collect(),
        Type::FnSig {
            param_types,
            return_type, // if it does not return anything, it will return unit ()
        } => {
            let mut s: HashSet<TypeVarId> = param_types
                .iter()
                .flat_map(|pt| free_type_vars(pt))
                .collect();
            s.extend(free_type_vars(&return_type));
            s
        }
        Type::SelfRef(v) => [*v].into_iter().collect(),
        Type::Struct { key: _, fields } => {
            fields.iter().flat_map(|pt| free_type_vars(pt)).collect()
        }
        Type::Tuple(types) => types.iter().flat_map(free_type_vars).collect(),
        _ => HashSet::new(),
    }
}
