use std::collections::{HashMap, HashSet};

use thin_vec::{ThinVec, thin_vec};

use super::{
    inference::KeyToTypeBinding,
    store::{TypeStore, TypeVarId},
    types::{Maybe, Type, free_type_vars},
};

/*
   Example for quantified variables:

       let identity = |any| any;

       at time of type inference, any will be put in quantified_vars to generalize the closure type.
       (we are able to do it iff any does not appear in anywhere else i.e. not bound)
       This is the same as
           turning fn identity(any:i32)->i32 {any}
           into fn identity<T>(any:T)->T {any}

       so that each time the polymorphic closure is called,
       the type is not bound and can restart inference for
       another type allowing polymorphism.

*/
#[derive(Debug, Clone)]
pub struct Scheme {
    pub quantified_vars: ThinVec<TypeVarId>, //  variables that are NOT of specific value, that can stand for ANY type (polymorphic)
    pub ty: Type, // type that is using the quantified variables i.e. type that is polymorphic
}

impl Scheme {
    /// Temporarily reserve a type (e.g. for recursion) even though its full form is not known yet
    pub fn monomorphic(t: &Type) -> Scheme {
        Scheme {
            quantified_vars: thin_vec![],
            ty: t.clone(),
        }
    }
}

/// compute type variables that are polymorphic
pub fn generalize(ty: &Type, env: &KeyToTypeBinding, store: &mut TypeStore) -> Scheme {
    let mut env_fv = HashSet::new();
    for t in env.values() {
        env_fv.extend(free_type_vars(&store.resolve(t)));
    }

    let ty_fv = free_type_vars(&store.resolve(ty));
    let quantified_vars = ty_fv.difference(&env_fv).copied().collect::<ThinVec<_>>();

    Scheme {
        quantified_vars,
        ty: ty.clone(),
    }
}

#[inline]
fn instantiate_type(ty: &Type, subst: &HashMap<TypeVarId, Type>) -> Type {
    match ty {
        Type::Var(v) => subst.get(v).cloned().unwrap_or(Type::Var(*v)),
        Type::SelfRef(v) => Type::SelfRef(*v),
        Type::FnSig {
            param_types,
            return_type,
        } => Type::FnSig {
            param_types: param_types
                .iter()
                .map(|pt| instantiate_type(pt, subst))
                .collect(),
            return_type: Box::new(instantiate_type(return_type, subst)),
        },
        Type::Tuple(items) => {
            Type::Tuple(items.iter().map(|t| instantiate_type(t, subst)).collect())
        }
        Type::Struct { key: name, fields } => Type::Struct {
            key: name.clone(),
            fields: fields.iter().map(|t| instantiate_type(t, subst)).collect(),
        },
        Type::Tensor { shape, data_type } => {
            let dt = data_type.as_ref().map(|dt| match dt {
                Maybe::Checked(t) => {
                    let t = instantiate_type(&*t, subst);
                    Maybe::Checked(Box::new(t))
                }
                Maybe::Unchecked(thin_vec) => Maybe::Unchecked(
                    thin_vec
                        .iter()
                        .map(|t| instantiate_type(t, subst))
                        .collect(),
                ),
            });

            Type::Tensor {
                shape: shape.clone(),
                data_type: dt,
            }
        }
        Type::Buffer { shape, data_type } => {
            let dt = match data_type {
                Maybe::Checked(t) => {
                    let t = instantiate_type(&*t, subst);
                    Maybe::Checked(Box::new(t))
                }
                Maybe::Unchecked(thin_vec) => Maybe::Unchecked(
                    thin_vec
                        .iter()
                        .map(|t| instantiate_type(t, subst))
                        .collect(),
                ),
            };

            Type::Buffer {
                shape: shape.clone(),
                data_type: dt,
            }
        }
        Type::Ref { of, is_mut } => Type::Ref {
            of: Box::new(instantiate_type(of, subst)),
            is_mut: *is_mut,
        },
        t => t.clone(),
    }
}

pub fn instantiate(scheme: &Scheme, store: &mut TypeStore) -> Type {
    let mut mapping = HashMap::new();
    for &var in &scheme.quantified_vars {
        let fresh = store.new_var_type();
        mapping.insert(var, Type::Var(fresh));
    }
    instantiate_type(&scheme.ty, &mapping)
}
