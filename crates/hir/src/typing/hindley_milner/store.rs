use thin_vec::ThinVec;

use std::fmt::Debug;

use super::{
    inference::{TypeInferenceError, TypeInferenceResult},
    types::{Maybe, Type},
};

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Hash)]
pub struct TypeVarId(pub usize);

pub fn placeholder_type_var_id() -> TypeVarId {
    TypeVarId(0)
}

#[derive(Debug, Clone)]
pub enum TypeTerm {
    Unbound,
    Bound(Type),
}

fn shape_validation_for<T: Debug + PartialEq>(
    s1: &ThinVec<T>,
    s2: &ThinVec<T>,
    on_type: &str,
) -> TypeInferenceResult<()> {
    if s1 == s2 {
        return Ok(());
    }
    Err(TypeInferenceError::UnificationFailure(
        format!(
            "{:?} shapes do not match : {:?} vs {:?}",
            on_type,
            s1.as_ref(),
            s2.as_ref()
        )
        .into(),
    ))
}

#[derive(Debug, Clone)]
pub struct TypeStore {
    terms: ThinVec<TypeTerm>,
    parents: ThinVec<TypeVarId>,
}

impl TypeStore {
    pub fn new() -> Self {
        Self {
            terms: ThinVec::new(),
            parents: ThinVec::new(),
        }
    }

    pub fn new_var_type(&mut self) -> TypeVarId {
        let id = TypeVarId(self.terms.len());
        self.terms.push(TypeTerm::Unbound);
        self.parents.push(id);
        id
    }

    pub fn find(&mut self, var: TypeVarId) -> TypeVarId {
        if self.parents[var.0] != var {
            // path compression
            let root = self.find(self.parents[var.0]);
            self.parents[var.0] = root;
        }
        self.parents[var.0]
    }

    fn resolve_maybe(&mut self, m: &Maybe<Type>) -> Maybe<Type> {
        match m {
            Maybe::Checked(t) => Maybe::Checked(Box::new(self.resolve(&*t))),
            Maybe::Unchecked(ts) => {
                Maybe::Unchecked(ts.iter().map(|t| self.resolve(t)).collect::<ThinVec<_>>())
            }
        }
    }

    pub fn resolve(&mut self, ty: &Type) -> Type {
        match ty {
            Type::Buffer { shape, data_type } => Type::Buffer {
                shape: shape.clone(),
                data_type: self.resolve_maybe(&*data_type),
            },
            Type::Var(v) => {
                let root = self.find(*v);
                let term = self.terms[root.0].clone();
                match term {
                    TypeTerm::Unbound => Type::Var(root),
                    TypeTerm::Bound(t) => self.resolve(&t),
                }
            }
            Type::FnSig {
                param_types,
                return_type,
            } => Type::FnSig {
                param_types: param_types.iter().map(|pt| self.resolve(pt)).collect(),
                return_type: Box::new(self.resolve(return_type)),
            },
            Type::Tuple(items) => Type::Tuple(items.iter().map(|t| self.resolve(t)).collect()),
            Type::Struct { key, fields } => Type::Struct {
                key: *key,
                fields: fields.iter().map(|t| self.resolve(t)).collect(),
            },
            Type::Tensor { shape, data_type } => Type::Tensor {
                shape: shape.clone(),
                data_type: data_type.as_ref().map(|m| self.resolve_maybe(&*m)),
            },

            t => t.clone(),
        }
    }

    fn data_type_validation_for(
        &mut self,
        d1: &Maybe<Type>,
        d2: &Maybe<Type>,
        on_type: &str,
    ) -> TypeInferenceResult<()> {
        match (d1, d2) {
            (Maybe::Checked(c1), Maybe::Checked(c2)) => self.unify(&*c1, &*c2),
            (Maybe::Unchecked(dd1), Maybe::Unchecked(dd2)) => {
                for (d1, d2) in dd1.iter().zip(dd2.iter()) {
                    self.unify(d1, d2)?;
                }
                Ok(())
            }
            (Maybe::Checked(_), Maybe::Unchecked(_)) | (Maybe::Unchecked(_), Maybe::Checked(_)) => {
                return Err(TypeInferenceError::UnificationFailure(
                    format!("{:?} types do not match : {:?} vs {:?}", on_type, d1, d2).into(),
                ));
            }
        }
    }

    pub fn unify(&mut self, t1: &Type, t2: &Type) -> TypeInferenceResult<()> {
        match (t1, t2) {
            (Type::Var(v1), Type::Var(v2)) => {
                let r1 = self.find(*v1);
                let r2 = self.find(*v2);
                if r1 != r2 {
                    let term_1 = self.terms[r1.0].clone();
                    let term_2 = self.terms[r2.0].clone();
                    match (term_1, term_2) {
                        // TODO: check when both are Bound
                        (TypeTerm::Bound(ty_1), _) => self.unify(&ty_1, &Type::Var(r2))?,
                        (TypeTerm::Unbound, TypeTerm::Bound(ty_2)) => {
                            self.unify(&Type::Var(r1), &ty_2)?
                        }
                        (TypeTerm::Unbound, TypeTerm::Unbound) => self.parents[r1.0] = r2,
                    }
                }
                Ok(())
            }
            (Type::Var(v), t) | (t, Type::Var(v)) => {
                let root = self.find(*v);
                match &self.terms[root.0].clone() {
                    TypeTerm::Unbound => {
                        self.terms[root.0] = TypeTerm::Bound(t.clone());
                        Ok(())
                    }
                    TypeTerm::Bound(existing) => self.unify(existing, t),
                }
            }
            (Type::Bool, Type::Bool)
            | (Type::Char, Type::Char)
            | (Type::I32, Type::I32)
            | (Type::F32, Type::F32)
            | (Type::Str, Type::Str) => Ok(()),

            (
                Type::FnSig {
                    param_types: param_types_1,
                    return_type: ret_type_1,
                },
                Type::FnSig {
                    param_types: param_types_2,
                    return_type: ret_type_2,
                },
            ) => {
                for (pt1, pt2) in param_types_1.iter().zip(param_types_2.iter()) {
                    self.unify(pt1, pt2)?;
                }
                self.unify(ret_type_1, ret_type_2)
            }
            (Type::Tuple(items1), Type::Tuple(items2)) if items1.len() == items2.len() => {
                for (t1, t2) in items1.iter().zip(items2.iter()) {
                    self.unify(t1, t2)?;
                }
                Ok(())
            }
            (
                Type::Struct {
                    key: name1,
                    fields: fields1,
                },
                Type::Struct {
                    key: name2,
                    fields: fields2,
                },
            ) if name1 == name2 && fields1.len() == fields2.len() => {
                for (f1, f2) in fields1.iter().zip(fields2.iter()) {
                    self.unify(f1, f2)?;
                }
                Ok(())
            }
            (
                Type::Buffer {
                    shape: s1,
                    data_type: d1,
                },
                Type::Buffer {
                    shape: s2,
                    data_type: d2,
                },
            ) => {
                let for_type = "Buffer";
                _ = shape_validation_for(s1, s2, for_type)?;
                self.data_type_validation_for(d1, d2, for_type)
            }
            (
                Type::Tensor {
                    shape: s1,
                    data_type: d1,
                },
                Type::Tensor {
                    shape: s2,
                    data_type: d2,
                },
            ) => {
                let for_type = "Tensor";
                _ = shape_validation_for(s1, s2, for_type)?;
                if let (Some(d1), Some(d2)) = (d1, d2) {
                    self.data_type_validation_for(d1, d2, for_type)
                } else {
                    Err(TypeInferenceError::UnificationFailure(
                        format!("Tensor types do not match : {:?} vs {:?}", d1, d2).into(),
                    ))
                }
            }
            _ => Err(TypeInferenceError::UnificationFailure(
                format!("Type mismatch: {:?} vs {:?}", t1, t2).into(),
            )),
        }
    }
}
