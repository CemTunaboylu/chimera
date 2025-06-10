use thin_vec::{ThinVec, thin_vec};

use std::fmt::Debug;

use super::{
    inference::{TypeInferenceError, TypeInferenceResult},
    types::{Maybe, Status, Type},
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
        format!("{:} shapes do not match : {:?} vs {:?}", on_type, s1, s2).into(),
    ))
}

#[derive(Debug, Default, Clone)]
pub struct TypeStore {
    terms: ThinVec<TypeTerm>,
    parents: ThinVec<TypeVarId>,
}

impl TypeStore {
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
            Maybe::Checked(t) => Maybe::Checked(Box::new(self.resolve(t))),
            Maybe::Unchecked(ts) => {
                Maybe::Unchecked(ts.iter().map(|t| self.resolve(t)).collect::<ThinVec<_>>())
            }
            _ => Maybe::Unchecked(thin_vec![Type::Var(self.new_var_type())]),
        }
    }

    pub fn resolve(&mut self, ty: &Type) -> Type {
        match ty {
            Type::Buffer { shape, data_type } => Type::Buffer {
                shape: shape.clone(),
                data_type: self.resolve_maybe(data_type),
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
                data_type: self.resolve_maybe(data_type),
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
            (Maybe::Checked(c1), Maybe::Checked(c2)) => self.unify(c1, c2),
            (Maybe::Unchecked(dd1), Maybe::Unchecked(dd2)) => {
                for (d1, d2) in dd1.iter().zip(dd2.iter()) {
                    self.unify(d1, d2)?;
                }
                Ok(())
            }
            (Maybe::Checked(_), Maybe::Unchecked(_)) | (Maybe::Unchecked(_), Maybe::Checked(_)) => {
                Err(TypeInferenceError::UnificationFailure(
                    format!("{:} types do not match : {:?} vs {:?}", on_type, d1, d2).into(),
                ))
            }
            (Maybe::None, _) | (_, Maybe::None) => Err(TypeInferenceError::UnificationFailure(
                format!(
                    "{:?} types do not match : {:?} vs {:?}, one of the types is None",
                    on_type, d1, d2
                )
                .into(),
            )),
        }
    }
    /// Prevents forming infinite types like T = fn(T) -> Int by checking
    /// If variable `v` appears inside type `may_contain_v`.
    /// If we encounter a type variable during traversal, and it resolves to the
    /// same variable we're currently checking (`vt`), it means the type would
    /// recursively include itself — which would create an infinite type.
    /// For example, trying to unify T = (T -> Int) must fail.
    /// Direct self-unification (T = T) is fine; this only catches recursive cases
    /// because the first call to the recursion is called on a composite type.
    fn occurs_in_type(&mut self, v: TypeVarId, may_contain_v: &Type) -> bool {
        let vt = self.find(v);

        match self.resolve(may_contain_v) {
            Type::Var(i) => self.find(i) == vt,
            Type::Buffer {
                shape: _,
                data_type,
            } => match self.resolve_maybe(&data_type) {
                Maybe::None => false,
                Maybe::Checked(t) => self.occurs_in_type(vt, &t),
                Maybe::Unchecked(thin_vec) => thin_vec.iter().any(|t| self.occurs_in_type(vt, t)),
            },
            Type::FnSig { param_types, .. } => {
                param_types.iter().any(|t| self.occurs_in_type(vt, t))
            }
            Type::Ptr { of, .. } => self.occurs_in_type(v, &of),
            // FIXME: needs a deferred check because idx is on caller, cannot reach it
            Type::StructAsType(status) => match status {
                Status::Pending(_) => false,
                Status::Resolved(_) => false,
            },
            Type::Struct { fields, .. } => fields.iter().any(|t| self.occurs_in_type(vt, t)),
            Type::Tensor { data_type, .. } => match self.resolve_maybe(&data_type) {
                Maybe::None => false,
                Maybe::Checked(t) => self.occurs_in_type(vt, &t),
                Maybe::Unchecked(thin_vec) => thin_vec.iter().any(|t| self.occurs_in_type(vt, t)),
            },
            Type::Tuple(thin_vec) => thin_vec.iter().any(|t| self.occurs_in_type(vt, t)),
            _ => false,
        }
    }
    /// Attempts to make two types equal by finding a substitution.
    /// Also checks for occurence (a = a -> b is invalid)
    /// note: infinite types are by ensuring unification of a
    /// variable with a type that contains itself is prevented
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
                        if self.occurs_in_type(root, t) {
                            Err(TypeInferenceError::InfiniteType {
                                contained: root,
                                contains: t.clone(),
                            })
                        } else {
                            self.terms[root.0] = TypeTerm::Bound(t.clone());
                            Ok(())
                        }
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
                shape_validation_for(s1, s2, for_type)?;
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
                shape_validation_for(s1, s2, for_type)?;
                match (d1, d2) {
                    (Maybe::None, _) | (_, Maybe::None) => {
                        Err(TypeInferenceError::UnificationFailure(
                            format!("Tensor types do not match : {:?} vs {:?}", d1, d2).into(),
                        ))
                    }
                    (m1, m2) => self.data_type_validation_for(m1, m2, for_type),
                }
            }
            _ => Err(TypeInferenceError::UnificationFailure(
                format!("Type mismatch: {:?} vs {:?}", t1, t2).into(),
            )),
        }
    }
}
