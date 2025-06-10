use crate::{
    HIRResult,
    builder::HIRBuilder,
    self_ref::SelfRef,
    typing::hindley_milner::{
        store::placeholder_type_var_id,
        types::{Status, Type, unit_type},
    },
};
use ast::types::Type as ASTType;
use thin_vec::ThinVec;

/*
   1.	Lowering / Processing AST:
    •	While lowering an expression (e.g., a variable or function call)
            assign fresh type variables to unknowns.
   2.	Unification:
    •	When applying functions or comparing values, unify types.
            (substitution map grows)
   3.	Generalization:
    •	When binding a variable (e.g., let id = |x| x), generalize its type.
            this creates polymorphism: forall a. a -> a
   4.	Usage:
    •	When calling or referencing the variable later,
            instantiate the scheme to a fresh type again.
*/

impl HIRBuilder {
    pub fn lower_type(&mut self, ast_type: &ASTType) -> HIRResult<Type> {
        let t = match ast_type {
            ASTType::Bool => Type::Bool,
            ASTType::Char => Type::Char,
            ASTType::Float32 => Type::F32,
            ASTType::Fn {
                parameters,
                return_type,
            } => {
                let mut param_types = ThinVec::with_capacity(parameters.len());
                for ast_type in parameters {
                    let lowered = self.lower_type(ast_type)?;
                    param_types.push(lowered);
                }

                let return_type = if let Some(ret_type) = return_type {
                    self.lower_type(ret_type)?
                } else {
                    unit_type()
                };

                Type::FnSig {
                    param_types,
                    return_type: Box::new(return_type),
                }
            }
            ASTType::Integer32 => Type::I32,
            ASTType::Pointer { ty, is_mut } => {
                let lowered_pointer_type = self.lower_type(ty)?;
                Type::Ptr {
                    of: Box::new(lowered_pointer_type),
                    is_mut: *is_mut,
                }
            }
            ASTType::String => Type::Str,
            ASTType::SelfRef(self_ref) => {
                let lowered_self_ref = self.lower_self_ref(self_ref)?;
                match lowered_self_ref {
                    // TODO: this should be assigned self
                    SelfRef::Instance => Type::Var(placeholder_type_var_id()),
                    SelfRef::Struct => Type::SelfRef(placeholder_type_var_id()),
                }
            }
            ASTType::Struct(_) => {
                let unresolved = self.lower_struct_ref(ast_type)?;
                let idx = self.allocate_for_resolution(unresolved);
                Type::StructAsType(Status::Pending(idx))
            }
            ASTType::Tensor { ty, shape } => {
                // let mut dims = t.iter();
                // let data_type = if let Some(ASTHint::Type(type_hint)) = t.first() {
                //     _ = dims.next();
                //     let datatype = self.lower_type(type_hint)?;
                //     Box::new(datatype)
                // } else {
                //     return Err(HIRError::for_ast(
                //         ast_type,
                //         "type hint for data type of the tensor",
                //     )
                //     .into());
                // };
                // Type::Tensor {
                //     shape: dims
                //         .filter_map(|h| {
                //             if let ASTHint::Dim(d) = h {
                //                 Some(*d)
                //             } else {
                //                 None
                //             }
                //         })
                //         .collect(),
                //     data_type,
                // }
                todo!()
            }
            ASTType::Unit => Type::Unit,
            _ => {
                unreachable!()
            }
        };
        Ok(t)
    }
}

#[cfg(test)]
mod tests {}
