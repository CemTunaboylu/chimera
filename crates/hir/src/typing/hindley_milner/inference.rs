use std::{collections::{HashMap, HashSet}, str::FromStr};

use crate::{
    metadata::VarMeta,
    operation::{BinaryOp, UnaryOp},
    scope::StrIdx,
};

use super::{
    expression::{Conditional, HMExpr}, scheme::{generalize, instantiate, Scheme}, statement::HMStmt, store::{TypeStore, TypeVarId}, types::{unit_type, Maybe, Type}
};

use la_arena::{Arena, Idx};
use miette::Diagnostic;
use smol_str::SmolStr;
use thin_vec::ThinVec;
use thiserror::Error;

#[derive(Clone, Diagnostic, Debug, PartialEq, Error)]
#[diagnostic()]
#[error("TypeInferenceError")]
// Source and span will be injected at the end by HIR
pub enum TypeInferenceError {
    ArityMismatch(#[help] SmolStr),
    AttemptedShadowing(#[help] SmolStr),
    // "Cannot access field or method `{}` on non-struct type: {:?}",
    CannotAccessFieldOrMethod {
        non_struct: Type,
        attr_key: TypeKey,
    },
    DerefNonRef(#[help] SmolStr),
    Expected(#[help] SmolStr),
    //  "Infinite type detected: variable T occurs inside (T -> Int)"
    InfiniteType{
        contained: TypeVarId,
        contains: Type,
    },
    ResolutionFailure(#[help] SmolStr),
    UnificationFailure(#[help] SmolStr),
    // "Unbound variable: {:?}"
    Unbound(TypeKey),
    // "Field or method `{}` not found in struct `{}`",
    UnknownFieldOrMethod {
        struct_key: TypeKey,
        attr_key: TypeKey,
    },
}
// TODO: method to form the help message form TypeInferenceError

pub type TypeInferenceResult<I> = Result<I, TypeInferenceError>;

pub type TypeKey = StrIdx;
pub type TypeIdx = Idx<Type>;

pub type DoubleKeyedTypeMap = HashMap<(TypeKey, TypeKey), Type>;
pub type KeyToTypeBinding = HashMap<TypeKey, Type>;

#[derive(Debug, Default)]
pub struct TypeContext {
    pub self_stack: ThinVec<TypeVarId>,

    // note: when a StructAsType is declared as a type, it is just an identifier.
    // after we resolve that, we will end up with a StrIdx, which is the position
    // in the arena of the name of the struct. Thus when type checking, we need to 
    // be able to reach the Type with the StrIdx. 
    pub key_to_id: HashMap<TypeKey, TypeIdx>,
    pub types: Arena<Type>,

    pub struct_field_types: DoubleKeyedTypeMap, // (StructName, field_name) → field type
    pub struct_method_types: DoubleKeyedTypeMap, // (StructName, method_name) → fn type

    pub generalized_types: HashMap<TypeKey, Scheme>,
    pub bindings: KeyToTypeBinding,
}
impl TypeContext {
    pub fn insert_type_with_key(&mut self, key: TypeKey, ty: Type ) -> TypeInferenceResult<()> {
        if let Some(idx) = self.key_to_id.get(&key) {
            let already_in = &self.types[*idx];
            Err(TypeInferenceError::AttemptedShadowing(format!("There is already a declared type '{:?}' for '{:?}'", already_in, idx).into()))
        } else {
            let idx = self.types.alloc(ty);
            self.key_to_id.insert(key, idx);
            Ok(())
        }
    }

    pub fn get_type_with_key(&mut self, key: &TypeKey) -> TypeInferenceResult<Type>{
        if let Some(idx) = self.key_to_id.get(key) {
            let t = &self.types[*idx];
            Ok(t.clone())
        } else {
            Err(TypeInferenceError::ResolutionFailure(format!("There is no declared type with key '{:?}'", key).into()))
        }
    }

    pub fn current_self(&self) -> Option<TypeVarId> {
        self.self_stack.last().copied()
    }
    pub fn find_field_on_struct(
        &self,
        struct_key: TypeKey,
        attr_key: TypeKey,
    ) -> TypeInferenceResult<Type> {
        self.struct_field_types
            .get(&(struct_key, attr_key))
            .cloned()
            .ok_or_else(|| TypeInferenceError::UnknownFieldOrMethod {
                struct_key,
                attr_key,
            })
    }
    pub fn find_on_struct(
        &self,
        struct_key: TypeKey,
        attr_key: TypeKey,
    ) -> TypeInferenceResult<Type> {
        self.struct_field_types
            .get(&(struct_key, attr_key))
            .or_else(|| {
                self.struct_method_types
                    .get(&(struct_key,attr_key))
            })
            .cloned()
            .ok_or_else(|| TypeInferenceError::UnknownFieldOrMethod {
                struct_key,
                attr_key,
            })
    }
}

pub fn mark_variable_mut(meta: &mut VarMeta) {
    meta.is_mut = true;
    if meta.first_write_idx.is_none() {
        meta.first_write_idx = Some(0); // placeholder — real usage index would be filled in by analyzer
    }
}

fn infer_expr_base(
    expr: &HMExpr,
    _ctx: &mut TypeContext,
    _store: &mut TypeStore,
) -> TypeInferenceResult<Type> {
    match expr {
        HMExpr::Bool => Ok(Type::Bool),
        HMExpr::Char => Ok(Type::Char),
        HMExpr::F32 => Ok(Type::F32),
        HMExpr::I32 => Ok(Type::I32),
        HMExpr::Str => Ok(Type::Str),
        // _ => unimplemented!(),
        t => {
            println!("got {:?}", t);
            unimplemented!()
        },
    }
}

pub fn infer_bin_expr(
    op: &BinaryOp,
    lhs: &HMExpr,
    rhs: &HMExpr,
    ctx: &mut TypeContext,
    store: &mut TypeStore,
) -> TypeInferenceResult<Type> {
    if *op == BinaryOp::Dot {
        let base_type = infer_expr(lhs, ctx, store)?;
        let attr_key = if let HMExpr::Var(f) = *rhs {
            f
        } else {
            return Err(TypeInferenceError::Expected(
                "Expected field or method name on RHS of dot expression".into(),
            ));
        };
        let struct_key = match store.resolve(&base_type) {
            Type::Struct { key, fields: _ } => key,
            Type::SelfRef(id) => {
                // resolve SelfRef(id) to Struct from stack metadata
                let self_ty = store.resolve(&Type::Var(id));
                if let Type::Struct { key, fields: _ } = self_ty {
                    key
                } else {
                    return Err(TypeInferenceError::ResolutionFailure(
                        "`Self` did not resolve to a struct type".into(),
                    ));
                }
            }
            non_struct => {
                return Err(TypeInferenceError::CannotAccessFieldOrMethod {
                    non_struct,
                    attr_key,
                });
            }
        };
        ctx.find_on_struct(struct_key, attr_key)
    } else {
        let lhs_type = infer_expr(lhs, ctx, store)?;
        let rhs_type = infer_expr(rhs, ctx, store)?;

        // must be able to unify to the same type
        store.unify(&lhs_type, &rhs_type)?;
        if op.is_bool_op() {
            // bool ops expect bools as operands
            store.unify(&lhs_type, &Type::Bool)?;
        } else if op.is_bit_op() {
            // bit ops expect i32 as operands
            store.unify(&lhs_type, &Type::I32)?;
        }
        // comparison results with a bool type
        let t = if op.is_comparison() {
            Type::Bool
        } else {
            store.resolve(&lhs_type)
        };
        Ok(t)
    }
}
fn infer_unary_expr(
    op: &UnaryOp,
    operand: &HMExpr,
    ctx: &mut TypeContext,
    store: &mut TypeStore,
) -> TypeInferenceResult<Type> {
    let is_mut = matches!(*operand, HMExpr::Mut(_));
    // note: a mutable expression has its type reduced to its inner type
    let operand_type = infer_expr(operand, ctx, store)?;
    let result_ty = match op {
        UnaryOp::Ref => Type::Ref {
            of: Box::new(operand_type),
            is_mut,
        },
        UnaryOp::Deref => {
            let result_ty = Type::Var(store.new_var_type());
            store.unify(
                &operand_type,
                &Type::Ref {
                    of: Box::new(result_ty.clone()),
                    is_mut: false, // Note: conservative for now, only can deref if immutable reference  
                },
            ).or(Err(
                TypeInferenceError::DerefNonRef(
                    format!("cannot dereference non-reference type '{:?}'", operand_type).into()
                )
            ))?;
            result_ty
        }
        UnaryOp::Neg => {
            // negative expects either f32 or i32
            store
                .unify(&operand_type, &Type::I32)
                .or_else(|_| 
                    store.unify(&operand_type, &Type::F32)
                    .or(Err(
                        TypeInferenceError::UnificationFailure( 
                            format!("Negation expects either '{:?}' or '{:?}', but got '{:?}' which cannot be unified with expected types", Type::F32, Type::I32, operand_type).into()
                            ))
                        ))?;
            operand_type
        }
        UnaryOp::Not => {
            // not expects bool 
            store.unify(&operand_type, &Type::Bool)?;
            operand_type
        },
        UnaryOp::CondUnwrap => todo!(),
    };
    Ok(store.resolve(&result_ty))
}

/// assumes it is in the right context
pub fn infer_expr(
    expr: &HMExpr,
    ctx: &mut TypeContext,
    store: &mut TypeStore,
) -> TypeInferenceResult<Type> {
    match expr {
        HMExpr::BinaryOp { op, lhs, rhs } => infer_bin_expr(op, lhs, rhs, ctx, store),
        HMExpr::Block{ returns, statements } => {
            let unit_type = unit_type();
            let mut return_type = unit_type.clone();
            let mut returning_indices : HashSet::<usize> = HashSet::from_iter(returns.iter().copied());
            
            for (ix, stmt) in statements.iter().enumerate() {
                let inferred =  infer_stmt(stmt, ctx, store)?;
                if !returning_indices.remove(&ix) {
                    continue;
                } 
                if return_type == unit_type {
                    return_type = inferred; 
                } else {
                    store.unify(&inferred, &return_type)?;
                }
            }

            Ok(return_type)
        },
        HMExpr::Buffer{ data_type, shape } => {
            let inferred_type = match  data_type {
                    Maybe::Checked(t) => &**t,
                    Maybe::Unchecked(unresolved_types) => {
                        if unresolved_types.is_empty() {
                            &Type::Var(store.new_var_type())
                        } else {
                            let t = unresolved_types.first().unwrap();
                            for u in &unresolved_types[1..] {
                                store.unify(t, u)?;
                            }
                            &store.resolve(t)
                        }
                    },
                    Maybe::None => return Err(TypeInferenceError::Expected(SmolStr::from_str("buffers must have their type declared").unwrap())),
            };
            Ok(Type::Buffer{ shape: shape.clone(), data_type: Maybe::Checked(Box::new(inferred_type.clone())) })
        }
        HMExpr::FnCall { fn_to_call, args } => {
            let t_fun = infer_stmt(fn_to_call, ctx, store)?;
            let mut arg_types = ThinVec::<Type>::with_capacity(args.len());
            for arg in args {
                let t_arg = infer_stmt(arg, ctx, store)?;
                arg_types.push(t_arg);
            }
            let t_ret = Type::Var(store.new_var_type());
            store.unify(
                &t_fun,
                &Type::FnSig {
                    param_types: arg_types,
                    return_type: Box::new(t_ret.clone()),
                },
            )?;
            Ok(store.resolve(&t_ret))
        }
        HMExpr::Mut(inner) => {
            infer_expr(inner, ctx, store)
        }
        // note: before the type checking, HIR elements that are not unresolved 
        // will be resolved first, thus it is assumed to be of Status::Resolved(_) type
        HMExpr::StructAsType(key) => {
            let ty = ctx.get_type_with_key(key)?;
            Ok(ty)
        }
        // TODO: remove this
        HMExpr::StructInit { key, fields } => {
            let struct_def = ctx.get_type_with_key(key)?;
            let mut struct_type_fields = ThinVec::with_capacity(fields.len());
            for (f_key, f_expr) in fields {
                let expected = ctx.find_field_on_struct(*key, *f_key)?;
                let inferred = infer_expr(f_expr, ctx, store)?;
                store.unify(&expected, &inferred)?;
                struct_type_fields.push(inferred);
            }
            let ty = Type::Struct {
                key: *key,
                fields: struct_type_fields,
            };
            // ensure that such a struct is defined 
            store.unify(&struct_def, &ty)?;
            Ok(ty)
        }
        HMExpr::SelfRef => {
            if let Some(id) = ctx.current_self() {
                Ok(Type::SelfRef(id))
            } else {
                Err(TypeInferenceError::ResolutionFailure(
                    "`Self` used outside of an impl or struct scope".into(),
                ))
            }
        }
        HMExpr::Tensor { data_type, shape } => {
            let inferred_type = match  data_type {
                    Maybe::Checked(t) => &**t,
                    Maybe::Unchecked(unresolved_types) => {
                        if unresolved_types.is_empty() {
                            &Type::Var(store.new_var_type())
                        } else {
                            let t = unresolved_types.first().unwrap();
                            for u in &unresolved_types[1..] {
                                store.unify(t, u)?;
                            }
                            &store.resolve(t)
                        }
                    },
                    Maybe::None => &Type::Var(store.new_var_type()),
            };
            Ok(Type::Tensor{ shape: shape.clone(), data_type: Maybe::Checked(Box::new(inferred_type.clone())) })
        }
        HMExpr::Tuple(exprs) => {
            let mut inferred_types = ThinVec::with_capacity(exprs.len());
            for e in exprs {
                let i = infer_expr(e, ctx, store)?;
                inferred_types.push(i);
            }
            Ok(Type::Tuple(inferred_types))
        }
        HMExpr::Var(name) => {
            if let Some(scheme) = ctx.generalized_types.get(name) {
                Ok(instantiate(scheme, store))
            } else if let Some(ty) = ctx.bindings.get(name) {
                Ok(store.resolve(ty))
            } else {
                Err(TypeInferenceError::Unbound(*name))
            }
        }
        HMExpr::Unary(op, operand) => {
            infer_unary_expr(op, operand, ctx, store)
        }
        base => infer_expr_base(base, ctx, store),
    }
}

/// assumes it is in the right context
pub fn infer_stmt(
    stmt: &HMStmt,
    ctx: &mut TypeContext,
    store: &mut TypeStore,
) -> TypeInferenceResult<Type> {
    match stmt {
        HMStmt::Expr(expr) => {
                        let inferred = infer_expr(expr, ctx, store)?;
                        Ok(inferred)
            }
        HMStmt::ControlFlow {
                branches: branches_with_condition_and_blocks,
                else_block,
            } => {
            let mut blk_ret_type = None::<Type>;

            for Conditional { condition, body } in branches_with_condition_and_blocks {
                let inferred_cond_type = infer_expr(condition, ctx, store)?;
                store.unify(&inferred_cond_type, &Type::Bool)?;

                let inferred_ret_type = infer_expr(body, ctx, store)?;
                if let Some(ret_type_of_blk) = &blk_ret_type {
                    store.unify(&inferred_ret_type, ret_type_of_blk)?;
                } else {
                    blk_ret_type = Some(inferred_ret_type);
                }
            }
            // at this point since even a non-returning block assigns to block_type (with unit type ()),
            // it should not be None anymore, thus we should unwrap it
            let blk_ret_type = blk_ret_type.expect("a return type for conditional block");

            let inferred_ret_ty_for_else = if let Some(else_blk) = else_block {
                infer_expr(else_blk, ctx, store)?
            } else {
                unit_type()
            };
            store.unify(&inferred_ret_ty_for_else, &blk_ret_type)?;

            Ok(blk_ret_type)
        }
        HMStmt::FnDef {
                key,
                parameters,
                body,
                return_type,
            } => {
            let param_types = parameters
                .iter()
                .map(|(p_name, ty)| {
                    ctx.bindings.insert(*p_name, ty.clone());
                    ty.clone()
                })
                .collect::<ThinVec<_>>();

            // allow recursion
            let fresh = Type::Var(store.new_var_type());
            ctx.generalized_types
                .insert(*key, Scheme::monomorphic(&fresh));

            // then infer the body, unify the result with `fresh`
            if let Some(ret_ty) = return_type {
                let body_type = infer_stmt(body, ctx, store)?;
                store.unify(ret_ty, &body_type)?;
            }
            let fn_sig_type = Type::FnSig {
                param_types,
                return_type: Box::new(return_type.clone().unwrap_or(unit_type())),
            };
            let scheme = generalize(&fn_sig_type, &ctx.bindings, store);
            ctx.generalized_types.insert(*key, scheme);
            parameters.iter().for_each(|(p_name, _)| {
                ctx.bindings.remove(p_name);
            });
            // unify the fresh with the function signature, i.e. allowing recursion of this function
            store.unify(&fresh, &fn_sig_type)?;

            // TODO: this may not be necessary
            Ok(fn_sig_type)
        }
        HMStmt::Lambda { params, body } => {
            let param_types = params
                .iter()
                .map(|param_name| {
                    let param_var_id = store.new_var_type();
                    let param_type_as_var = Type::Var(param_var_id);
                    ctx.bindings
                        .insert(*param_name, param_type_as_var.clone());
                    param_type_as_var
                })
                .collect::<ThinVec<_>>();
            let ret_ty = infer_stmt(body, ctx, store)?;
            params.iter().for_each(|param_name| {
                ctx.bindings.remove(param_name);
            });
            Ok(Type::FnSig {
                param_types,
                return_type: Box::new(ret_ty),
            })
        }
        HMStmt::Let {
                key: name,
                val,
                body,
            } => {
            if let HMStmt::Lambda { params: _, body: _ } = &**val {
                // pre-binding to support recursion
                let fresh_type = Type::Var(store.new_var_type());
                ctx.bindings.insert(*name, fresh_type.clone());
            }

            let val_type = infer_stmt(val, ctx, store)?;

            // temporarily remove before generalization over itself
            let backup = ctx.bindings.remove(name);

            let scheme = generalize(&val_type, &ctx.bindings, store);

            ctx.generalized_types.insert(*name, scheme);

            if let Some(fresh_type) = &backup {
                ctx.bindings.insert(*name, fresh_type.clone());
            }

            let result_type = infer_stmt(body, ctx, store)?;

            if let Some(fresh_type) = &backup {
                let fresh_type_resolved = match store.resolve(fresh_type) {
                    Type::FnSig {
                        param_types: _,
                        return_type,
                    } => *return_type,
                    rest => rest,
                };
                store.unify(&fresh_type_resolved, &result_type)?;
            }

            ctx.bindings.remove(name);

            Ok(result_type)
        }
        HMStmt::StructDef { key , fields } => {
            // Also enables ordered without the keys init
            let struct_type_fields = fields
                .iter()
                .map(|(field_name, field_type)| {
                    ctx.struct_field_types
                        .insert((*key, *field_name), field_type.clone());
                    field_type.clone()
                })
                .collect::<ThinVec<_>>();

            let t = Type::Struct {
                key: *key ,
                fields: struct_type_fields,
            };
            ctx.insert_type_with_key(*key, t.clone())?;
            Ok(t)
        }
        HMStmt::Jump(hmexpr) => todo!(),
        HMStmt::Loop(_) => todo!(),
        }
}

#[cfg(test)]
mod tests {

    use thin_vec::thin_vec;
    use crate::scope::into_idx;

    use super::super::types::Maybe;

    use super::*;
    use parameterized_test::create;

    fn get_idx_for(s: &str) -> StrIdx {
        match s {
            "id" => into_idx(0),
            "x" => into_idx(1),
            "y" => into_idx(2),
            "Point" => into_idx(3),
            "is_walkable" => into_idx(4),
            "add" => into_idx(5),
            "result" => into_idx(6),
            "fact" => into_idx(7),
            "even" => into_idx(8),
            "odd" => into_idx(9),
            "n" => into_idx(10),
            "p" => into_idx(11),
            "loopback" => into_idx(12),
            "foo" => into_idx(13),
            "self_apply" => into_idx(13),
            "add1" => into_idx(14),
            "f" => into_idx(15),
            _ => into_idx(16),
        }
    }

    fn mutual_let_bindings_expr() -> HMExpr {
        // placeholder inference-only logic, not true evaluation
        let even_def = HMStmt::Lambda {
            params: thin_vec![get_idx_for("even")],
            body: Box::new(HMStmt::Lambda {
                params: thin_vec![get_idx_for("odd")],
                body: Box::new(HMStmt::Lambda {
                    params: thin_vec![get_idx_for("n")],
                    // simplified placeholder
                    body: Box::new(HMStmt::Expr(HMExpr::Var(get_idx_for("n")))), 
                }),
            }),
        };

        let odd_def = HMStmt::Lambda {
            params: thin_vec![get_idx_for("odd")],
            body: Box::new(HMStmt::Lambda {
                params: thin_vec![get_idx_for("even")],
                body: Box::new(HMStmt::Lambda {
                    params: thin_vec![get_idx_for("n")],
                    // simplified placeholder
                    body: Box::new(HMStmt::Expr(HMExpr::Var(get_idx_for("n")))), 
                }),
            }),
        };

        HMExpr::FnCall {
            fn_to_call: Box::new(HMStmt::Expr(HMExpr::FnCall {
                fn_to_call: Box::new(HMStmt::Expr(HMExpr::FnCall {
                    fn_to_call: Box::new(even_def.clone()),
                    args: thin_vec![even_def],
                })),
                args: thin_vec![HMStmt::Expr(HMExpr::FnCall {
                    fn_to_call: Box::new(odd_def.clone()),
                    args: thin_vec![odd_def],
                })],
            })),
            args: thin_vec![HMStmt::Expr(HMExpr::I32)],
        }
    }

    fn recursive_function_expr() -> HMExpr {
        let fact_def = HMStmt::Lambda {
            params: thin_vec![get_idx_for("fact")],
            body: Box::new(HMStmt::Lambda {
                params: thin_vec![get_idx_for("n")],
                body: Box::new(HMStmt::Expr(HMExpr::Var(get_idx_for("n")))), // placeholder body; inference only
            }),
        };

        HMExpr::FnCall {
            fn_to_call: Box::new(HMStmt::Expr(HMExpr::FnCall {
                fn_to_call: Box::new(fact_def.clone()),
                args: thin_vec![fact_def],
            })),
            args: thin_vec![HMStmt::Expr(HMExpr::I32)],
        }
    }

    fn real_world_addition_pipeline_expr() -> HMStmt{
        // simulated structure without actual operator support
        let add = HMStmt::Lambda {
            params: thin_vec![get_idx_for("x")],
            body: Box::new(HMStmt::Lambda {
                params: thin_vec![get_idx_for("y")],
                body: Box::new(
                    // placeholder for x + y
                    HMStmt::Expr(HMExpr::Var(get_idx_for("x")) )
                ),
            }),
        };

        HMStmt::Let {
            key: get_idx_for("add"),
            val: Box::new(add),
            body: Box::new(HMStmt::Let {
                key: get_idx_for("result"),
                val: Box::new(HMStmt::Expr(HMExpr::FnCall {
                    fn_to_call: Box::new(HMStmt::Expr(HMExpr::FnCall {
                        fn_to_call: Box::new(HMStmt::Expr(HMExpr::Var(get_idx_for("add")))),
                        args: thin_vec![HMStmt::Expr(HMExpr::I32)],
                    })),
                    args: thin_vec![HMStmt::Expr(HMExpr::I32)],
                })),
                body: Box::new(HMStmt::Expr(HMExpr::Var(get_idx_for("result")))),
            }),
        }
    }

    /* Note: In Hindley-Milner type system, a function with multiple arguments is actually consists of
         a function chain of which each function takes one argument

        fn add<T:Add>(a:T,b:T)->T {...}

        is expressed as

        Type::Fn(a, Type::Fn(b, <Type of the Body))
    */
    fn add_like_operator_expr() -> HMExpr {
        let add_fn = HMStmt::Lambda {
            params: thin_vec![get_idx_for("x")],
            body: Box::new(HMStmt::Lambda {
                params: thin_vec![get_idx_for("y")],
                body: Box::new(
                    // simulate x + y as just x for typing
                    HMStmt::Expr(HMExpr::Var(get_idx_for("y"))), 
                ),
            }),
        };

        HMExpr::FnCall {
            fn_to_call: Box::new(HMStmt::Expr(HMExpr::FnCall {
                fn_to_call: Box::new(add_fn),
                args: thin_vec![HMStmt::Expr(HMExpr::I32)],
            })),
            args: thin_vec![HMStmt::Expr(HMExpr::I32)],
        }
    }

    fn point_struct_def() -> HMStmt{
        HMStmt::StructDef {
            key: get_idx_for("Point"),
            fields: thin_vec![
                (get_idx_for("x"), Type::I32),
                (get_idx_for("y"), Type::I32),
                (get_idx_for("is_walkable"), Type::Bool),
            ],
        }
    }

    create! {
        hindley_miller_test,
        (stmt, expected_inferred_type), {
            let mut store = TypeStore::default();
            let mut ctx = TypeContext::default();

            // to make the tests for StructInits work, we define the Struct that they will
            // initialize by inferring its type
            let _ = infer_stmt(&point_struct_def(), &mut ctx,  &mut store).expect("struct def inferred");

            let inferred_type = infer_stmt(stmt, &mut ctx,  &mut store).expect("inferred type");
            assert_eq!(expected_inferred_type, store.resolve(&inferred_type));
        }
    }

    hindley_miller_test! {
        // Chimera equivalent: true
        bool_literal: (&HMStmt::Expr(HMExpr::Bool), Type::Bool),
        /*
            Chimera equivalent:
            let id = |x| x;
            id(42)
        */
        identity_function:
            (&HMStmt::Let{
                key: get_idx_for("id"),
                val: Box::new(HMStmt::Lambda{
                    params: thin_vec![get_idx_for("x")],
                    body: Box::new(HMStmt::Expr(HMExpr::Var(get_idx_for("x")))),
                }),
                body:Box::new(HMStmt::Expr(HMExpr::FnCall{
                    fn_to_call: Box::new(HMStmt::Expr(HMExpr::Var(get_idx_for("id")))), // f
                    args: thin_vec![HMStmt::Expr(HMExpr::I32)],
                })),
            },
            Type::I32),
        /*
            Chimera equivalent:
            let id = |x| x;
            let (integer32, boolean) = (id(42), id(true));
        */
        polymorphic_id: (
           &HMStmt::Let{
                key: get_idx_for("id"),
                val: Box::new(HMStmt::Lambda{
                    params: thin_vec![get_idx_for("x")],
                    body: Box::new(HMStmt::Expr(HMExpr::Var(get_idx_for("x")))),
                }),
                body: Box::new(HMStmt::Expr(HMExpr::Tuple(thin_vec![
                    HMExpr::FnCall{
                        fn_to_call: Box::new(HMStmt::Expr(HMExpr::Var(get_idx_for("id")))),
                        args: thin_vec![HMStmt::Expr(HMExpr::I32)],
                    },
                    HMExpr::FnCall{
                        fn_to_call:Box::new(HMStmt::Expr(HMExpr::Var(get_idx_for("id")))),
                        args: thin_vec![HMStmt::Expr(HMExpr::Bool)],
                    },
                ]))),
            },
            Type::Tuple(thin_vec![Type::I32, Type::Bool])
        ),
        /*
            Chimera equivalent:
            let integer32 = (|x| x)(7);
        */
        lambda_and_its_application: (
            &HMStmt::Expr(HMExpr::FnCall{
                fn_to_call: Box::new(
                    HMStmt::Lambda{
                        params: thin_vec![get_idx_for("x")],
                        body: Box::new(HMStmt::Expr(HMExpr::Var(get_idx_for("x")))),
                        }),
                args: thin_vec![HMStmt::Expr(HMExpr::I32)]}),
            Type::I32
        ),
        /*
            Chimera equivalent:
            (1, false)
        */
        tuple_exprs: (
            &HMStmt::Expr(HMExpr::Tuple(thin_vec![HMExpr::I32, HMExpr::Bool])),
            Type::Tuple(thin_vec![Type::I32, Type::Bool])
        ),
        /*
            Chimera equivalent:
            Point{x:0, y:0, is_passable:true}
        */
        struct_init: (
            &HMStmt::Expr(HMExpr::StructInit{
                key: get_idx_for("Point"),
                fields: thin_vec![
                    (get_idx_for("x"), HMExpr::I32),
                    (get_idx_for("y"), HMExpr::I32),
                    (get_idx_for("is_walkable"),HMExpr::Bool),
                    ]}),
            Type::Struct{
                key: get_idx_for("Point"),
                fields: thin_vec![Type::I32, Type::I32, Type::Bool]
               }
        ),
        /*
            Chimera equivalent:
            [1,2,3]
        */
        buffer_literal: (
            &HMStmt::Expr(HMExpr::Buffer{data_type: Maybe::Checked(Box::new(Type::I32)), shape: thin_vec![3]}),
            Type::Buffer{
                shape: thin_vec![3],
                data_type: Maybe::Checked(Box::new(Type::I32)),
            }
        ),
        /*
            Chimera equivalent:
                let x = 1;
                let y = true;
                (x,y)
        */
        multiple_let_bindings: (
            &HMStmt::Let{
                key: get_idx_for("x"),
                val: Box::new(HMStmt::Expr(HMExpr::I32)),
                body: Box::new(HMStmt::Let{
                    key: get_idx_for("y"),
                    val: Box::new(HMStmt::Expr(HMExpr::Bool)),
                    body: Box::new(HMStmt::Expr(HMExpr::Tuple(thin_vec![
                        HMExpr::Var(get_idx_for("x")),
                        HMExpr::Var(get_idx_for("y")),
                    ]))),
                }),
            },
            Type::Tuple(thin_vec![Type::I32, Type::Bool])
        ),
        /*
            Chimera equivalent:
               let p = Point{x:1, y:2};
               p
        */
        let_with_struct_init: (
            &HMStmt::Let{
                key: get_idx_for("p"),
                val: Box::new(HMStmt::Expr(HMExpr::StructInit{
                    key: get_idx_for("Point"),
                    fields: thin_vec![
                        (get_idx_for("x"), HMExpr::I32),
                        (get_idx_for("y"), HMExpr::I32),
                        (get_idx_for("is_walkable"),HMExpr::Bool),
                        ],
                })),
                body: Box::new(HMStmt::Expr(HMExpr::Var(get_idx_for("p")))),
            },
            Type::Struct{
                key: get_idx_for("Point"),
                fields: thin_vec![Type::I32, Type::I32, Type::Bool]
                },
        ),
        /*
            Chimera equivalent:
               let p = Point{x:1, y:2};
               p.x
        */
        let_with_struct_init_and_dot: (
            &HMStmt::Let{
                key: get_idx_for("p"),
                val: Box::new(HMStmt::Expr(HMExpr::StructInit{
                    key: get_idx_for("Point"),
                    fields: thin_vec![
                        (get_idx_for("x"), HMExpr::I32),
                        (get_idx_for("y"), HMExpr::I32),
                        (get_idx_for("is_walkable"),HMExpr::Bool),
                        ],
                })),
                body: Box::new(
                    HMStmt::Expr(
                        HMExpr::BinaryOp{
                            lhs : Box::new(HMExpr::Var(get_idx_for("p"))), 
                            op: BinaryOp::Dot, 
                            rhs : Box::new(HMExpr::Var(get_idx_for("x"))),
                            }),
                    )
            },
            Type::I32,
        ),
        /*
            Chimera equivalent:
               p.x      // where we know that p is of type Point
        */
        struct_as_type_and_dot: (
            &HMStmt::Expr(HMExpr::BinaryOp{
                lhs : Box::new(HMExpr::StructAsType(get_idx_for("Point"))), 
                op: BinaryOp::Dot, 
                rhs : Box::new(HMExpr::Var(get_idx_for("x"))),
            }),
            Type::I32,
        ),
        /*
            Chimera equivalent:
                let dummy_buffer = [x, [1,2,3]];
                dummy_tensor
        */
        let_with_buffer_init:  (
            &HMStmt::Let{
                key: get_idx_for("dummy_buffer"),
                val: Box::new(HMStmt::Expr(HMExpr::Buffer{
                    data_type: Maybe::Checked(Box::new(Type::I32)),
                    shape: thin_vec![1,2,3],
                    })
                ),
                body: Box::new(HMStmt::Expr(HMExpr::Var(get_idx_for("dummy_tensor")))),
            },
            Type::Buffer{
                shape: thin_vec![1,2,3],
                data_type: Maybe::Checked(Box::new(Type::I32)), 
            }
        ),

        /*
            Chimera equivalent:
                let fact = |fact| {
                                |n| {
                                    if n == 0 {
                                        1
                                    } else {
                                        n * fact(fact)(n - 1)
                                    }
                                }
        };
        This simulates recursion using self-application
        */
        recursive_function: (
            &HMStmt::Expr(recursive_function_expr()),
            Type::I32,
        ),
        /*
            Chimera equivalent:
            let even = |even| {
                |odd| {
                    |n| if n == 0 { true } else { odd(odd)(n - 1) }
                }
            };

            let odd = |odd| {
                |even| {
                    |n| if n == 0 { false } else { even(even)(n - 1) }
                }
            };

            even(even)(odd)(4)
        */
        mutual_let_bindings: (
            &HMStmt::Expr(mutual_let_bindings_expr()),
            Type::I32
        ),
        /*
            Chimera equivalent:
                let add = |x| |y| x + y;
                let result = add(1)(2);
                result
        */
        real_world_addition_pipeline: (
            &real_world_addition_pipeline_expr(),
            Type::I32
        ),
        /*
            Chimera equivalent:
                let add = |x| |y| x; // simulating (x + y) just returns x for type inference
                add(1)(2)
            note: the reason for curried functions is because this is how Hindley-Milner
            handles multiple parametered functions (converts them into single parameter function types,
            it allows partial application and function chaining)
        */
        add_like_operator: (
            &HMStmt::Expr(add_like_operator_expr()),
            Type::I32
        ),
        /*
            Chimera equivalent:
                if true {1}
                else {2}
        */
        if_else_conditional_branching: (
            &HMStmt::ControlFlow {
            branches: thin_vec![
                Conditional{condition: HMExpr::Bool, body: HMExpr::I32},
            ],
            else_block: Some(Box::new(HMExpr::I32)),
            },
            Type::I32,
        ),
        if_else_if_else_conditional_branching: (
            &HMStmt::ControlFlow {
            branches: thin_vec![
                Conditional{condition: HMExpr::Bool, body: HMExpr::I32},
                Conditional{condition: HMExpr::Bool, body: HMExpr::I32},
            ],
            else_block: Some(Box::new(HMExpr::I32)),
            },
            Type::I32,
        ),
        /*
            Chimera equivalent:
                let loopback = |x| if x == 0 { 0 } else { loopback(x - 1) };
                loopback(5)
        */
        recursive_lambda_with_base_case: (
            &HMStmt::Let {
                key: get_idx_for("loopback"),
                val: Box::new(HMStmt::Lambda {
                    params: thin_vec![get_idx_for("x")],
                    body: Box::new(HMStmt::ControlFlow {
                        branches: thin_vec![
                            Conditional{
                                condition: HMExpr::BinaryOp {
                                    op: BinaryOp::EqEq,
                                    lhs: Box::new(HMExpr::Var(get_idx_for("x"))),
                                    rhs: Box::new(HMExpr::I32),
                                },
                                body:HMExpr::I32,
                            },
                        ],
                        else_block: Some(Box::new(HMExpr::FnCall {
                            fn_to_call: Box::new(HMStmt::Expr(HMExpr::Var(get_idx_for("loopback")))),
                            args: thin_vec![HMStmt::Expr(HMExpr::BinaryOp {
                                op: BinaryOp::Sub,
                                lhs: Box::new(HMExpr::Var(get_idx_for("x"))),
                                rhs: Box::new(HMExpr::I32),
                            })],
                        })),
                    }),
                }),
                body: Box::new(HMStmt::Expr(HMExpr::FnCall {
                    fn_to_call: Box::new(HMStmt::Expr(HMExpr::Var(get_idx_for("loopback")))),
                    args: thin_vec![HMStmt::Expr(HMExpr::I32)],
                })),
            },
            Type::I32,
        ),
        /*
            Chimera equivalent:
            {
                let x = 1;
                x
            } 
        */
        block_with_final_expr: (
            &HMStmt::Expr(
                HMExpr::Block{
                statements: thin_vec![
                HMStmt::Let {
                    key: get_idx_for("x"),
                    val: Box::new(HMStmt::Expr(HMExpr::I32)),
                    body: Box::new(HMStmt::Expr(HMExpr::Var(get_idx_for("x")))),
                }],
                returns: thin_vec![0],
                }
            ),
            Type::I32,
        ),
        /*
            Chimera equivalent:
            {
                1;
                true;
            } 
        */
        block_with_semi_as_last: (
            &HMStmt::Expr(
                HMExpr::Block{
                statements: thin_vec![
                    HMStmt::Expr(HMExpr::I32),
                    HMStmt::Expr(HMExpr::Bool),
                ],
                returns: thin_vec![],
                }
            ),
            unit_type(),
        ),
        /*
            Chimera equivalent:
            {
                if false { 1 }
                else if false { 2 }
                else { 3 }
            } 
         */
        block_with_conditional_returns: (
            &HMStmt::Expr(
                HMExpr::Block{
                statements: thin_vec![
                    HMStmt::ControlFlow {
                        branches: thin_vec![
                            Conditional{ condition: HMExpr::Bool, body: HMExpr::Block{ returns: thin_vec![0], statements: thin_vec![HMStmt::Expr(HMExpr::I32)] }},
                            Conditional{ condition: HMExpr::Bool, body: HMExpr::Block{ returns: thin_vec![0], statements: thin_vec![HMStmt::Expr(HMExpr::I32)] }},
                        ],
                        else_block: Some(Box::new(HMExpr::Block {
                            statements: thin_vec![HMStmt::Expr(HMExpr::I32)],
                            returns: thin_vec![0],
                            })), 
                    }
                ],
                returns: thin_vec![0],
                },
            ),
            Type::I32,
        ),
    }

    fn assert_err_msg<T>(err: TypeInferenceResult<T>, exp_err_msg: &str) {
        match err {
            Ok(_) => unreachable!(),
            Err(err) => {
                if let Some(display) = err.help() {
                    assert_eq!(exp_err_msg, format!("{}", display))
                } else {
                    assert!(matches!(err, TypeInferenceError::Unbound(_)));
                }
            }
        }
    }
    #[test]
    fn test_unify_tensor_shape_mismatch() {
        /*
            Chimera equivalent:
                type([1, 2, 3]) == type([1, 2])
            (unification should fail due to shape mismatch)
        */

        let mut store = TypeStore::default();
        let mut ctx = TypeContext::default();

        let a = HMExpr::Tensor{data_type: Maybe::Checked(Box::new(Type::I32)), shape:thin_vec![Some(3)]};
        let b = HMExpr::Tensor{data_type: Maybe::Checked(Box::new(Type::I32)), shape:thin_vec![Some(2)]};

        let ty_a = infer_expr(&a, &mut ctx, &mut store).unwrap();
        let ty_b = infer_expr(&b, &mut ctx, &mut store).unwrap();
        let exp_err_msg = "Tensor shapes do not match : [Some(3)] vs [Some(2)]";
        assert_err_msg(store.unify(&ty_a, &ty_b), exp_err_msg);
    }

    #[test]
    fn test_unify_tuple_length_mismatch() {
        /*
            Chimera equivalent:
                type((1, true)) == type(1, true, false))
        */

        let mut store = TypeStore::default();
        let mut ctx = TypeContext::default();

        let a = HMExpr::Tuple(thin_vec![HMExpr::I32, HMExpr::Bool]);
        let b = HMExpr::Tuple(thin_vec![HMExpr::I32, HMExpr::Bool, HMExpr::Bool]);

        let ty_a = infer_expr(&a, &mut ctx, &mut store).unwrap();
        let ty_b = infer_expr(&b, &mut ctx, &mut store).unwrap();

        let exp_err_msg = "Type mismatch: Tuple([I32, Bool]) vs Tuple([I32, Bool, Bool])";
        assert_err_msg(store.unify(&ty_a, &ty_b), exp_err_msg);
    }

    #[test]
    fn test_apply_non_function_type() {
        /* Chimera equivalent:
        1(true)
        */

        let mut store = TypeStore::default();
        let mut ctx = TypeContext::default();

        let expr = HMExpr::FnCall {
            fn_to_call: Box::new(HMStmt::Expr(HMExpr::I32)),
            args: thin_vec![HMStmt::Expr(HMExpr::Bool)],
        };
        let result = infer_expr(&expr, &mut ctx, &mut store);

        let exp_err_msg =
            "Type mismatch: I32 vs FnSig { param_types: [Bool], return_type: Var(TypeVarId(0)) }";
        assert_err_msg(result, exp_err_msg);
    }

    #[test]
    fn test_unbound_variable_error() {
        /* Chimera equivalent:
            foo
        */

        let mut store = TypeStore::default();
        let mut ctx = TypeContext::default();

        let expr = HMExpr::Var(get_idx_for("foo"));
        let result = infer_expr(&expr, &mut ctx, &mut store);
        let exp_err_msg = "Unbound variable: foo";
        assert_err_msg(result, exp_err_msg);
    }

    #[test]
    fn test_infinitely_recursive_lambda_via_let_stays_unbound() {
        /* Chimera equivalent:
            let loopback = |x| loopback(x);
            loopback(1)
        */

        let mut store = TypeStore::default();
        let mut ctx = TypeContext::default();

        let stmt= HMStmt::Let {
            key: get_idx_for("loopback"),
            val: Box::new(HMStmt::Lambda {
                params: thin_vec![get_idx_for("x")],
                body: Box::new(HMStmt::Expr(HMExpr::FnCall {
                    fn_to_call: Box::new(HMStmt::Expr(HMExpr::Var(get_idx_for("loopback")))),
                    args: thin_vec![HMStmt::Expr(HMExpr::Var(get_idx_for("x")))],
                })),
            }),
            body: Box::new(HMStmt::Expr(HMExpr::FnCall {
                fn_to_call: Box::new(HMStmt::Expr(HMExpr::Var(get_idx_for("loopback")))),
                args: thin_vec![HMStmt::Expr(HMExpr::I32)],
            })),
        };

        let ty = infer_stmt(&stmt, &mut ctx, &mut store).unwrap();
        assert!(ty != Type::I32);
        assert!(matches!(ty, Type::Var(_)));
    }
    #[test]
    /// TODO: do I allow this in Chimera?
    fn test_recursive_polymorphic_function() {
        /*
            Chimera equivalent:
                let self_apply = fn(f: fn(i32) -> i32) -> i32 { f(10) };
                self_apply(fn(x: i32) -> i32 { x + 1 })
        */

        let mut store = TypeStore::default();
        let mut ctx = TypeContext::default();

        let fn_def = HMStmt::FnDef {
            key: get_idx_for("self_apply"),
            parameters: thin_vec![(
                get_idx_for("f"),
                Type::FnSig {
                    param_types: thin_vec![Type::I32],
                    return_type: Box::new(Type::I32),
                }
            )],
            return_type: Some(Type::I32),
            body: Box::new(HMStmt::Expr(HMExpr::FnCall {
                fn_to_call: Box::new(HMStmt::Expr(HMExpr::Var(get_idx_for("f")))),
                args: thin_vec![HMStmt::Expr(HMExpr::I32)],
            })),
        };

        let _ = infer_stmt(&fn_def, &mut ctx, &mut store);

        let expr = HMExpr::FnCall {
            fn_to_call: Box::new(HMStmt::Expr(HMExpr::Var(get_idx_for("self_apply")))),
            args: thin_vec![HMStmt::FnDef {
                key: get_idx_for("add1"),
                parameters: thin_vec![(get_idx_for("x"), Type::I32)],
                return_type: Some(Type::I32),
                body: Box::new(HMStmt::Expr(
                    HMExpr::BinaryOp {
                    op: BinaryOp::Add,
                    lhs: Box::new(HMExpr::Var(get_idx_for("x"))),
                    rhs: Box::new(HMExpr::I32),
                    }),
                )
            }],
        };

        let ty = infer_expr(&expr, &mut ctx, &mut store).unwrap();
        assert_eq!(ty, Type::I32);
    }
    #[test]
    fn test_conditional_branching_type_mismatch_should_fail() {
        /* Chimera equivalent:
            if true { 1 } else { true }
        */
        let mut store = TypeStore::default();
        let mut ctx = TypeContext::default();

        let stmt= HMStmt::ControlFlow {
            branches: thin_vec![Conditional {
                condition: HMExpr::Bool,
                body: HMExpr::I32
            },],
            else_block: Some(Box::new(HMExpr::Bool)),
        };

        let result = infer_stmt(&stmt, &mut ctx, &mut store);
        let exp_err_msg = "Type mismatch: Bool vs I32";
        assert_err_msg(result, exp_err_msg);
    }
    #[test]
    fn test_undefined_struct_init_fails() {
        /* Chimera equivalent:
            let p = Undefined{}
        */
        let mut store = TypeStore::default();
        let mut ctx = TypeContext::default();

        let expr = HMExpr::StructInit { key: get_idx_for("Undefined"), fields: thin_vec![] } ;

        let result = infer_expr(&expr, &mut ctx, &mut store);
        let exp_err_msg = "There is no declared type with key 'Idx::<SmolStr>(16)'";
        assert_err_msg(result, exp_err_msg);
    }
    #[test]
    fn test_cannot_unify_var_with_itself_in_function() {
        /* 
            t1: T
            t2: f(T) -> i32   
            
            note: t2 is a function that takes t1 itself, 
            if not handled, will cause infinite recursion
        */
        let mut store = TypeStore::default();

        let t = Type::Var(store.new_var_type());
        let func_type = Type::FnSig {
            param_types: thin_vec![t.clone()],
            return_type: Box::new(Type::I32),
        };

        let result = store.unify(&t, &func_type);
        let exp_err = TypeInferenceError::InfiniteType { 
            contained: TypeVarId(0), 
            contains: Type::FnSig { 
                param_types: thin_vec![Type::Var(TypeVarId(0))], return_type: Box::new(Type::I32) } };
        assert_eq!(result.unwrap_err(), exp_err);
    }
    #[test]
    fn unify_var_with_itself_in_tuple() {
        /* 
            t1: T
            t2: (T,..)    
            
            note: t2 is a tuple that takes t1 itself, 
            if not handled, will cause infinite recursion
        */
        let mut store = TypeStore::default();

        let t = Type::Var(store.new_var_type());
        let tuple_type = Type::Tuple(thin_vec![t.clone(), Type::I32]);

        let result = store.unify(&t, &tuple_type);
        let exp_err = TypeInferenceError::InfiniteType { 
            contained: TypeVarId(0), 
            contains: Type::Tuple(thin_vec![Type::Var(TypeVarId(0)), Type::I32])
        };

        assert_eq!(result.unwrap_err(), exp_err);
    }

#[test]
    fn unify_var_with_itself_in_tensor() {
        /* 
            t1: T
            t2: (T,..)    
            
            note: t2 is a tuple that takes t1 itself, 
            if not handled, will cause infinite recursion
        */
        let mut store = TypeStore::default();

        let t = Type::Var(store.new_var_type());
        let tensor_type = Type::Tensor {
            shape: thin_vec![Some(3)],
            data_type: Maybe::Checked(Box::new(t.clone())),
        };

        let result = store.unify(&t, &tensor_type);
        let exp_err = TypeInferenceError::InfiniteType { 
            contained: TypeVarId(0), 
            contains: Type::Tensor{shape: thin_vec![Some(3)], data_type:Maybe::Checked(Box::new(Type::Var(TypeVarId(0))))}
        };
        assert_eq!(result.unwrap_err(), exp_err);
    }

#[test]
fn unify_with_self() {
    let mut store = TypeStore::default();

    let t = Type::Var(store.new_var_type());
    let result = store.unify(&t, &t);

    assert!(result.is_ok(), "Expected unifying with self to pass!");
    }
}