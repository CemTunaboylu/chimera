use crate::{
    HIRResult,
    builder::HIRBuilder,
    climbing::EnumeratedScope,
    control_flow::{Conditional, ControlFlow},
    definition_allocator::{NameIndexed, NameToIndexTrie},
    expression::Expr,
    function::FnArg,
    index_types::{ExprIdx, ScopeIdx, placeholder_idx},
    indexing::Indexing,
    scope::Scope,
    scope::Selector,
    span::Span,
    statement::Stmt,
    typing::hindley_milner::types::{Maybe, Status, Type},
    unwrap_or_err,
};
use la_arena::Idx;
use miette::{Diagnostic, Report};
use smol_str::SmolStr;
use thin_vec::ThinVec;
use thiserror::Error;

use core::hash::Hash;

use std::{borrow::Cow, collections::HashMap, fmt::Debug};

pub type ResolutionResult<T> = Result<T, Report>;

#[derive(Clone, Diagnostic, Debug, PartialEq, Error)]
#[diagnostic()]
#[error("ResolutionError")]
// Source will be injected at the end by the caller
pub struct ResolutionError {
    #[help]
    msg: String,
}

impl ResolutionError {
    pub fn unresolved(unresolved: impl Debug) -> Self {
        let msg = format!("{:?} is unresolved", unresolved);
        ResolutionError { msg }
    }
    pub fn already_resolved(cannot_resolve: impl Debug) -> Self {
        let msg = format!(
            "cannot resolve {:?}, it is already resolved",
            cannot_resolve
        );
        ResolutionError { msg }
    }

    pub fn new_with_guestimate(cannot_resolve: impl Debug, guestimate_help: impl Debug) -> Self {
        let msg = format!(
            "cannot resolve {:?}, did you mean {:?}",
            cannot_resolve, guestimate_help
        );
        ResolutionError { msg }
    }
}
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum ResolutionType {
    Container(Span),
    Fn,
    Struct,
    Var(Span),
}

impl ResolutionType {
    fn get_span(&self) -> Option<Span> {
        match self {
            ResolutionType::Container(range) => Some(range.clone()),
            ResolutionType::Var(range) => Some(range.clone()),
            _ => None,
        }
    }
}

fn span_check(
    scope: &Scope,
    key: &SmolStr,
    resolution_type: &ResolutionType,
) -> ResolutionResult<()> {
    match (resolution_type.get_span(), scope.name_to_spans.get(key)) {
        (None, None) => Ok(()),
        (None, Some(_)) => Ok(()),
        (Some(_), None) => Err(ResolutionError {
            msg: format!("cannot resolve binding {:?}", key),
        }
        .into()),
        (Some(ref_scope), Some(def_scope)) => {
            if ref_scope.start <= def_scope.end {
                Err(ResolutionError {
                    msg: format!(
                        "binding {:?} references a value that is not yet defined",
                        key
                    ),
                }
                .into())
            } else {
                Ok(())
            }
        }
    }
}

pub fn resolve<'caller, E, S: Selector<E>>(
    mut scopes: impl Iterator<Item = EnumeratedScope<'caller>>,
    unresolved_ref: &Reference<E>,
) -> ResolutionResult<Reference<E>>
where
    E: Clone + Debug + Eq + Hash + PartialEq + PartialOrd + NameIndexed,
{
    let (mut scope_idx, mut scope) = *unwrap_or_err(
        scopes.next().as_ref(),
        format!("cannot find scope for {:?}", unresolved_ref).as_str(),
    )?;
    let (key, resolution_type, baggage) = {
        let idx = unresolved_ref.get_unresolved_index()?;
        let unresolved = &scope.to_resolve[idx];
        (&unresolved.name, &unresolved.for_type, &unresolved.baggage)
    };

    let mut guesstimates = ThinVec::new();
    loop {
        if let Some((name_idx, obj_idx)) = scope.resolve_in::<E, S>(key) {
            span_check(scope, key, resolution_type)?;
            let resolved = Reference::<E>::Resolved {
                at: scope_idx,
                baggage: baggage.clone(),
                name_idx,
                obj_idx,
            };
            return Ok(resolved);
        } else {
            guesstimates.push(guess(&S::select_alloc(scope).name_to_idx_trie, key));
            let next = scopes.next();
            if next.is_none() {
                break Err(ResolutionError::new_with_guestimate(key, guesstimates).into());
            }
            (scope_idx, scope) = next.unwrap();
        }
    }
    // Err(ResolutionError::new_with_guestimate(key, guesstimates).into())
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub enum Baggage {
    None,
    Arg(ThinVec<FnArg>),
    Index(ThinVec<Indexing>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Unresolved {
    pub name: SmolStr,
    pub baggage: Baggage,
    pub for_type: ResolutionType,
}

impl Unresolved {
    pub fn baggaged(name: SmolStr, baggage: Baggage, t: ResolutionType) -> Self {
        Self {
            name,
            baggage,
            for_type: t,
        }
    }
    pub fn new(name: SmolStr, t: ResolutionType) -> Self {
        Self {
            name,
            baggage: Baggage::None,
            for_type: t,
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub enum Reference<R>
where
    R: Eq + Hash + PartialEq + PartialOrd,
{
    Unresolved(Idx<Unresolved>),
    Resolved {
        at: ScopeIdx,
        baggage: Baggage,
        name_idx: Idx<SmolStr>,
        obj_idx: Idx<R>,
    },
}

impl<D: Default> Default for Reference<D>
where
    D: Eq + Hash + PartialEq + PartialOrd,
{
    fn default() -> Self {
        Self::Unresolved(placeholder_idx())
    }
}

impl<R: Debug> Reference<R>
where
    R: Eq + Hash + PartialEq + PartialOrd,
{
    pub fn get_unresolved_index(&self) -> ResolutionResult<Idx<Unresolved>> {
        match self {
            Reference::Resolved { .. } => Err(ResolutionError::already_resolved(self).into()),
            Reference::Unresolved(idx) => Ok(*idx),
        }
    }
    pub fn get_obj_index(&self) -> ResolutionResult<Idx<R>> {
        match self {
            Reference::Resolved {
                at: _,
                baggage: _,
                name_idx: _,
                obj_idx,
            } => Ok(*obj_idx),
            Reference::Unresolved(_) => Err(ResolutionError::unresolved(self).into()),
        }
    }
}

fn does_type_slice_need_resolution(types: &[Type]) -> bool {
    types.iter().any(|t| does_type_need_resolution(t))
}
fn does_maybe_need_resolution(m: &Maybe<Type>) -> bool {
    match m {
        Maybe::None => return false,
        Maybe::Checked(_) => return false,
        Maybe::Unchecked(thin_vec) => does_type_slice_need_resolution(thin_vec.as_slice()),
    }
}
fn does_type_need_resolution(t: &Type) -> bool {
    let types = match t {
        Type::Buffer { data_type, .. } => return does_maybe_need_resolution(data_type),
        Type::FnSig {
            param_types,
            return_type,
        } => {
            if does_type_need_resolution(return_type) {
                return true;
            }
            param_types
        }
        Type::Ptr { of, .. } => return does_type_need_resolution(of),
        Type::StructAsType(status) => return matches!(status, Status::Pending(_)),
        Type::Struct { fields, .. } => fields,
        Type::Tensor { data_type, .. } => return does_maybe_need_resolution(data_type),
        Type::Tuple(thin_vec) => thin_vec,
        Type::Var(_) => return true,
        _ => return false,
    };
    does_type_slice_need_resolution(types)
}
impl HIRBuilder {
    pub fn allocate_for_resolution(&mut self, unresolved: Unresolved) -> Idx<Unresolved> {
        let current_scope = self.get_current_scope_mut();
        current_scope.to_resolve.alloc(unresolved)
    }
    pub fn needs_resolution(&self, expr: &Expr) -> bool {
        match expr {
            Expr::Block(block) => todo!(),
            Expr::Class(t) => does_type_need_resolution(t),
            Expr::FnCall(reference) => todo!(),
            Expr::LitCall(call) => todo!(),
            Expr::Indexing(indexing) => todo!(),
            Expr::Infix(binary_infix) => todo!(),
            Expr::Literal(literal) => todo!(),
            Expr::Missing => todo!(),
            Expr::Mut(_) => todo!(),
            Expr::Paren(paren) => todo!(),
            Expr::SelfRef(self_ref) => todo!(),
            Expr::Tuple(tuple) => todo!(),
            Expr::Unary(unary) => todo!(),
            Expr::Unit => todo!(),
            Expr::VarRef(reference) => todo!(),
            Expr::TensorExpr(tensor_expr) => todo!(),
        }
    }

    pub fn resolve_all(&mut self) -> HIRResult<()> {
        let relocation_of_indices = HashMap::<ExprIdx, ExprIdx>::new();
        for scope in self.scopes.values_mut() {
            for stmt in scope.statements.values_mut() {
                match stmt {
                    Stmt::ControlFlow(control_flow) => {}
                    Stmt::Expr(idx) => todo!(),
                    Stmt::FnDef(idx) => todo!(),
                    Stmt::Impl(_) => todo!(),
                    Stmt::Jump(jump) => todo!(),
                    Stmt::Loop(_) => todo!(),
                    Stmt::Return(_) => todo!(),
                    Stmt::Semi(semi) => todo!(),
                    Stmt::StructDef(idx) => todo!(),
                    Stmt::LetBinding(thin_vec) => todo!(),
                }
            }
        }

        Ok(())
    }
    pub fn resolve_control_flow(
        &mut self,
        mut c_flow: ControlFlow,
        relocation_indices: &mut HashMap<ExprIdx, ExprIdx>,
    ) -> HIRResult<ControlFlow> {
        for conditional in c_flow.0.iter_mut() {
            match conditional {
                Conditional::If(condition, block) => {
                    let condition_expr = self.get_expr(&condition.0);
                }
                Conditional::Elif(condition, block) => todo!(),
                Conditional::Else(block) => todo!(),
            }
        }
        Ok(c_flow)
    }
}

fn guess<'caller, T>(
    trie: &'caller NameToIndexTrie<T>,
    key: &'caller SmolStr,
) -> Cow<'caller, str> {
    let empty: &[u8] = &[];
    let guesstimate = trie
        .get_longest_common_prefix(key)
        .map(|(chars, _)| chars)
        .unwrap_or(empty);
    String::from_utf8_lossy(guesstimate)
}

// ! TODO: check if can be used or delete
pub fn resolve_with_err<T>(trie: &NameToIndexTrie<T>, key: &SmolStr) -> ResolutionResult<Idx<T>> {
    if let Some(idx) = trie.get(key) {
        Ok(*idx)
    } else {
        let guesstimate = guess::<T>(trie, key);
        Err(ResolutionError::new_with_guestimate(key, guesstimate).into())
    }
}
