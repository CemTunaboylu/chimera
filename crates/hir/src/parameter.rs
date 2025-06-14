use smol_str::SmolStr;

use crate::{
    HIRResult, builder::HIRBuilder, context::UsageContext, typing::hindley_milner::types::Type,
};
use ast::{parameter::Param as ASTParam, types::Type as ASTType};

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub enum Param {
    /// Represents a lambda parameter without an explicit type annotation.
    Generic {
        name: SmolStr,
        is_mut: bool,
    },
    Named {
        is_mut: bool,
        name: SmolStr,
        param_type: Type,
    },
    SelfRef {
        by_ref: bool,
        is_mut: bool,
    },
}

impl Param {
    fn set_as_mut(&mut self) {
        match self {
            Param::Generic { is_mut, .. } => *is_mut = true,
            Param::Named { is_mut, .. } => *is_mut = true,
            Param::SelfRef { is_mut, .. } => *is_mut = true,
        }
    }
}

impl HIRBuilder {
    pub fn lower_type_with_ctx(
        &mut self,
        param_type: &ASTType,
        ctx: UsageContext,
    ) -> HIRResult<Type> {
        self.push_usage_context(ctx);
        let t = self.lower_type(param_type);
        self.pop_usage_context();
        t
    }
    fn get_ctx_for(param: &ASTParam) -> UsageContext {
        if !param.by_ref {
            UsageContext::Moved
        } else if param.is_mut {
            UsageContext::RefMut
        } else {
            UsageContext::Ref
        }
    }
    pub fn lower_parameter(&mut self, param: &ASTParam) -> HIRResult<Param> {
        if param.param_type.is_none() {
            return Ok(Param::Generic {
                name: param.name.clone(),
                is_mut: param.is_mut,
            });
        }
        if param.name.is_empty() {
            return Ok(Param::SelfRef {
                by_ref: param.by_ref,
                is_mut: param.is_mut,
            });
        }
        let ctx = Self::get_ctx_for(param);
        let ty = self.lower_type_with_ctx(&param.param_type.as_ref().unwrap(), ctx)?;
        Ok(Param::Named {
            is_mut: param.is_mut,
            name: param.name.clone(),
            param_type: ty,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        builder::HIRBuilder, scope::placeholder_idx, typing::hindley_milner::types::Status,
    };
    use ast::{ast_root_from, cast_node_into_type};
    use parameterized_test::create;
    use smol_str::SmolStr;
    use thin_vec::ThinVec;

    fn get_params_nodes_from(program: &str) -> Vec<ASTParam> {
        let ast_root = ast_root_from(program);
        let fn_def_node = ast_root.get_root().first_child().unwrap();
        ast::parameter::Param::get_params_nodes_from(&fn_def_node)
            .into_iter()
            .map(|n| cast_node_into_type::<ASTParam>(&n))
            .collect()
    }

    create! {
        lower_self_ref_param,
        (program, exp_by_ref, exp_is_mut), {
            let ast_param = get_params_nodes_from(program).remove(0);
            let mut builder = HIRBuilder::new(ast_root_from(program));
            let hir_param = builder.lower_parameter(&ast_param).unwrap();
            assert_eq!(hir_param, Param::SelfRef { by_ref: exp_by_ref, is_mut: exp_is_mut });
        }
    }
    const BY_REF: bool = true;
    const IS_MUT: bool = true;
    lower_self_ref_param! {
        param_ref_mut_self: ("fn f(&mut self) {}", BY_REF, IS_MUT),
        param_ref_self: ("fn f(&self) {}", BY_REF, !IS_MUT),
        param_mut_self: ("fn f(mut self) {}", !BY_REF, IS_MUT),
        param_self: ("fn f(self) {}", !BY_REF, !IS_MUT),
    }

    create! {
        lower_named_param,
        (program, exp_is_mut, exp_name, exp_type), {
            let ast_param = get_params_nodes_from(program).remove(0);
            let mut builder = HIRBuilder::new(ast_root_from(program));
            let hir_param = builder.lower_parameter(&ast_param).unwrap();
            assert_eq!(hir_param, Param::Named {
                is_mut: exp_is_mut,
                name: SmolStr::new(exp_name),
                param_type: exp_type,
            });
        }
    }
    use crate::typing::hindley_milner::types::Type;
    lower_named_param! {
        param_ref_mut_struct: ("fn f(s: &mut Structure) {}", !IS_MUT, "s", Type::Ptr { is_mut: IS_MUT, of: Box::new(Type::StructAsType(Status::Pending(placeholder_idx())))}),
        param_ref_struct: ("fn f(s: &Structure) {}", !IS_MUT, "s", Type::Ptr { is_mut: !IS_MUT, of: Box::new(Type::StructAsType(Status::Pending(placeholder_idx())))}),
        param_struct: ("fn f(s: Structure) {}", !IS_MUT, "s", Type::StructAsType(Status::Pending(placeholder_idx()))),
        mut_param_ref_mut_struct: ("fn f(mut s: &mut Structure) {}", IS_MUT, "s", Type::Ptr { is_mut: true, of: Box::new(Type::StructAsType(Status::Pending(placeholder_idx())))}),
        mut_param_ref_struct: ("fn f(mut s: &Structure) {}", IS_MUT, "s", Type::Ptr { is_mut: !IS_MUT, of: Box::new(Type::StructAsType(Status::Pending(placeholder_idx())))}),
        mut_param_struct: ("fn f(mut s: Structure) {}", IS_MUT, "s", Type::StructAsType(Status::Pending(placeholder_idx()))),
        param_ref_mut_int: ("fn f(i: &mut i32) {}", !IS_MUT, "i", Type::Ptr { is_mut: IS_MUT, of: Box::new(Type::I32)}),
        param_ref_int: ("fn f(i: &i32) {}", !IS_MUT, "i", Type::Ptr { is_mut: !IS_MUT, of: Box::new(Type::I32)}),
        param_int: ("fn f(i: i32) {}", !IS_MUT, "i", Type::I32),
    }

    // ! this should be handled such that generic params are rejected in a function definition
    // #[test]
    // fn lower_generic_param() {
    //     let program = "fn f(g) {}";
    //     let ast_param = get_params_nodes_from(program).remove(0);
    //     let mut builder = crate::builder::HIRBuilder::new(ast_root_from(program));
    //     let hir_param = builder.lower_parameter(&ast_param).unwrap();
    //     assert_eq!(
    //         hir_param,
    //         Param::Generic {
    //             name: SmolStr::new("g"),
    //             is_mut: false
    //         }
    //     );
    // }

    #[test]
    fn lower_multiple_params() {
        let program = "fn f(&mut self, s: &Structure, c:i32) {}";
        let ast_params = get_params_nodes_from(program);
        let mut builder = HIRBuilder::new(ast_root_from(program));
        let lowered: ThinVec<_> = ast_params
            .iter()
            .map(|p| builder.lower_parameter(p).unwrap())
            .collect();
        assert!(matches!(
            lowered[0],
            Param::SelfRef {
                by_ref: true,
                is_mut: true
            }
        ));
        assert!(matches!(&lowered[1], Param::Named { name, .. } if name == "s"));
        assert!(matches!(&lowered[2], Param::Named { name, .. } if name == "c"));
    }

    #[test]
    fn lower_lambda_generic_params() {
        let program = "|g, en, eric| {g+en+eric}";
        let ast_root = ast_root_from(program);
        let literal_node = ast_root.get_root().first_child().unwrap();
        let lambda_node = literal_node.first_child().unwrap();
        let params_nodes = ast::parameter::Param::get_params_nodes_from(&lambda_node);
        let ast_params: Vec<_> = params_nodes
            .iter()
            .map(|n| cast_node_into_type::<ASTParam>(n))
            .collect();
        let mut builder = HIRBuilder::new(ast_root);
        let lowered: ThinVec<_> = ast_params
            .iter()
            .map(|p| builder.lower_parameter(p).unwrap())
            .collect();
        let names = ["g", "en", "eric"];
        for (hir_param, &name) in lowered.iter().zip(&names) {
            assert_eq!(
                hir_param,
                &Param::Generic {
                    name: SmolStr::new(name),
                    is_mut: false
                }
            );
        }
    }
}
