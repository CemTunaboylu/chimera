use ast::return_stmt::Return as ASTReturn;
use hir_macro::with_context;

use crate::{HIRResult, builder::HIRBuilder, context::UsageContext, scope::ExprIdx};

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub struct Return(ExprIdx);

impl Return {
    pub fn expr(&self) -> ExprIdx {
        self.0
    }
}

impl HIRBuilder {
    #[with_context(UsageContext::Return)]
    pub fn lower_return(&mut self, ret: &ASTReturn) -> HIRResult<Return> {
        let index = self.lower_expr_as_idx(ret.expr())?;
        Ok(Return(index))
    }
}

#[cfg(test)]
mod test {

    use super::*;
    use crate::builder::tests::ast_root_from;
    use ast::cast_node_into_type;
    use parameterized_test::create;

    create! {
        happy_path_return_test,
        (program), {
            let ast_root = ast_root_from(program);
            let return_node = ast_root.get_root().first_child().unwrap();
            let ast_return = cast_node_into_type::<ASTReturn>(&return_node);
            let mut hir_builder = HIRBuilder::new(ast_root);
            let low_return = hir_builder.lower_return(&ast_return).expect("should have been ok");
        }
    }

    happy_path_return_test! {
        return_true_literal: "return true;",
        return_binary_infix_op: "return here & now;",
        return_method_call: "return obj.blueprint();",
    }
}
