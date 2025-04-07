use ast::return_stmt::Return as ASTReturn;

use crate::{HIRResult, builder::HIRBuilder, scope::ExprIdx};

#[derive(Clone, Debug, PartialEq)]
pub struct Return(ExprIdx);

impl Return {
    pub fn expr(&self) -> ExprIdx {
        self.0
    }
}

impl HIRBuilder {
    pub fn lower_return(&mut self, ret: &ASTReturn) -> HIRResult<Return> {
        let index = self.lower_expr_as_idx(ret.expr())?;
        Ok(Return(index))
    }
}

#[cfg(test)]
mod test {

    use parameterized_test::create;

    use crate::builder::tests::ast_root_from;

    create! {
        happy_path_return_test,
        (program), {
            let ast_root = ast_root_from(program);
            let return_node = ast_root.get_root().first_child().unwrap();
            // _ = cast_node_into_type::<Return>(&return_node);
        }
    }

    happy_path_return_test! {
        return_true_literal: "return true;",
        return_binary_infix_op: "return here & now;",
        return_method_call: "return obj.blueprint();",
    }
}
