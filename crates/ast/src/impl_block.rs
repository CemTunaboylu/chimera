use smol_str::ToSmolStr;
use syntax::{language::SyntaxNode, syntax_kind::SyntaxKind};
use thin_vec::{ThinVec, thin_vec};

use crate::{
    errors::ASTError,
    function::FnDef,
    lang_elems::{get_children_in, get_token_of_errs},
    types::Type,
};

#[derive(Clone, Debug, PartialEq)]
pub struct Impl {
    pub of: Type,
    pub methods: ThinVec<FnDef>,
}

impl TryFrom<&SyntaxNode> for Impl {
    type Error = ASTError;

    fn try_from(impl_node: &SyntaxNode) -> Result<Self, Self::Error> {
        let impl_type = get_token_of_errs(impl_node, SyntaxKind::StructAsType)?;
        let of = Type::Struct(impl_type.text().to_smolstr());

        let mut methods = thin_vec![];
        for fn_def in get_children_in(impl_node, SyntaxKind::FnDef) {
            let ast_fn_def = FnDef::try_from(&fn_def)?;
            methods.push(ast_fn_def);
        }

        Ok(Self { of, methods })
    }
}

#[cfg(test)]
mod tests {

    use smol_str::SmolStr;

    use super::*;
    use crate::{ast_root_from, cast_node_into_type};

    #[test]
    fn impl_block_for_struct() {
        let program = "impl Point { fn translate(&mut self, by: Point) { self.x += by.x; self.y += by.y; } fn rotate(&mut self, by: Point) { self.rotate_around(&by);} \n}";
        let fn_names = ["translate", "rotate"];
        let ast_root = ast_root_from(program);
        let impl_block =
            cast_node_into_type::<Impl>(ast_root.get_root().first_child().as_ref().unwrap());
        assert_eq!(Type::Struct(SmolStr::from("Point")), impl_block.of,);
        impl_block
            .methods
            .iter()
            .enumerate()
            .for_each(|(ix, fn_def)| assert!(fn_def.name() == fn_names[ix]));
    }
}
