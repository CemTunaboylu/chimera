use smol_str::{SmolStr, ToSmolStr};
use syntax::{
    language::{SyntaxNode, SyntaxToken},
    syntax_kind::SyntaxKind,
};
use thin_vec::{ThinVec, thin_vec};

use crate::{
    errors::ASTError,
    lang_elems::{
        first_child_of_kind_errs, get_children_in, get_token_of_errs, get_tokens_in_errs,
    },
    types::Type,
};

#[derive(Clone, Debug, PartialEq)]
pub struct StructDef {
    pub name: SmolStr,
    pub fields: ThinVec<StructField>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructField {
    pub name: SmolStr,
    pub field_type: Type,
}

impl TryFrom<&SyntaxNode> for StructField {
    type Error = ASTError;

    fn try_from(field_node: &SyntaxNode) -> Result<Self, Self::Error> {
        let mut types = SyntaxKind::types();
        types.extend_from_slice([SyntaxKind::Ident, SyntaxKind::StructAsType].as_ref());
        let idents = get_tokens_in_errs(field_node, types.as_ref())?;
        if idents.is_empty() {
            return Err(ASTError::new(
                field_node.text_range().into(),
                SyntaxKind::Ident,
                None::<SyntaxToken>,
            ));
        }
        let name = idents.get(0).unwrap().text().to_smolstr();
        let field_type = if idents.len() == 2 {
            let struct_as_type = idents.last().unwrap();
            Type::try_from(struct_as_type)?
        } else {
            let t = first_child_of_kind_errs(field_node, SyntaxKind::TensorType)?;
            Type::try_from(&t)?
        };
        Ok(Self { name, field_type })
    }
}

impl TryFrom<&SyntaxNode> for StructDef {
    type Error = ASTError;

    fn try_from(struct_def_node: &SyntaxNode) -> Result<Self, Self::Error> {
        let name = get_token_of_errs(struct_def_node, SyntaxKind::Ident)?
            .text()
            .to_smolstr();
        let struct_fields_node =
            first_child_of_kind_errs(struct_def_node, SyntaxKind::StructFields)?;
        let mut fields = thin_vec![];
        for field in get_children_in(&struct_fields_node, SyntaxKind::StructField) {
            let ast_field = StructField::try_from(&field)?;
            fields.push(ast_field);
        }
        Ok(Self { name, fields })
    }
}
#[cfg(test)]
mod tests {

    use super::*;
    use crate::{ast_root_from, cast_node_into_type, tensor::Hint, types::Type};

    #[test]
    fn struct_def() {
        let field_names = ["coordinates", "item"];
        let field_types = [
            Type::Tensor(thin_vec![Hint::Type(Type::Float32), Hint::Dim(3)]),
            Type::Struct(SmolStr::new("Item")),
        ];
        let program = "struct Tester { coordinates: tensor<f32><3>, item: Item }";
        let ast_root = ast_root_from(program);
        let struct_def =
            cast_node_into_type::<StructDef>(ast_root.get_root().first_child().as_ref().unwrap());
        assert_eq!("Tester", struct_def.name);
        let fields = struct_def.fields;
        for (ix, (n, t)) in field_names.iter().zip(field_types.iter()).enumerate() {
            assert_eq!(*n, fields[ix].name);
            assert_eq!(*t, fields[ix].field_type);
        }
    }
}
