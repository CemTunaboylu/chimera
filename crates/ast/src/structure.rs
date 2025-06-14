use smol_str::{SmolStr, ToSmolStr};
use syntax::{
    bitset::SyntaxKindBitSet,
    language::{SyntaxNode, SyntaxToken},
    syntax_kind::SyntaxKind,
};
use thin_vec::{ThinVec, thin_vec};

use crate::{
    errors::ASTError,
    expression::Expr,
    lang_elems::{
        filter_irrelevant_out, first_child_of_kind_errs, get_children_in,
        get_kind_on_node_or_token, get_token_of_errs, get_tokens_in_errs,
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
        let mut types: SyntaxKindBitSet = SyntaxKind::types().into();
        types += [SyntaxKind::Ident, SyntaxKind::StructAsType]
            .as_ref()
            .into();
        let idents = get_tokens_in_errs(field_node, types)?;
        if idents.is_empty() {
            return Err(ASTError::new(
                field_node.text_range().into(),
                SyntaxKind::Ident,
                None::<SyntaxToken>,
            ));
        }
        let name = idents.first().unwrap().text().to_smolstr();
        let field_type = if idents.len() == 2 {
            let struct_as_type = idents.last().unwrap();
            Type::try_from(struct_as_type)?
        } else {
            let t = first_child_of_kind_errs(field_node, SyntaxKind::TyTensor)?;
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
        let mut fields = thin_vec![];
        for field in get_children_in(struct_def_node, SyntaxKind::StructField) {
            let ast_field = StructField::try_from(&field)?;
            fields.push(ast_field);
        }
        Ok(Self { name, fields })
    }
}
#[derive(Clone, Debug, PartialEq)]
pub struct StructLiteral {
    pub name: SmolStr,
    pub fields: ThinVec<StructFieldInit>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructFieldInit {
    pub name: SmolStr,
    pub value: Expr,
}

impl TryFrom<&SyntaxNode> for StructFieldInit {
    type Error = ASTError;

    fn try_from(field_node: &SyntaxNode) -> Result<Self, Self::Error> {
        let field_name_and_value =
            filter_irrelevant_out(field_node.children_with_tokens(), get_kind_on_node_or_token);
        if field_name_and_value.len() != 2 {
            return Err(ASTError::with_err_msg(
                field_node.text_range().into(),
                format!("expects exactly {:?} and an expression", SyntaxKind::Ident),
            ));
        }
        let name = field_name_and_value
            .first()
            .expect("an identifier")
            .as_token()
            .expect("as token")
            .text()
            .to_smolstr();
        let value = Expr::try_from(field_name_and_value.last().expect("an expression"))?;
        Ok(Self { name, value })
    }
}

impl TryFrom<&SyntaxNode> for StructLiteral {
    type Error = ASTError;

    fn try_from(struct_lit_node: &SyntaxNode) -> Result<Self, Self::Error> {
        let name = get_token_of_errs(struct_lit_node, SyntaxKind::Ident)?
            .text()
            .to_smolstr();
        let mut fields = thin_vec![];
        for field in get_children_in(struct_lit_node, SyntaxKind::StructField) {
            let field_init = StructFieldInit::try_from(&field)?;
            fields.push(field_init);
        }
        Ok(Self { name, fields })
    }
}
#[cfg(test)]
mod tests {

    use super::*;
    use crate::{
        ast_root_from, cast_node_into_type,
        literal::{Literal, Value},
        types::Type,
    };

    #[test]
    fn struct_def() {
        let field_names = ["coordinates", "item"];
        let field_types = [
            Type::Tensor {
                ty: Some(Box::new(Type::Float32)),
                shape: thin_vec![Some(Expr::Literal(Literal(Value::Int(3))))],
            },
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
    #[test]
    fn struct_init() {
        let field_names = ["coordinates", "item"];
        let program = "Tester{ coordinates: origin_tensor, item: origin_item}";
        let ast_root = ast_root_from(program);
        let struct_literal = cast_node_into_type::<StructLiteral>(
            ast_root.get_root().first_child().as_ref().unwrap(),
        );
        assert_eq!("Tester", struct_literal.name);
        let fields = struct_literal.fields;
        for (exp_name, got_name) in field_names.iter().zip(fields.iter()) {
            assert_eq!(*exp_name, got_name.name);
        }
    }
}
