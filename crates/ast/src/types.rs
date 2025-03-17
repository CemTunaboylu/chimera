use syntax::{language::SyntaxNode, syntax_kind::SyntaxKind};
use syntax::{
    language::{NodeOrToken, SyntaxToken},
    syntax_kind::SyntaxKind::*,
};
use thin_vec::ThinVec;

use crate::{
    ast::ASTResult,
    errors::ASTError,
    lang_elems::{
        assert_node_to_be_of_kind, error_for_node, error_for_token,
        get_filtered_children_with_tokens, get_token_of_errs,
    },
};

// possible types are primitives + custom types i.e. structs
#[derive(Clone, Debug)]
pub enum Type {
    Bool(SyntaxToken),
    Byte(SyntaxToken),
    Char(SyntaxToken),
    Float32(SyntaxToken),
    Integer32(SyntaxToken),
    String(SyntaxToken),
    Struct(SyntaxToken),
}

impl Type {
    fn validate_nodes_or_tokens(thin: &ThinVec<NodeOrToken>, parent: &SyntaxNode) -> ASTResult<()> {
        if thin.is_empty() {
            return Err(error_for_node(parent, "a type node"));
        } else if thin.len() > 1 {
            return Err(ASTError::new(
                parent.text_range().into(),
                "a single type",
                thin.as_ref(),
            ));
        }
        Ok(())
    }
}

impl TryFrom<&SyntaxNode> for Type {
    type Error = ASTError;

    fn try_from(parent_node: &SyntaxNode) -> Result<Self, Self::Error> {
        let mut types = SyntaxKind::types();
        types.push(StructAsType);

        // parent wraps the type token/node as ParamDecl(Ident, Colon, TyI32), or ParamDecl(Ident, Colon, StructAsType(Ident))
        let nodes_or_tokens = get_filtered_children_with_tokens(&parent_node, types);
        _ = Self::validate_nodes_or_tokens(&nodes_or_tokens, parent_node)?;

        let node_or_token = nodes_or_tokens.first().unwrap();
        match node_or_token {
            NodeOrToken::Node(struct_as_type) => {
                _ = assert_node_to_be_of_kind(struct_as_type, StructAsType)?;
                let token = get_token_of_errs(struct_as_type, Ident)?;
                Ok(Self::Struct(token))
            }
            NodeOrToken::Token(primitive) => match primitive.kind() {
                TyBool => Ok(Self::Bool(primitive.clone())),
                TyByte => Ok(Self::Byte(primitive.clone())),
                TyChar => Ok(Self::Char(primitive.clone())),
                TyF32 => Ok(Self::Float32(primitive.clone())),
                TyI32 => Ok(Self::Integer32(primitive.clone())),
                // str slice is a ref str
                TyStr | TyStrSlc => Ok(Self::String(primitive.clone())),
                _ => Err(error_for_token(primitive, SyntaxKind::types())),
            },
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::ast::tests::{ast_root_from, cast_into_type};
    use parameterized_test::create;

    create! {
        create_type_test,
        (program), {
        let ast_root = ast_root_from(program);
        cast_into_type::<Type>(ast_root.get_root());
        }
    }

    create_type_test! {
        valid_bool: "bool",
        valid_byte: "byte",
        valid_char: "char",
        valid_f32: "f32",
        valid_i32: "i32",
        valid_string: "String",
        valid_str_slice: "&str",
    }

    #[test]
    fn valid_struct_identifier() {
        let program = "fn def(arg:Structure)";
        let ast_root = ast_root_from(program);

        let fn_def_node = ast_root.get_root().first_child().unwrap();
        let param_node = fn_def_node
            .children()
            .find(|node| node.kind() == ParamDecl)
            .unwrap();
        cast_into_type::<Type>(&param_node);
    }
    #[test]
    fn invalid_type() {
        let program = "structure";
        let ast_root = ast_root_from(program);

        let result: ASTResult<Type> = ast_root.get_root().try_into();
        let err = result.expect_err("should have been errored");
        assert_eq!(
            "ASTError { err_span: 0..9, expected_but_found: \"expected a type node, but got Root\" }",
            format!("{:?}", err)
        );
    }
}
