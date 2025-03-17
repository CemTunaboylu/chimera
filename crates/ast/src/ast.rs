#![allow(dead_code)]

use crate::{errors::ASTError, literal::ParsedValueIndex, statement::Stmt};
use miette::Report;
use syntax::{ParsedValue, language::SyntaxNode, syntax_kind::SyntaxKind};

use parser::cst::{ConcreteSyntaxTree, IndicedParsedValues};

// AST is a pseudo abstract syntax tree because it is built on
// top of the CST that's constructed by rowan.

pub type ASTResult<R> = Result<R, ASTError>;

#[derive(Debug)]
pub struct Root {
    root: SyntaxNode,
    parsed_values: IndicedParsedValues,
}

impl Root {
    pub fn get_root(&self) -> &SyntaxNode {
        &self.root
    }
    pub fn statements(&self) -> impl Iterator<Item = Stmt> {
        self.root
            .children()
            .filter_map(|syntax_node| match Stmt::try_from(&syntax_node) {
                Ok(s) => Some(s),
                Err(ast_err) => {
                    let src = self.root.text().to_string();
                    let report: Report = ast_err.into();
                    println!("{:?}", report.with_source_code(src));
                    None
                }
            })
    }

    pub fn get_value_in_range(&self, index: ParsedValueIndex) -> Option<&ParsedValue> {
        self.parsed_values.get(&index)
    }
}

impl TryFrom<ConcreteSyntaxTree> for Root {
    type Error = ASTError;

    fn try_from(value: ConcreteSyntaxTree) -> Result<Self, Self::Error> {
        if SyntaxKind::Root == value.root.kind() {
            Ok(Self {
                root: value.root,
                parsed_values: value.parsed_values,
            })
        } else {
            let kind = value.root.kind();
            Err(ASTError::new(
                value.root.text_range().into(),
                SyntaxKind::Root,
                kind,
            ))
        }
    }
}

impl TryFrom<&ConcreteSyntaxTree> for Root {
    type Error = ASTError;

    fn try_from(value: &ConcreteSyntaxTree) -> Result<Self, Self::Error> {
        if SyntaxKind::Root == value.root.kind() {
            Ok(Self {
                root: value.root.clone(),
                parsed_values: value.parsed_values.clone(),
            })
        } else {
            let kind = value.root.kind();
            Err(ASTError::new(
                value.root.text_range().into(),
                SyntaxKind::Root,
                kind,
            ))
        }
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;

    use crate::ast::Root;
    use parser::parser::Parser;
    use std::fmt::Debug;
    use thin_vec::ThinVec;

    pub(crate) fn ast_root_from(program: &str) -> Root {
        let root = Parser::new(program).parse();
        Root::try_from(root).expect("should have been ok")
    }

    pub(crate) fn cast_into_type<'caller, T>(parent_node: &'caller SyntaxNode) -> T
    where
        T: TryFrom<&'caller SyntaxNode>,
        <T as TryFrom<&'caller SyntaxNode>>::Error: Debug,
    {
        let _desired_type: T = parent_node.try_into().expect(
            format!(
                "try_into should have been successful for {:?}",
                parent_node.text()
            )
            .as_str(),
        );
        _desired_type
    }

    #[test]
    /*
    Root@0..5
        VarDef@0..5
            KwLet@0..3 "let"
            Recovered@3..4
                Eq@3..4 "="
            Recovered@4..5
                Int@4..5 "9"
     */
    fn try_from_var_def_malformed_with_no_name() {
        let malformed = "let = 9";
        let root = ast_root_from(&malformed);
        let stmts = root.statements();
        let collected = stmts.collect::<ThinVec<_>>();
        assert_eq!(0, collected.len());
    }

    #[test]
    /*
    Root@0..13
      VarDef@0..13
        KwLet@0..3 "let"
        Whitespace@3..4 " "
        InfixBinOp@4..13
          VarRef@4..5
            Ident@4..5 "a"
          Whitespace@5..6 " "
          Eq@6..7 "="
          Whitespace@7..8 " "
          InfixBinOp@8..13
            Literal@8..9
              Int@8..9 "9"
            Whitespace@9..10 " "
            Plus@10..11 "+"
            Whitespace@11..12 " "
            Whitespace@12..13 "\n"
           */

    fn try_from_overflowing_number() {
        let overflowing = "let a = 9 + 18446744073709551616";
        let root = ast_root_from(&overflowing);
        let collected = root.statements().collect::<ThinVec<_>>();
        assert_eq!(0, collected.len());
    }
}
