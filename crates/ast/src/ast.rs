#![allow(dead_code)]

use crate::{errors::ASTError, statement::Stmt};
use miette::Report;
use syntax::{language::SyntaxNode, syntax_kind::SyntaxKind};

use parser::cst::ConcreteSyntaxTree;

// AST is a pseudo abstract syntax tree because it is built on
// top of the CST that's constructed by rowan.

pub type ASTResult<R> = Result<R, ASTError>;

#[derive(Debug)]
pub struct Root {
    root: SyntaxNode,
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
}

impl TryFrom<ConcreteSyntaxTree> for Root {
    type Error = ASTError;

    fn try_from(value: ConcreteSyntaxTree) -> Result<Self, Self::Error> {
        Self::try_from(&value)
    }
}

impl TryFrom<&ConcreteSyntaxTree> for Root {
    type Error = ASTError;

    fn try_from(value: &ConcreteSyntaxTree) -> Result<Self, Self::Error> {
        if SyntaxKind::Root == value.root.kind() {
            Ok(Self {
                root: value.root.clone(),
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
pub mod tests {
    use crate::ast_root_from;
    use thin_vec::ThinVec;

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
    fn try_from_let_binding_malformed_with_no_name() {
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
