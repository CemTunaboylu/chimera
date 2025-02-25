#![allow(dead_code)]

use std::ops::Range;

use miette::{Diagnostic, Report, Result as MietteResult};
use smol_str::{SmolStr, ToSmolStr};
use syntax::{
    language::{SyntaxElement, SyntaxNode, SyntaxToken},
    syntax::SyntaxKind,
};
use thiserror::Error;

use crate::errors::{HasSpan, Inner, Stringer};

// AST is a pseudo abstract syntax tree because it is built on
// top of the CST that's constructed by rowan.

pub type ASTResult<R> = MietteResult<R, ASTError>;

#[derive(Clone, Diagnostic, Debug, PartialEq, Error)]
#[error("ASTError")]
pub struct ASTError {
    #[source]
    #[diagnostic_source]
    cause: Inner,
}

impl ASTError {
    fn new(span: Range<usize>, got: impl Stringer, exp: impl Stringer) -> Self {
        let cause = Inner::new("".to_string(), span, exp, got);
        Self { cause }
    }
    fn for_has_span(spanned: impl HasSpan, got: impl Stringer, exp: impl Stringer) -> Self {
        let cause = Inner::new("".to_string(), spanned.get_span(), exp, got);
        Self { cause }
    }

    fn for_expr(expr: Expr, exp: impl Stringer) -> Self {
        let span = expr.get_as_has_span().get_span();
        let cause = Inner::new("".to_string(), span, exp, expr);
        Self { cause }
    }
}

#[derive(Debug)]
pub struct Root(pub SyntaxNode);

impl Root {
    fn inject_src_to_captured_err(&self, ast_err: &mut ASTError) {
        let src = self.0.text().to_string();
        match ast_err {
            &mut ASTError { ref mut cause } => (*cause).src = src,
        }
    }
    pub fn statements(&self) -> impl Iterator<Item = Stmt> {
        self.0
            .children()
            .filter_map(|syntax_node| match Stmt::try_from(syntax_node) {
                Ok(s) => Some(s),
                Err(mut ast_err) => {
                    self.inject_src_to_captured_err(&mut ast_err);
                    let report: Report = ast_err.into();
                    println!("{:?}", report);
                    None
                }
            })
    }
}

impl TryFrom<SyntaxNode> for Root {
    type Error = ASTError;

    fn try_from(node: SyntaxNode) -> Result<Self, Self::Error> {
        if SyntaxKind::Root == node.kind() {
            Ok(Self(node))
        } else {
            let kind = node.kind();
            Err(ASTError::for_has_span(&node, kind, SyntaxKind::Root))
        }
    }
}

#[derive(Clone, Debug)]
pub enum Infix {
    Binary(SyntaxNode),
}

impl Infix {
    fn get_syntax_node(&self) -> &SyntaxNode {
        match self {
            Self::Binary(syntax_node) => syntax_node,
        }
    }

    pub fn lhs(&self) -> Option<Expr> {
        let syntax_node = self.get_syntax_node();
        syntax_node.children().find_map(try_from_opted::<Expr>)
    }

    pub fn rhs(&self) -> Option<Expr> {
        let syntax_node = self.get_syntax_node();
        syntax_node
            .children()
            .filter_map(try_from_opted::<Expr>)
            .nth(1)
    }

    pub fn op(&self) -> Option<SyntaxToken> {
        let syntax_node = self.get_syntax_node();
        syntax_node
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind().is_binary_operator())
    }
}

#[derive(Debug)]
pub struct VarDef(VarRef, Option<Expr>);

impl VarDef {
    pub fn name(&self) -> SmolStr {
        self.0.name()
    }

    pub fn value(&self) -> Option<&Expr> {
        self.1.as_ref()
    }
}

impl TryFrom<SyntaxNode> for VarDef {
    type Error = ASTError;

    fn try_from(node: SyntaxNode) -> Result<Self, Self::Error> {
        let infix = node
            .children()
            .filter(|c| try_from_opted::<Expr>(c.clone()).is_some())
            .nth(0);

        if infix.is_none() {
            let recovered = node
                .children()
                .filter(|node| node.kind() == SyntaxKind::Recovered)
                .nth(0)
                .unwrap()
                .parent()
                .unwrap();

            let got = recovered.text().to_string();
            return Err(ASTError::for_has_span(
                &recovered,
                got.as_str(),
                "a valid assignment",
            ));
        }
        let infix = infix.unwrap();
        let mut exprs = infix.children().filter_map(try_from_opted::<Expr>);
        let name_as_var_ref = exprs.next();
        let var_ref = if name_as_var_ref.is_none() {
            let got = infix.kind();
            return Err(ASTError::for_has_span(
                &infix,
                got,
                "non-None and Expr::VarRef",
            ));
        } else {
            match name_as_var_ref.unwrap() {
                Expr::VarRef(var_ref) => var_ref,
                unwanted => {
                    return Err(ASTError::for_expr(unwanted, "Expr::VarRef"));
                }
            }
        };
        let assignment = exprs.next();
        Ok(Self(var_ref, assignment))
    }
}

#[derive(Debug)]
pub struct VarRef(SyntaxNode);

impl VarRef {
    pub fn name(&self) -> SmolStr {
        self.0
            .children_with_tokens()
            .find_map(SyntaxElement::into_token)
            .unwrap()
            .text()
            .to_smolstr()
    }

    fn get_syntax_node(&self) -> &SyntaxNode {
        &self.0
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Char(char),
    Str(SmolStr),
    Num(usize),
}

#[derive(Debug)]

pub enum LiteralValue {
    Char(SyntaxToken),
    Str(SyntaxToken),
    Num(SyntaxToken),
}

fn value_parsing_error(syntax_token: &SyntaxToken, err_str: &str) -> ASTError {
    ASTError::for_has_span(syntax_token, syntax_token.text(), err_str)
}
impl LiteralValue {
    pub fn value(&self) -> ASTResult<Value> {
        match self {
            LiteralValue::Char(syntax_token) => syntax_token.text().parse::<char>().map_or_else(
                |err| Err(value_parsing_error(syntax_token, err.to_string().as_str())),
                |ch| Ok(Value::Char(ch)),
            ),

            LiteralValue::Str(syntax_token) => Ok(Value::Str(syntax_token.text().to_smolstr())),
            LiteralValue::Num(syntax_token) => syntax_token.text().parse::<usize>().map_or_else(
                |err| Err(value_parsing_error(syntax_token, err.to_string().as_str())),
                |num| Ok(Value::Num(num)),
            ),
        }
    }
}
impl TryFrom<SyntaxToken> for LiteralValue {
    type Error = ASTError;

    fn try_from(node: SyntaxToken) -> Result<Self, Self::Error> {
        let result = match node.kind() {
            SyntaxKind::StringLiteral => Self::Str(node),
            SyntaxKind::CharLiteral => Self::Char(node),
            SyntaxKind::Number => Self::Num(node),
            kind => {
                return Err(ASTError::for_has_span(
                    &node,
                    kind,
                    [
                        SyntaxKind::StringLiteral,
                        SyntaxKind::CharLiteral,
                        SyntaxKind::Number,
                    ]
                    .as_ref(),
                ));
            }
        };

        Ok(result)
    }
}
#[derive(Debug)]
pub struct Literal(LiteralValue);

impl Literal {
    pub fn value(&self) -> ASTResult<Value> {
        self.0.value()
    }

    fn get_syntax_token(&self) -> &SyntaxToken {
        match &self.0 {
            LiteralValue::Char(syntax_token) => syntax_token,
            LiteralValue::Str(syntax_token) => syntax_token,
            LiteralValue::Num(syntax_token) => syntax_token,
        }
    }
}

impl TryFrom<SyntaxNode> for Literal {
    type Error = ASTError;

    fn try_from(node: SyntaxNode) -> Result<Self, Self::Error> {
        let node_containing_value = node
            .children_with_tokens()
            .filter(|syntax_node| syntax_node.kind().is_literal_value())
            .find_map(SyntaxElement::into_token)
            .unwrap();

        LiteralValue::try_from(node_containing_value).map(|literal_value| Self(literal_value))
    }
}

#[derive(Debug)]
pub struct Paren(SyntaxNode);

impl Paren {
    pub fn expr(&self) -> Option<Expr> {
        self.0.children().find_map(try_from_opted::<Expr>)
    }

    fn get_syntax_node(&self) -> &SyntaxNode {
        &self.0
    }
}

#[derive(Debug)]
pub enum Unary {
    Prefix(SyntaxNode),
    Postfix(SyntaxNode),
}

impl Unary {
    fn get_syntax_node(&self) -> &SyntaxNode {
        match self {
            Self::Prefix(syntax_node) => syntax_node,
            Self::Postfix(syntax_node) => syntax_node,
        }
    }
    pub fn expr(&self) -> Option<Expr> {
        let syntax_node = self.get_syntax_node();
        syntax_node.children().find_map(try_from_opted::<Expr>)
    }

    pub fn op(&self) -> Option<SyntaxToken> {
        let syntax_node = self.get_syntax_node();
        syntax_node
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind().is_unary_operator())
    }
}

// TODO: fix this hack
fn try_from_opted<S: TryFrom<SyntaxNode, Error = ASTError>>(node: SyntaxNode) -> Option<S> {
    match S::try_from(node) {
        Ok(s) => Some(s),
        Err(ast_err) => {
            // let report: Report = ast_err.into();
            // println!("{:?}", report);
            None
        }
    }
}
#[derive(Debug)]
pub enum Expr {
    Infix(Infix),
    Literal(Literal),
    Paren(Paren),
    Unary(Unary),
    VarRef(VarRef),
}

impl Expr {
    fn get_as_has_span(&self) -> Box<dyn HasSpan + '_> {
        let node = match self {
            Expr::Infix(infix) => infix.get_syntax_node(),
            Expr::Unary(unary) => unary.get_syntax_node(),
            Expr::Paren(paren) => paren.get_syntax_node(),
            Expr::VarRef(var_ref) => var_ref.get_syntax_node(),
            Expr::Literal(literal) => return Box::new(literal.get_syntax_token()),
        };
        Box::new(node)
    }
}

impl TryFrom<SyntaxNode> for Expr {
    type Error = ASTError;

    fn try_from(node: SyntaxNode) -> Result<Self, Self::Error> {
        let result = match node.kind() {
            SyntaxKind::InfixBinaryOp => Self::Infix(Infix::Binary(node)),
            SyntaxKind::PrefixUnaryOp => Self::Unary(Unary::Prefix(node)),
            SyntaxKind::PostFixUnaryOp => Self::Unary(Unary::Postfix(node)),
            SyntaxKind::Literal => Self::Literal(try_from_opted::<Literal>(node).unwrap()),
            SyntaxKind::ParenExpr => Self::Paren(Paren(node)),
            SyntaxKind::VariableRef => Self::VarRef(VarRef(node)),
            kind => {
                return Err(ASTError::for_has_span(
                    &node,
                    kind,
                    [
                        SyntaxKind::InfixBinaryOp,
                        SyntaxKind::Literal,
                        SyntaxKind::ParenExpr,
                        SyntaxKind::PrefixUnaryOp,
                        SyntaxKind::VariableRef,
                    ]
                    .as_ref(),
                ));
            }
        };

        Ok(result)
    }
}

impl Stringer for Expr {
    fn into(self) -> String {
        format!("{:?}", self)
    }
}

#[derive(Debug)]
pub enum Stmt {
    VarDef(VarDef),
    Expr(Expr),
}

impl TryFrom<SyntaxNode> for Stmt {
    type Error = ASTError;

    fn try_from(node: SyntaxNode) -> Result<Self, Self::Error> {
        match node.kind() {
            SyntaxKind::VariableDef => match VarDef::try_from(node) {
                Ok(var_def) => Ok(Self::VarDef(var_def)),
                Err(err) => Err(err),
            },
            _ => match Expr::try_from(node) {
                Ok(expr) => Ok(Self::Expr(expr)),
                Err(err) => Err(err),
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{parse_behaviors::IgnoreTrivia, parser::Parser};

    fn parse(program: &str) -> Root {
        let parse = Parser::new(program).parse::<IgnoreTrivia>();
        Root::try_from(parse.syntax_node_root()).unwrap()
    }
    #[test]
    /*
    Root@0..5
        VariableDef@0..4
            LetKw@0..3 "let"
            Recovered@3..4
                Eq@3..4 "="
        Literal@4..5
            Number@4..5 "9"
     */
    fn try_from_var_def_malformed_with_no_name() {
        let malformed = "let = 9";
        let root = parse(&malformed);
        let stmts = root.statements();
        assert_eq!(0, stmts.collect::<Vec<_>>().len());
    }

    #[test]
    /*
    Root@0..27
        VariableDef@0..27
            LetKw@0..3 "let"
            InfixBinaryOp@3..27
                VariableRef@3..4
                    Identifier@3..4 "a"
                Eq@4..5 "="
                InfixBinaryOp@5..27
                    Literal@5..6
                        Number@5..6 "9"
                    Plus@6..7 "+"
                    Literal@7..27
                        Number@7..27 "18446744073709551616"

    VarDef(
            VarDef(
                VarRef(
                    VariableRef@3..4
                      Identifier@3..4 "a"
                ),
                Some(
                    Infix(
                        Binary(
                            InfixBinaryOp@5..27
                              Literal@5..6
                                Number@5..6 "9"
                              Plus@6..7 "+"
                              Literal@7..27
                                Number@7..27 "18446744073709551616"
                        ),
                    ),
                ),
            ),
        ),
       */

    fn try_from_overflowing_number() {
        let overflowing = "let a = 9 + 18446744073709551616";
        let root = parse(&overflowing);
        let stmt = root.statements().next().unwrap();
        let var_def = if let Stmt::VarDef(var_def) = stmt {
            var_def
        } else {
            panic!("Stmt should have been a variable definition");
        };
        let name = var_def.name();
        assert_eq!(name.as_str(), "a");
        let infix = if let Expr::Infix(infix) = var_def.value().unwrap() {
            infix
        } else {
            panic!("Expr should have been an infix operation");
        };
        let op = infix.op().unwrap();
        assert_eq!(op.kind(), SyntaxKind::Plus);

        let lhs_literal = match infix.lhs().unwrap() {
            Expr::Literal(literal) => literal,
            _ => unreachable!(),
        };

        assert_eq!(lhs_literal.value(), ASTResult::Ok(Value::Num(9)));
        let rhs_literal = match infix.rhs().unwrap() {
            Expr::Literal(literal) => literal,
            _ => unreachable!(),
        };

        assert!(rhs_literal.value().is_err());
        assert_eq!(
            rhs_literal.value(),
            Err(ASTError::new(
                7..27,
                "18446744073709551616",
                "number too large to fit in target type"
            ))
        );
    }
}
