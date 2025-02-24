#![allow(dead_code)]
use std::ops::Range;

use miette::{Diagnostic, Report};
use smol_str::{SmolStr, ToSmolStr};
use syntax::{
    language::{SyntaxElement, SyntaxNode, SyntaxToken},
    syntax::SyntaxKind,
};
use thiserror::Error;

use crate::errors::{Inner, Stringer};

// AST is a pseudo abstract syntax tree because it is built on
// top of the CST that's constructed by rowan.

#[derive(Clone, Diagnostic, Debug, PartialEq, Error)]
pub enum ASTError {
    #[error(transparent)]
    #[diagnostic(transparent)]
    Unexpected(#[from] Inner),
}

type SrcTextAndErrSpan = (String, Range<usize>);

fn get_src_and_span_from_node(node: &SyntaxNode) -> SrcTextAndErrSpan {
    (node.text().to_string(), node.text_range().into())
}

fn get_src_and_span_from_token(token: &SyntaxToken) -> SrcTextAndErrSpan {
    (token.text().to_string(), token.text_range().into())
}

impl ASTError {
    fn for_src_and_err_span(
        src_and_span: SrcTextAndErrSpan,
        got: SyntaxKind,
        exp: impl Stringer,
    ) -> Self {
        let inner = Inner::new(src_and_span.0, src_and_span.1, exp, got);
        Self::Unexpected(inner)
    }

    fn for_expr(expr: Expr, exp: impl Stringer) -> Self {
        let src_and_span = expr.get_src_and_span();
        let inner = Inner::new(src_and_span.0, src_and_span.1, exp, expr);
        Self::Unexpected(inner)
    }
}

#[derive(Debug)]
pub struct Root(SyntaxNode);

impl Root {
    pub fn statements(&self) -> impl Iterator<Item = Stmt> {
        self.0.children().filter_map(try_from_opted::<Stmt>)
    }
}

impl TryFrom<SyntaxNode> for Root {
    type Error = ASTError;

    fn try_from(node: SyntaxNode) -> Result<Self, Self::Error> {
        if SyntaxKind::Root == node.kind() {
            Ok(Self(node))
        } else {
            let kind = node.kind();
            Err(ASTError::for_src_and_err_span(
                get_src_and_span_from_node(&node),
                kind,
                SyntaxKind::Root,
            ))
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
    fn get_src_and_span(&self) -> SrcTextAndErrSpan {
        let node = self.get_syntax_node();
        get_src_and_span_from_node(node)
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
            return Err(ASTError::for_src_and_err_span(
                get_src_and_span_from_node(&node),
                node.kind(),
                "non-None and Expr::VarRef",
            ));
        }
        let infix = infix.unwrap();
        let mut exprs = infix.children().filter_map(try_from_opted::<Expr>);
        let name_as_var_ref = exprs.next();
        let var_ref = if name_as_var_ref.is_none() {
            return Err(ASTError::for_src_and_err_span(
                get_src_and_span_from_node(&infix),
                infix.kind(),
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

    fn get_src_and_span(&self) -> SrcTextAndErrSpan {
        get_src_and_span_from_node(&self.0)
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

impl LiteralValue {
    pub fn value(&self) -> Value {
        match self {
            LiteralValue::Char(syntax_token) => Value::Char(
                syntax_token
                    .text()
                    .parse::<char>()
                    .expect("should have been a char dammit"),
            ),
            LiteralValue::Str(syntax_token) => Value::Str(syntax_token.text().to_smolstr()),
            LiteralValue::Num(syntax_token) => Value::Num(
                syntax_token
                    .text()
                    .to_string()
                    .parse::<usize>()
                    .expect("should have been parsable to uisize dammit"),
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
                return Err(ASTError::for_src_and_err_span(
                    get_src_and_span_from_token(&node),
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
    pub fn value(&self) -> Value {
        self.0.value()
    }

    fn get_src_and_span(&self) -> SrcTextAndErrSpan {
        let token = match &self.0 {
            LiteralValue::Char(syntax_token) => syntax_token,
            LiteralValue::Str(syntax_token) => syntax_token,
            LiteralValue::Num(syntax_token) => syntax_token,
        };
        get_src_and_span_from_token(token)
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

        let literal_value = LiteralValue::try_from(node_containing_value)
            .expect("child should have been convertible to LiteralValue");

        Ok(Self(literal_value))
    }
}

#[derive(Debug)]
pub struct Paren(SyntaxNode);

impl Paren {
    pub fn expr(&self) -> Option<Expr> {
        self.0.children().find_map(try_from_opted::<Expr>)
    }

    fn get_src_and_span(&self) -> SrcTextAndErrSpan {
        get_src_and_span_from_node(&self.0)
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
    fn get_src_and_span(&self) -> SrcTextAndErrSpan {
        let node = self.get_syntax_node();
        get_src_and_span_from_node(node)
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
            let report: Report = ast_err.into();
            println!("{:?}", report);
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
    fn get_src_and_span(&self) -> SrcTextAndErrSpan {
        match self {
            Expr::Infix(infix) => infix.get_src_and_span(),
            Expr::Unary(unary) => unary.get_src_and_span(),
            Expr::Paren(paren) => paren.get_src_and_span(),
            Expr::VarRef(var_ref) => var_ref.get_src_and_span(),
            Expr::Literal(literal) => literal.get_src_and_span(),
        }
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
                return Err(ASTError::for_src_and_err_span(
                    get_src_and_span_from_node(&node),
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
            SyntaxKind::VariableDef => {
                let v = VarDef::try_from(node)?;
                Ok(Self::VarDef(v))
            }
            _ => {
                let e = Expr::try_from(node)?;
                Ok(Self::Expr(e))
            }
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
}
