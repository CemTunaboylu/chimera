#![allow(dead_code)]
use smol_str::{SmolStr, ToSmolStr};
use syntax::{
    language::{SyntaxElement, SyntaxNode, SyntaxToken},
    syntax::SyntaxKind,
};

// AST is a pseudo abstract syntax tree because it is built on
// top of the CST that's constructed by rowan.

/*
   TODO:
       - maybe I can merge VarRef and VarDef into a single enum
*/

#[derive(Debug)]
pub struct ASTError {}

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
            Err(ASTError {})
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
pub struct VarDef(SyntaxNode);

impl VarDef {
    pub fn name(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|syntax_token| SyntaxKind::Identifier == syntax_token.kind())
    }

    pub fn value(&self) -> Option<Expr> {
        self.0.children().find_map(try_from_opted::<Expr>)
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
}

#[derive(Clone, Debug)]
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
            _ => return Err(ASTError {}),
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

fn try_from_opted<S: TryFrom<SyntaxNode>>(node: SyntaxNode) -> Option<S> {
    S::try_from(node).ok()
}
#[derive(Debug)]
pub enum Expr {
    Infix(Infix),
    Literal(Literal),
    Paren(Paren),
    Unary(Unary),
    VarRef(VarRef),
}

impl TryFrom<SyntaxNode> for Expr {
    type Error = ASTError;

    fn try_from(node: SyntaxNode) -> Result<Self, Self::Error> {
        let result = match node.kind() {
            SyntaxKind::InfixBinaryOp => Self::Infix(Infix::Binary(node)),
            SyntaxKind::Literal => Self::Literal(try_from_opted::<Literal>(node).unwrap()),
            SyntaxKind::ParenExpr => Self::Paren(Paren(node)),
            SyntaxKind::PrefixUnaryOp => Self::Unary(Unary::Prefix(node)),
            SyntaxKind::VariableRef => Self::VarRef(VarRef(node)),
            _ => return Err(ASTError {}),
        };

        Ok(result)
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
            SyntaxKind::VariableDef => Ok(Self::VarDef(VarDef(node))),
            _ => {
                let e = Expr::try_from(node)?;
                Ok(Self::Expr(e))
            }
        }
    }
}
