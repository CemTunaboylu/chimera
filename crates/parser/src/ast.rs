#![allow(dead_code)]
use rowan::SyntaxText;
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
       - add test for let a = let b = let c

*/

/*
→ let a = b
CST:
    Root@0..6
    VariableDef@0..6
        LetKw@0..3 "let"
        Identifier@3..4 "a"
        Eq@4..5 "="
        VariableRef@5..6
        Identifier@5..6 "b"
AST:
    VarDef(
        VarDef(
            VariableDef@0..6
              LetKw@0..3 "let"
              Identifier@3..4 "a"
              Eq@4..5 "="
              VariableRef@5..6
                Identifier@5..6 "b"
            ,
        ),
    ),

→ let a = 3 + 14
CST:
    Root@0..9
    VariableDef@0..9
        LetKw@0..3 "let"
        Identifier@3..4 "a"
        Eq@4..5 "="
        InfixBinaryOp@5..9
        Literal@5..6
            Number@5..6 "3"
        Plus@6..7 "+"
        Literal@7..9
            Number@7..9 "14"
AST:
    VarDef(
            VarDef(
                VariableDef@0..9
                LetKw@0..3 "let"
                Identifier@3..4 "a"
                Eq@4..5 "="
                InfixBinaryOp@5..9
                    Literal@5..6
                    Number@5..6 "3"
                    Plus@6..7 "+"
                    Literal@7..9
                    Number@7..9 "14"
                ,
            ),
        ),
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

#[derive(Debug)]
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
        self.0.first_child().unwrap().text().to_smolstr()
    }
}

#[derive(Debug)]
pub struct Literal(SyntaxNode);

impl Literal {
    pub fn value<As: From<SyntaxText>>(&self) -> As {
        As::from(self.0.first_child().unwrap().text())
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
            SyntaxKind::Literal => Self::Literal(Literal(node)),
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
