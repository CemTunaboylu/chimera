use std::ops::Range;

use smol_str::{SmolStr, ToSmolStr};
use syntax::{
    language::SyntaxNode,
    syntax_kind::SyntaxKind::{
        self, Eq, Ident, InfixBinOp, Mut, Tuple, TypeHint, VarRef as VarRefKind,
    },
};
use thin_vec::ThinVec;

use crate::{
    errors::ASTError,
    expression::Expr,
    lang_elems::{
        ensure_node_kind_is, first_child_of_kind_errs, get_children_in, get_token_of_errs,
    },
    types::{Type, parse_type_hinted},
};

#[derive(Clone, Debug, PartialEq)]
pub struct Spanned<A> {
    pub element: A,
    pub span: Range<usize>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Identifier {
    is_mut: bool,
    spanned_name: Spanned<SmolStr>,
    type_hint: Option<Type>,
}

impl Identifier {
    pub fn is_mut(&self) -> bool {
        self.is_mut
    }
    pub fn name(&self) -> &SmolStr {
        &self.spanned_name.element
    }
    pub fn span(&self) -> Range<usize> {
        self.spanned_name.span.clone()
    }

    pub fn type_hint(&self) -> Option<&Type> {
        self.type_hint.as_ref()
    }
}

impl TryFrom<&SyntaxNode> for Identifier {
    type Error = ASTError;

    fn try_from(node: &SyntaxNode) -> Result<Self, ASTError> {
        let identifier = match node.kind() {
            Mut => {
                let child_of_mut = first_child_of_kind_errs(&node, [VarRefKind].as_ref())?;
                let mut identifier = Self::try_from(&child_of_mut)?;
                identifier.is_mut = true;
                identifier
            }
            TypeHint => {
                let (hinted, ty) = parse_type_hinted(node)?;
                let is_mut = matches!(hinted, Expr::Mut(_));
                Self {
                    is_mut,
                    spanned_name: Spanned::<SmolStr> {
                        element: hinted.name().or(Some("".to_smolstr())).unwrap(),
                        span: node.text_range().into(),
                    },
                    type_hint: Some(ty),
                }
            }
            VarRefKind => {
                let var_ref = VarRef::try_from(node)?;
                Self {
                    is_mut: false,
                    spanned_name: Spanned::<SmolStr> {
                        element: var_ref.name().clone(),
                        span: var_ref.span(),
                    },
                    type_hint: None,
                }
            }
            _ => {
                return Err(ASTError::with_err_msg(
                    node.text_range().into(),
                    format!("expected a valid identifier but got {:?}", node.kind()),
                ));
            }
        };
        Ok(identifier)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Pattern {
    Ident(Identifier),
    Tuple((ThinVec<Pattern>, Range<usize>)),
}

impl Pattern {
    pub fn as_ident(&self) -> Option<&Identifier> {
        match self {
            Pattern::Ident(identifier) => Some(identifier),
            Pattern::Tuple(_) => None,
        }
    }

    pub fn as_reversed_tuple_pattern(&self) -> Option<ThinVec<Pattern>> {
        match self {
            Pattern::Ident(_) => None,
            Pattern::Tuple((patterns, _)) => {
                let mut clone = patterns.clone();
                clone.reverse();
                Some(clone)
            }
        }
    }
    fn lhs_kinds() -> [SyntaxKind; 4] {
        [Mut, Tuple, TypeHint, VarRefKind]
    }
    pub fn is_mut(&self) -> Option<bool> {
        match self {
            Pattern::Ident(identifier) => Some(identifier.is_mut()),
            Pattern::Tuple(_) => None,
        }
    }
    pub fn name(&self) -> Option<&SmolStr> {
        match self {
            Pattern::Ident(identifier) => Some(identifier.name()),
            Pattern::Tuple(_) => None,
        }
    }
    pub fn span(&self) -> Range<usize> {
        match self {
            Pattern::Ident(identifier) => identifier.span(),
            Pattern::Tuple((_, span)) => span.clone(),
        }
    }
    pub fn type_hint(&self) -> Option<&Type> {
        match self {
            Pattern::Ident(identifier) => identifier.type_hint(),
            Pattern::Tuple(_) => None,
        }
    }
}

impl TryFrom<&SyntaxNode> for Pattern {
    type Error = ASTError;

    fn try_from(node: &SyntaxNode) -> Result<Self, Self::Error> {
        if node.kind() == Tuple {
            let mut patterns = ThinVec::new();
            for child_node in get_children_in(node, Pattern::lhs_kinds().as_ref()) {
                let pattern = if child_node.kind() == Tuple {
                    Pattern::try_from(&child_node)?
                } else {
                    Pattern::Ident(Identifier::try_from(&child_node)?)
                };
                patterns.push(pattern);
            }
            return Ok(Pattern::Tuple((patterns, node.text_range().into())));
        }
        let identifier = Identifier::try_from(node)?;
        Ok(Pattern::Ident(identifier))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct LetBinding {
    pattern: Pattern,
    spanned_expr: Option<Spanned<Expr>>,
    span: Range<usize>,
}

impl LetBinding {
    pub fn pattern_span(&self) -> Range<usize> {
        self.pattern.span()
    }
    pub fn pattern(&self) -> &Pattern {
        &self.pattern
    }
    pub fn as_ident(&self) -> Option<&Identifier> {
        if let Pattern::Ident(ident) = &self.pattern {
            Some(ident)
        } else {
            None
        }
    }
    pub fn value(&self) -> Option<&Spanned<Expr>> {
        self.spanned_expr.as_ref()
    }
    pub fn span(&self) -> Range<usize> {
        self.span.clone()
    }
}

impl TryFrom<&SyntaxNode> for LetBinding {
    type Error = ASTError;

    fn try_from(let_binding_node: &SyntaxNode) -> Result<Self, Self::Error> {
        let child = first_child_of_kind_errs(let_binding_node, [InfixBinOp].as_ref())?;
        get_token_of_errs(&child, Eq)?;
        let lhs = first_child_of_kind_errs(&child, Pattern::lhs_kinds().as_ref())?;
        let pattern = Pattern::try_from(&lhs)?;

        let assignment = child.last_child().unwrap();
        let spanned_expr = Some(Spanned::<Expr> {
            element: Expr::try_from(&assignment)?,
            span: assignment.text_range().into(),
        });
        let span = let_binding_node.text_range().into();
        Ok(Self {
            pattern,
            spanned_expr,
            span,
        })
    }
}
#[derive(Clone, Debug, PartialEq)]
pub struct VarRef {
    pub name: SmolStr,
    pub span: Range<usize>,
}

impl TryFrom<&SyntaxNode> for VarRef {
    type Error = ASTError;

    fn try_from(var_ref_node: &SyntaxNode) -> Result<Self, Self::Error> {
        ensure_node_kind_is(var_ref_node, VarRefKind)?;
        let name = get_token_of_errs(var_ref_node, Ident)
            .unwrap()
            .text()
            .to_smolstr();
        let span = var_ref_node.text_range().into();
        Ok(Self { name, span })
    }
}

impl VarRef {
    pub fn name(&self) -> &SmolStr {
        &self.name
    }
    pub fn span(&self) -> Range<usize> {
        self.span.clone()
    }
}

#[cfg(test)]
mod tests {

    use syntax::syntax_kind::SyntaxKind;

    use super::*;
    use crate::{
        ast_root_from, cast_node_into_type,
        function::{FnArg, On},
        literal::Value,
    };
    use crate::{literal::Literal, types::Type};

    fn get_tuple_elements_from_expr(expr: &Expr) -> ThinVec<Expr> {
        match expr {
            Expr::Tuple(tuple) => tuple.elements().expect("should have elements"),
            _ => panic!("should be a tuple"),
        }
    }

    fn get_identifiers_from_tuple_pattern(pattern: &Pattern) -> ThinVec<Identifier> {
        match pattern {
            Pattern::Tuple((identifiers, _)) => identifiers
                .iter()
                .map(|id| id.as_ident().unwrap().clone())
                .collect(),
            _ => panic!("should be a tuple pattern"),
        }
    }

    fn match_varref_with(expr: &Expr, name: &str) -> bool {
        matches!(expr, Expr::VarRef(varref) if varref.name() == &name.to_smolstr())
    }

    fn match_call_with(expr: &Expr, on: On, args: ThinVec<FnArg>) -> bool {
        matches!(expr, Expr::Call(call) if call.on == on && call.arguments == args)
    }

    fn assert_expr_is_infix_with(
        expr: &Expr,
        op: SyntaxKind,
        lhs_check: fn(&Expr) -> bool,
        rhs_check: fn(&Expr) -> bool,
    ) {
        let infix = match expr {
            Expr::Infix(bin) => bin,
            _ => panic!("should be a tuple"),
        };
        assert_eq!(infix.op(), Some(op));
        assert!(lhs_check(infix.lhs().unwrap()));
        assert!(rhs_check(infix.rhs().unwrap()));
    }

    fn assert_ident_in_tuple(
        identifiers: &ThinVec<Identifier>,
        at: usize,
        name: &str,
        is_mut: bool,
        type_hint: Option<&Type>,
    ) {
        assert_eq!(identifiers[at].name(), name);
        assert_eq!(identifiers[at].is_mut(), is_mut);
        assert_eq!(identifiers[at].type_hint(), type_hint);
    }

    #[test]
    fn valid_var_ref() {
        let program = "var";
        let ast_root = ast_root_from(program);
        let var_ref =
            cast_node_into_type::<VarRef>(ast_root.get_root().first_child().as_ref().unwrap());
        assert_eq!(program, var_ref.name().as_str());
    }

    #[test]
    fn let_binding_with_tuple_pattern() {
        let program = "let (mut p1_norm : Point, p2_norm : Point) = (point_1.normalize(), point_2.normalize());";
        let ast_root = ast_root_from(program);
        let let_binding_node = ast_root.get_root().first_child().unwrap();
        let let_binding = cast_node_into_type::<LetBinding>(&let_binding_node);
        let identifiers = get_identifiers_from_tuple_pattern(let_binding.pattern());
        assert_eq!(identifiers.len(), 2);
        assert_ident_in_tuple(
            &identifiers,
            0,
            "p1_norm",
            true,
            Some(&Type::Struct(SmolStr::from("Point"))),
        );
        assert_ident_in_tuple(
            &identifiers,
            1,
            "p2_norm",
            false,
            Some(&Type::Struct(SmolStr::from("Point"))),
        );
        let expr = &let_binding.value().unwrap().element;
        let elements = get_tuple_elements_from_expr(expr);
        assert_eq!(elements.len(), 2);
        assert_expr_is_infix_with(
            &elements[0],
            SyntaxKind::Dot,
            |lhs| match_varref_with(lhs, "point_1"),
            |rhs| match_call_with(rhs, On::Binding(SmolStr::from("normalize")), ThinVec::new()),
        );
        assert_expr_is_infix_with(
            &elements[1],
            SyntaxKind::Dot,
            |lhs| match_varref_with(lhs, "point_2"),
            |rhs| match_call_with(rhs, On::Binding(SmolStr::from("normalize")), ThinVec::new()),
        );
    }

    #[test]
    fn let_binding_with_nested_tuple_pattern() {
        let program = "let ((mut p1_norm : Point, p2_norm : Point), mut distance: i32) = ((point_1.normalize(), point_2.normalize()), 100);";
        let ast_root = ast_root_from(program);
        let let_binding_node = ast_root.get_root().first_child().unwrap();
        let let_binding = cast_node_into_type::<LetBinding>(&let_binding_node);
        let (nested_tuple_pattern, _) = match let_binding.pattern() {
            Pattern::Tuple(patterns) => patterns,
            _ => panic!("should be a tuple pattern"),
        };
        let identifiers = get_identifiers_from_tuple_pattern(&nested_tuple_pattern[0]);
        assert_eq!(identifiers.len(), 2);
        assert_ident_in_tuple(
            &identifiers,
            0,
            "p1_norm",
            true,
            Some(&Type::Struct(SmolStr::from("Point"))),
        );
        assert_ident_in_tuple(
            &identifiers,
            1,
            "p2_norm",
            false,
            Some(&Type::Struct(SmolStr::from("Point"))),
        );
        let identifier = &nested_tuple_pattern[1];
        assert_eq!(
            identifier,
            &Pattern::Ident(Identifier {
                is_mut: true,
                spanned_name: Spanned::<SmolStr> {
                    element: "distance".to_smolstr(),
                    span: 45..62,
                },
                type_hint: Some(Type::Integer32),
            })
        );

        let expr = &let_binding.value().unwrap().element;
        let tuple_elems = get_tuple_elements_from_expr(expr);
        let nested_tuple_values = get_tuple_elements_from_expr(&tuple_elems[0]);
        assert_eq!(nested_tuple_values.len(), 2);
        assert_expr_is_infix_with(
            &nested_tuple_values[0],
            SyntaxKind::Dot,
            |lhs| match_varref_with(lhs, "point_1"),
            |rhs| match_call_with(rhs, On::Binding(SmolStr::from("normalize")), ThinVec::new()),
        );
        assert_expr_is_infix_with(
            &nested_tuple_values[1],
            SyntaxKind::Dot,
            |lhs| match_varref_with(lhs, "point_2"),
            |rhs| match_call_with(rhs, On::Binding(SmolStr::from("normalize")), ThinVec::new()),
        );
        let hundred = tuple_elems[1].clone();
        assert_eq!(hundred, Expr::Literal(Literal(Value::Int(100))));
    }

    #[test]
    fn let_binding() {
        let program = "let diff_norm : Point = (point_1.locus() - point_2.locus()).normalize();";
        let ast_root = ast_root_from(program);
        let let_binding_node = ast_root.get_root().first_child().unwrap();
        let let_binding = cast_node_into_type::<LetBinding>(&let_binding_node);
        assert_eq!("diff_norm", let_binding.pattern().name().unwrap().as_str());
        assert_eq!(
            &Type::Struct(SmolStr::from("Point")),
            let_binding
                .pattern()
                .type_hint()
                .expect("should have a type hint")
        );
        assert!(!let_binding.pattern().is_mut().unwrap());
    }

    #[test]
    fn mut_let_binding() {
        let program = "let mut diff_norm = (point_1.locus() - point_2.locus()).normalize();";
        let ast_root = ast_root_from(program);
        let let_binding_node = ast_root.get_root().first_child().unwrap();
        let let_binding = cast_node_into_type::<LetBinding>(&let_binding_node);
        assert_eq!("diff_norm", let_binding.pattern().name().unwrap().as_str());
        assert!(let_binding.pattern().is_mut().unwrap());
    }
}
