use crate::syntax_kind::SyntaxKind;
use num_traits::{FromPrimitive, ToPrimitive};

#[derive(Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum ChimeraLanguage {}

pub type SyntaxNode = rowan::SyntaxNode<ChimeraLanguage>;
pub type SyntaxToken = rowan::SyntaxToken<ChimeraLanguage>;
pub type SyntaxElement = rowan::SyntaxElement<ChimeraLanguage>;

impl rowan::Language for ChimeraLanguage {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        Self::Kind::from_u16(raw.0).unwrap()
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        rowan::SyntaxKind(kind.to_u16().unwrap())
    }
}
