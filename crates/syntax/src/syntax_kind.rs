use lexer::token_kind::TokenKind;
use num_derive::{FromPrimitive, ToPrimitive};
use thin_vec::{ThinVec, thin_vec};

use crate::{RestrictionType, bitset::SyntaxKindBitSet, non_assigning_operators};

#[derive(Copy, Clone, Debug, FromPrimitive, Eq, Hash, Ord, PartialEq, PartialOrd, ToPrimitive)]
pub enum SyntaxKind {
    // recovering
    Recovered,
    // required for CST
    Root,
    // token-tree roots for n-ary operators
    InfixBinOp,
    PrefixUnaryOp,
    PostfixUnaryOp,
    // token-tree roots for expressions/sub-expressions
    BufferLit,
    Call,
    CharLit,
    DimHints,
    DimValue,
    ImplBlock,
    In,
    Jump,
    Literal,
    Mut,
    ParamDecl,
    ParenExpr,
    RetType,
    Return,
    SelfRef,
    Semi,
    StrLit,
    StructAsType,
    StructField,
    StructLit,
    StructRef,
    TensorLit,
    Tuple,
    Unit,
    TypeHint,
    VarRef,
    // token-tree roots for statements
    Block,
    Condition,
    Conditional,
    ContainerRef,
    ControlFlow,
    FnArg,
    FnDef,
    Lambda,
    ForIdent,
    ForLoop,
    Generic,
    Indexing,
    StructDef,
    LetBinding,
    WhileLoop,
    // mirrored
    And,
    AndAnd,
    AndEq,
    Colon,
    ColonColon,
    Comma,
    Comment,
    Dot,
    Eq,
    EqEq,
    Excl,
    Float,
    Gt,
    Ge,
    Ident,
    Int,
    KwBreak,
    KwBuffer,
    KwConst,
    KwContinue,
    KwElif,
    KwElse,
    KwEnum,
    KwFalse,
    KwFn,
    KwFor,
    KwIf,
    KwImport,
    KwImpl,
    KwIn,
    KwLet,
    KwMatch,
    KwModule,
    KwMut,
    KwReturn,
    KwSelf,
    KwStruct,
    KwTensor,
    KwTrue,
    KwType,
    KwWhile,
    Kwself,
    LBrace,
    LParen,
    LShift,
    LShiftEq,
    // arr[<expr>] -> [<expr>] is actually a 'composite' postfix operator
    LBrack,
    Lt,
    Le,
    Minus,
    MinusEq,
    NotEq,
    NullTerm,
    Or,
    OrEq,
    OrOr,
    Percent,
    PercentEq,
    Plus,
    PlusEq,
    QMark,
    RArrow,
    RBrace,
    RParen,
    RShift,
    RShiftEq,
    RBrack,
    Slash,
    SlashEq,
    Star,
    StarEq,
    TyBool,
    TyBuffer,
    TyChar,
    TyF32,
    TyFn, // this is a composite type i.e. a node not a token
    TyI32,
    TyStr,
    TyTensor,
    Under,
    Whitespace,
    Xor,
}

pub fn from_token_kinds(token_kinds: ThinVec<TokenKind>) -> ThinVec<SyntaxKind> {
    token_kinds
        .iter()
        .map(|token_kind| SyntaxKind::from(*token_kind))
        .collect::<ThinVec<_>>()
}
impl SyntaxKind {
    pub fn is_delimiter(&self) -> bool {
        use SyntaxKind::*;
        matches!(self, LBrace | LParen | LBrack | RBrace | RParen | RBrack)
    }

    pub fn opening_delimiters() -> ThinVec<SyntaxKind> {
        use SyntaxKind::*;
        thin_vec![LBrace, LParen, LBrack]
    }

    pub fn closing_delimiters() -> ThinVec<SyntaxKind> {
        use SyntaxKind::*;
        thin_vec![RBrace, RParen, RBrack]
    }
    pub fn is_opening_delimiter(&self) -> bool {
        use SyntaxKind::*;
        matches!(self, LBrace | LParen | LBrack)
    }
    pub fn is_closing_delimiter(&self) -> bool {
        use SyntaxKind::*;
        matches!(self, RBrace | RParen | RBrack)
    }
    // TODO: a closure is a literal value?
    pub fn is_literal_value(&self) -> bool {
        use SyntaxKind::*;
        matches!(
            self,
            BufferLit | CharLit | Float | Int | KwFalse | KwTrue | StrLit | Unit
        )
    }

    pub fn types() -> ThinVec<SyntaxKind> {
        use SyntaxKind::*;
        thin_vec![
            KwBuffer, // allows parsing buffer<dim><type> as a type
            KwFn,     // allows parsing function declaration as a type
            KwTensor, // allows parsing tensor<dim><type> as a type
            StructAsType,
            TyBool,
            TyBuffer,
            TyChar,
            TyF32,
            TyFn,
            TyI32,
            TyStr,
            TyTensor,
            Unit,
        ]
    }

    pub fn can_be_parameter() -> ThinVec<SyntaxKind> {
        use SyntaxKind::*;
        thin_vec![
            Ident,
            KwBuffer, // allows parsing buffer<dim><type> as a type
            KwFn,     // allows parsing function declaration as a type
            KwTensor, // allows parsing tensor<dim><type> as a type
            SelfRef,
            StructAsType,
            TyBool,
            TyBuffer,
            TyChar,
            TyF32,
            TyFn,
            TyI32,
            TyStr,
            TyTensor,
        ]
    }

    pub fn is_keyword(&self) -> bool {
        use SyntaxKind::*;
        &KwBreak <= self && self <= &Kwself
    }

    pub fn is_binary_operator(&self) -> bool {
        use SyntaxKind::*;
        (self >= &And && self <= &ColonColon)
            || (self >= &Dot && self <= &Excl)
            || (self >= &Lt && self <= &NotEq)
            || (self >= &Or && self <= &QMark)
            || (self >= &Slash && self <= &StarEq)
            || matches!(
                self,
                Gt | Ge | LShift | LShiftEq | RShift | RShiftEq | Under | Xor
            )
    }

    pub fn is_unary_operator(&self) -> bool {
        use SyntaxKind::*;
        matches!(self, And | Minus | Excl | Star | QMark)
    }

    pub fn is_prefix_unary_operator(&self) -> bool {
        use SyntaxKind::*;
        matches!(self, And | Minus | Excl | Star)
    }

    pub fn is_posfix_unary_operator(&self) -> bool {
        use SyntaxKind::*;
        matches!(self, QMark)
    }

    pub fn operators() -> ThinVec<Self> {
        use SyntaxKind::*;
        thin_vec![
            And, AndAnd, AndEq, Colon, ColonColon, Dot, Eq, EqEq, Excl, Gt, Ge, LShift, LShiftEq,
            Lt, Le, Minus, MinusEq, NotEq, Or, OrEq, OrOr, Percent, PercentEq, Plus, PlusEq, QMark,
            RArrow, RShift, RShiftEq, Slash, SlashEq, Star, StarEq, Under, Xor,
        ]
    }

    pub fn prefix_unary_operators() -> ThinVec<Self> {
        use SyntaxKind::*;
        thin_vec![And, Minus, Excl, Star]
    }
    pub fn assignments() -> ThinVec<Self> {
        use SyntaxKind::*;
        thin_vec![
            AndEq, Eq, LShiftEq, MinusEq, NotEq, OrEq, PercentEq, PlusEq, RShiftEq, SlashEq,
            StarEq,
        ]
    }

    pub fn is_trivia(self) -> bool {
        use SyntaxKind::*;
        matches!(self, Whitespace | Comment | NullTerm)
    }

    pub fn imposed_restrictions(&self) -> [RestrictionType; 4] {
        use SyntaxKind::*;
        let mut context_update = [RestrictionType::None; 4];
        match self {
            Block => {
                context_update[1] = RestrictionType::Add([RBrace].as_ref().into());
                context_update[2] = RestrictionType::Sub([RBrace, StructAsType].as_ref().into());
            }
            BufferLit | TensorLit => {
                context_update[1] = RestrictionType::Add([Semi, RBrack].as_ref().into());
                context_update[2] = RestrictionType::Sub([RBrack].as_ref().into());
            }
            Call => {
                let operators: SyntaxKindBitSet = SyntaxKind::operators().as_ref().into();
                context_update[2] = RestrictionType::Add(operators);
            }
            Condition => {
                context_update[1] = RestrictionType::Add([LBrace, KwElif, KwElse].as_ref().into());
                let operators: SyntaxKindBitSet = SyntaxKind::operators().as_ref().into();
                let booleans_and_paren: SyntaxKindBitSet =
                    [KwTrue, KwFalse, LParen].as_ref().into();
                context_update[2] = RestrictionType::Override(operators + booleans_and_paren);
            }
            DimHints => {
                context_update[1] = RestrictionType::Add([Gt].as_ref().into());
                context_update[2] = RestrictionType::Override([].as_ref().into());
            }
            ForLoop => {
                context_update[1] = RestrictionType::Add([KwIn, LBrace].as_ref().into());
            }
            FnArg => {
                context_update[2] = RestrictionType::Add([RParen, StructAsType].as_ref().into());
            }
            Gt => {
                context_update[1] = RestrictionType::Add([Gt].as_ref().into());
                context_update[2] = RestrictionType::Sub([Gt].as_ref().into());
            }
            Ident => {
                context_update[1] = RestrictionType::Add([LParen, LBrace, LBrack].as_ref().into());
                context_update[2] = RestrictionType::Sub([LParen, LBrace, LBrack].as_ref().into());
            }
            // Note: Prevents for i in arr{} to be parsed as ..., StructLit (arr{})
            In => context_update[2] = RestrictionType::Sub(StructLit.into()),
            Indexing => {
                let non_assigning_operators: SyntaxKindBitSet = non_assigning_operators();
                context_update[2] = RestrictionType::Add(non_assigning_operators);
            }
            Jump | Return => {
                context_update[0] = RestrictionType::Add(Semi.into());
            }
            Lambda => {
                context_update[1] = RestrictionType::Add([LBrace, RBrace].as_ref().into());
            }
            LBrace => {
                context_update[1] = RestrictionType::Add([LBrace, RBrace].as_ref().into());
            }
            LBrack => {
                context_update[1] = RestrictionType::Add([LBrack, RBrack].as_ref().into());
            }
            ParenExpr => {
                context_update[1] = RestrictionType::Add([Comma, RBrace].as_ref().into());
                context_update[2] = RestrictionType::Sub([Comma].as_ref().into());
            }
            ParamDecl => {
                let misc: SyntaxKindBitSet = [And, Colon, Comma, Ident, KwMut, LParen, RParen]
                    .as_ref()
                    .into();
                let types: SyntaxKindBitSet = SyntaxKind::types().as_ref().into();
                context_update[2] = RestrictionType::Override(misc + types);
            }
            PrefixUnaryOp => {
                context_update[2] =
                    RestrictionType::Add(SyntaxKind::prefix_unary_operators().as_ref().into());
            }
            RBrack => {
                context_update[1] = RestrictionType::Add([LBrack, RBrack].as_ref().into());
            }
            RetType => {
                context_update[0] = RestrictionType::Add([StructAsType].as_ref().into());
                let types: SyntaxKindBitSet = SyntaxKind::types().as_ref().into();
                let ref_mut_struct_as_type: SyntaxKindBitSet =
                    [And, Mut, StructAsType].as_ref().into();
                context_update[2] = RestrictionType::Add(types + ref_mut_struct_as_type);
            }
            RParen => {
                context_update[1] = RestrictionType::Add([LParen, RParen].as_ref().into());
            }
            TypeHint => {
                context_update[1] = RestrictionType::Add([Gt].as_ref().into());
                context_update[2] = RestrictionType::Sub([Gt].as_ref().into());
            }
            LetBinding => {
                // ! FIXME: this is a hack, I need to find a better way to expect things IN ORDER
                context_update[0] = RestrictionType::Add([Eq, Ident, Semi].as_slice().into());
                let non_assignments: SyntaxKindBitSet = non_assigning_operators();
                let can_be_parameter: SyntaxKindBitSet =
                    SyntaxKind::can_be_parameter().as_ref().into();
                let opening_delimiters: SyntaxKindBitSet =
                    SyntaxKind::opening_delimiters().as_ref().into();
                let exceptionals: SyntaxKindBitSet = [Eq, Ident, Semi, VarRef, SelfRef, StructLit]
                    .as_ref()
                    .into();
                context_update[2] = RestrictionType::Override(
                    non_assignments + can_be_parameter + opening_delimiters + exceptionals,
                );
            }
            WhileLoop => {
                context_update[1] = RestrictionType::Add([LBrace, RBrace].as_ref().into());
                context_update[2] = RestrictionType::Add([KwTrue, KwFalse, LParen].as_ref().into());
            }
            _ => {}
        }
        context_update
    }
}

impl From<TokenKind> for SyntaxKind {
    fn from(token_kind: TokenKind) -> Self {
        use TokenKind::*;
        match token_kind {
            And => Self::And,
            AndAnd => Self::AndAnd,
            AndEq => Self::AndEq,
            BlockComment | LineComment => Self::Comment,
            CharLiteral => Self::CharLit,
            Colon => Self::Colon,
            ColonColon => Self::ColonColon,
            Comma => Self::Comma,
            Dot => Self::Dot,
            Eq => Self::Eq,
            EqEq => Self::EqEq,
            Exclamation => Self::Excl,
            Float => Self::Float,
            GreaterThan => Self::Gt,
            GreaterThanOrEq => Self::Ge,
            Identifier => Self::Ident,
            Integer => Self::Int,
            KwBuffer => Self::KwBuffer,
            KwBreak => Self::KwBreak,
            KwConst => Self::KwConst,
            KwContinue => Self::KwContinue,
            KwElif => Self::KwElif,
            KwElse => Self::KwElse,
            KwEnum => Self::KwEnum,
            KwFalse => Self::KwFalse,
            KwFn => Self::KwFn,
            KwFor => Self::KwFor,
            KwIf => Self::KwIf,
            KwImport => Self::KwImport,
            KwImpl => Self::KwImpl,
            KwIn => Self::KwIn,
            KwLet => Self::KwLet,
            KwMatch => Self::KwMatch,
            KwModule => Self::KwModule,
            KwMut => Self::KwMut,
            KwReturn => Self::KwReturn,
            KwSelf => Self::KwSelf,
            KwStruct => Self::KwStruct,
            KwTensor => Self::KwTensor,
            KwTrue => Self::KwTrue,
            KwType => Self::KwType,
            KwWhile => Self::KwWhile,
            Kwself => Self::Kwself,
            LeftBrace => Self::LBrace,
            LeftParen => Self::LParen,
            LeftShift => Self::LShift,
            LeftShiftEq => Self::LShiftEq,
            LeftSquareBrac => Self::LBrack,
            LessThan => Self::Lt,
            LessThanOrEq => Self::Le,
            Minus => Self::Minus,
            MinusEq => Self::MinusEq,
            NotEq => Self::NotEq,
            NullTerminator => Self::NullTerm,
            Or => Self::Or,
            OrEq => Self::OrEq,
            OrOr => Self::OrOr,
            Percent => Self::Percent,
            PercentEq => Self::PercentEq,
            Plus => Self::Plus,
            PlusEq => Self::PlusEq,
            QuestionMark => Self::QMark,
            RightArrow => Self::RArrow,
            RightBrace => Self::RBrace,
            RightParen => Self::RParen,
            RightShift => Self::RShift,
            RightShiftEq => Self::RShiftEq,
            RightSquareBrac => Self::RBrack,
            Root => SyntaxKind::Root,
            SemiColon => Self::Semi,
            Slash => Self::Slash,
            SlashEq => Self::SlashEq,
            Space | Newline | Tab => Self::Whitespace,
            Star => Self::Star,
            StarEq => Self::StarEq,
            StringLiteral => Self::StrLit,
            TypeBool => Self::TyBool,
            TypeChar => Self::TyChar,
            TypeF32 => Self::TyF32,
            TypeI32 => Self::TyI32,
            TypeStr => Self::TyStr,
            Underscore => Self::Under,
            Xor => Self::Xor,
            _ => {
                let msg = format!("{:?} don't have a corresponding syntaxkind", token_kind);
                panic!("{}", msg)
            }
        }
    }
}
