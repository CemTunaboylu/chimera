use std::ops::Sub;

use lazy_static::lazy_static;
use lexer::token_kind::TokenKind;
use num_derive::{FromPrimitive, ToPrimitive};
use num_traits::{FromPrimitive, ToPrimitive};
use thin_vec::{ThinVec, thin_vec};

use crate::{RestrictionType, bitset::SyntaxKindBitSet};

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

use SyntaxKind::*;

pub static CAN_BE_PARAMETERS: &[SyntaxKind] = &[
    Ident,
    KwBuffer, // allows parsing buffer<dim><type> as a type
    KwFn,     // allows parsing function declaration as a type
    KwTensor, // allows parsing tensor<dim><type> as a type
    SelfRef,
    StructAsType,
    Tuple,
    TyBool,
    TyBuffer,
    TyChar,
    TyF32,
    TyFn,
    TyI32,
    TyStr,
    TyTensor,
];

pub static ASSIGNMENTS: &[SyntaxKind] = &[
    AndEq, Eq, LShiftEq, MinusEq, NotEq, OrEq, PercentEq, PlusEq, RShiftEq, SlashEq, StarEq,
];
pub static CLOSING_DELIMITERS: &[SyntaxKind] = &[RBrace, RParen];
pub static OPENING_DELIMITERS: &[SyntaxKind] = &[LBrace, LParen];
pub static OPERATORS: &[SyntaxKind] = &[
    And, AndAnd, AndEq, Colon, ColonColon, Dot, Eq, EqEq, Excl, Gt, Ge, LBrack, LShift, LShiftEq,
    Lt, Le, Minus, MinusEq, NotEq, Or, OrEq, OrOr, Percent, PercentEq, Plus, PlusEq, QMark, RArrow,
    RShift, RShiftEq, Slash, SlashEq, Star, StarEq, Under, Xor,
];
pub static PREFIX_OPERATORS: &[SyntaxKind] = &[And, Excl, Minus, Star];
pub static TYPES: &[SyntaxKind] = &[
    KwBuffer, // allows parsing buffer<dim><type> as a type
    KwFn,     // allows parsing function declaration as a type
    KwTensor, // allows parsing tensor<dim><type> as a type
    StructAsType,
    Tuple,
    TyBool,
    TyBuffer,
    TyChar,
    TyF32,
    TyFn,
    TyI32,
    TyStr,
    TyTensor,
    Unit,
];

lazy_static! {
    static ref ASSIGNMENTS_SET: SyntaxKindBitSet = SyntaxKind::assignments().as_ref().into();
    static ref BINARY_OPERATORS_SET: SyntaxKindBitSet = {
        use SyntaxKind::*;
        let ranges = [
            (And, ColonColon),
            (Dot, Excl),
            (Lt, NotEq),
            (Or, QMark),
            (Slash, StarEq),
        ];
        let mut set = SyntaxKindBitSet::empty();
        for (start, end) in ranges {
            for kind in start.to_u16().unwrap()..=end.to_u16().unwrap() {
                let kind: SyntaxKind = SyntaxKind::from_u16(kind).unwrap();
                set += kind.into();
            }
        }

        set += [Gt, Ge, LShift, LShiftEq, RShift, RShiftEq, Under, Xor]
            .as_ref()
            .into();
        set
    };
    static ref CAN_BE_PARAMETERS_SET: SyntaxKindBitSet = CAN_BE_PARAMETERS.into();
    static ref CLOSING_DELIMITERS_SET: SyntaxKindBitSet = CLOSING_DELIMITERS.as_ref().into();
    static ref DELIMITERS: SyntaxKindBitSet = [LBrace, LParen, RBrace, RParen].as_ref().into();
    static ref LITERALS: SyntaxKindBitSet = [
        BufferLit, CharLit, Float, Int, KwFalse, KwTrue, StrLit, Unit
    ]
    .as_ref()
    .into();
    static ref NON_ASSIGNING_OPERATORS: SyntaxKindBitSet = {
        let a: SyntaxKindBitSet = SyntaxKind::assignments().as_ref().into();
        let o: SyntaxKindBitSet = SyntaxKind::operators().as_ref().into();
        o - a
    };
    static ref OPENING_DELIMITERS_SET: SyntaxKindBitSet = OPENING_DELIMITERS.as_ref().into();
    static ref OPERATORS_SET: SyntaxKindBitSet = SyntaxKind::operators().as_ref().into();
    static ref TYPES_SET: SyntaxKindBitSet = TYPES.into();
    static ref TRIVIA_SET: SyntaxKindBitSet = [Whitespace, Comment, NullTerm].as_ref().into();
    static ref UNARY_PREFIX_OPERATORS_SET: SyntaxKindBitSet =
        [And, Minus, Excl, Star].as_ref().into();
    static ref UNARY_POSTFIX_OPERATORS_SET: SyntaxKindBitSet = [QMark, LBrack].as_ref().into();
}

impl SyntaxKind {
    #[inline]
    pub fn can_be_a_parameter(self) -> bool {
        CAN_BE_PARAMETERS_SET.contains(self)
    }
    #[inline]
    pub fn is_binary_operator(self) -> bool {
        BINARY_OPERATORS_SET.contains(self)
    }
    #[inline]
    pub fn is_assignment(self) -> bool {
        ASSIGNMENTS_SET.contains(self)
    }
    #[inline]
    pub fn is_operator(self) -> bool {
        OPERATORS_SET.contains(self)
    }
    #[inline]
    pub fn is_type(self) -> bool {
        TYPES_SET.contains(self)
    }
    #[inline]
    pub fn is_closing_delimiter(self) -> bool {
        CLOSING_DELIMITERS_SET.contains(self)
    }
    #[inline]
    pub fn is_delimiter(self) -> bool {
        DELIMITERS.contains(self)
    }
    #[inline]
    pub fn is_keyword(self) -> bool {
        KwBreak <= self && self <= Kwself
    }
    // TODO: a closure is a literal value?
    #[inline]
    pub fn is_literal_value(self) -> bool {
        LITERALS.contains(self)
    }
    #[inline]
    pub fn is_opening_delimiter(self) -> bool {
        OPENING_DELIMITERS_SET.contains(self)
    }
    #[inline]
    pub fn is_unary_operator(self) -> bool {
        UNARY_PREFIX_OPERATORS_SET.contains(self) || UNARY_POSTFIX_OPERATORS_SET.contains(self)
    }
    #[inline]
    pub fn is_prefix_unary_operator(self) -> bool {
        UNARY_PREFIX_OPERATORS_SET.contains(self)
    }
    #[inline]
    pub fn is_posfix_unary_operator(self) -> bool {
        UNARY_POSTFIX_OPERATORS_SET.contains(self)
    }
    #[inline]
    pub fn is_trivia(self) -> bool {
        TRIVIA_SET.contains(self)
    }

    #[inline]
    pub fn assignments() -> &'static [Self] {
        ASSIGNMENTS
    }
    #[inline]
    pub fn can_be_parameter() -> &'static [Self] {
        CAN_BE_PARAMETERS
    }
    #[inline]
    pub fn closing_delimiters() -> &'static [Self] {
        CLOSING_DELIMITERS
    }
    #[inline]
    pub fn opening_delimiters() -> &'static [Self] {
        OPENING_DELIMITERS
    }
    #[inline]
    pub fn operators() -> &'static [Self] {
        OPERATORS
    }
    #[inline]
    pub fn non_assigning_operators() -> SyntaxKindBitSet {
        let a: SyntaxKindBitSet = SyntaxKind::assignments().as_ref().into();
        let o: SyntaxKindBitSet = SyntaxKind::operators().as_ref().into();
        o - a
    }
    #[inline]
    pub fn prefix_unary_operators() -> &'static [Self] {
        PREFIX_OPERATORS
    }
    #[inline]
    pub fn types() -> &'static [Self] {
        TYPES
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
                let operators: SyntaxKindBitSet = SyntaxKind::operators().into();
                context_update[2] = RestrictionType::Add(operators);
            }
            Colon => {
                context_update[0] = RestrictionType::Add([StructAsType, TypeHint].as_ref().into());
                let can_be_parameter: SyntaxKindBitSet = SyntaxKind::can_be_parameter().into();
                let allowed: SyntaxKindBitSet = [And, LParen, KwMut].as_ref().into();
                context_update[2] = RestrictionType::Override(can_be_parameter + allowed);
            }
            Condition => {
                context_update[1] = RestrictionType::Add([LBrace, KwElif, KwElse].as_ref().into());
                let operators: SyntaxKindBitSet = SyntaxKind::operators().into();
                let booleans_and_paren: SyntaxKindBitSet =
                    [KwTrue, KwFalse, LParen].as_ref().into();
                context_update[2] = RestrictionType::Override(operators + booleans_and_paren);
            }
            DimHints => {
                context_update[1] = RestrictionType::Add([Gt].as_ref().into());
                context_update[2] = RestrictionType::Override(SyntaxKindBitSet::empty());
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
                let non_assigning_operators = Self::non_assigning_operators();
                context_update[1] = RestrictionType::Add([LBrack, RBrack].as_ref().into());
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
            Mut => {
                // * Note: can_be_parameter also includes Ident as a parameter (to allow for StructAsType)
                let can_be_parameter: SyntaxKindBitSet = SyntaxKind::can_be_parameter().into();
                context_update[2] = RestrictionType::Override(can_be_parameter + LParen.into());
            }
            ParenExpr => {
                context_update[1] = RestrictionType::Add([Comma, RBrace].as_ref().into());
                context_update[2] = RestrictionType::Sub([Comma].as_ref().into());
            }
            ParamDecl => {
                let misc: SyntaxKindBitSet = [And, Colon, Comma, Ident, KwMut, LParen, RParen]
                    .as_ref()
                    .into();
                let can_be_parameter: SyntaxKindBitSet = SyntaxKind::can_be_parameter().into();
                context_update[2] = RestrictionType::Override(misc + can_be_parameter);
            }
            PrefixUnaryOp => {
                context_update[2] =
                    RestrictionType::Add(SyntaxKind::prefix_unary_operators().into());
            }
            RBrack => {
                context_update[1] = RestrictionType::Add([LBrack, RBrack].as_ref().into());
            }
            RetType => {
                context_update[0] = RestrictionType::Add([StructAsType].as_ref().into());
                let types: SyntaxKindBitSet = SyntaxKind::types().into();
                context_update[1] = RestrictionType::Add([LBrace].as_ref().into());
                let ref_struct_as_type: SyntaxKindBitSet =
                    [And, StructAsType, LParen].as_ref().into();
                context_update[2] = RestrictionType::Override(types + ref_struct_as_type);
            }
            RParen => {
                context_update[1] = RestrictionType::Add([LParen, RParen].as_ref().into());
            }
            TypeHint => {
                context_update[0] = RestrictionType::Add([StructAsType].as_ref().into());
                context_update[1] = RestrictionType::Add([Gt].as_ref().into());
                context_update[2] = RestrictionType::Sub([Gt].as_ref().into());
            }
            LetBinding => {
                // ! FIXME: this is a hack, I need to find a better way to expect things IN ORDER
                context_update[0] = RestrictionType::Add([Eq, Ident, Semi].as_ref().into());
                let non_assignments: SyntaxKindBitSet = Self::non_assigning_operators();
                let can_be_parameter: SyntaxKindBitSet = SyntaxKind::can_be_parameter().into();
                let opening_delimiters: SyntaxKindBitSet = SyntaxKind::opening_delimiters().into();
                let exceptionals: SyntaxKindBitSet =
                    [Eq, Ident, KwMut, SelfRef, Semi, StructLit, VarRef]
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
