use lexer::token_kind::TokenKind;
use num_derive::{FromPrimitive, ToPrimitive};
use thin_vec::{ThinVec, thin_vec};

#[derive(Copy, Clone, Debug, FromPrimitive, Eq, Hash, Ord, PartialEq, PartialOrd, ToPrimitive)]
pub enum SyntaxKind {
    // recovering
    Recovered,
    Errored,
    // required for CST
    Root,
    // token-tree roots for n-ary operators
    InfixBinOp,
    PrefixUnaryOp,
    PostfixUnaryOp,
    // token-tree roots for expressions/sub-expressions
    CharLit,
    DimHints,
    DimHint,
    DimValue,
    FnCall,
    In,
    Initializer,
    ImplBlock,
    Jump,
    Literal,
    Mut,
    ParenExpr,
    ParamDecl,
    RetType,
    Return,
    SelfRef,
    Semi,
    StrLit,
    StructField,
    StructFields,
    StructInit,
    StructRef,
    StructAsType,
    TensorInit,
    TensorLit,
    TensorStruct,
    TensorType,
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
    ForIdent,
    ForLoop,
    Indexing,
    StructDef,
    VarDef,
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
    // TOOD: arr[<expr>] -> [<expr>] is actually a 'composite' postfix operator
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
    TyChar,
    TyF32,
    TyI32,
    TyStr,
    TyStrSlc,
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
    pub fn is_literal_value(&self) -> bool {
        use SyntaxKind::*;
        matches!(
            self,
            CharLit | Float | Int | KwFalse | KwTrue | StrLit | TensorLit
        )
    }

    pub fn types() -> ThinVec<SyntaxKind> {
        use SyntaxKind::*;
        thin_vec![
            StructAsType,
            TensorType,
            TyBool,
            TyChar,
            TyF32,
            TyI32,
            TyStr,
            TyStrSlc,
            TyTensor,
        ]
    }

    pub fn can_be_parameter() -> ThinVec<SyntaxKind> {
        use SyntaxKind::*;
        thin_vec![
            SelfRef,
            StructAsType,
            TensorType,
            TyBool,
            TyChar,
            TyF32,
            TyI32,
            TyStr,
            TyStrSlc,
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
            TypeStrSlice => Self::TyStrSlc,
            TypeString => Self::TyStr,
            TypeTensor => Self::TyTensor,
            Underscore => Self::Under,
            Xor => Self::Xor,
            _ => {
                let msg = format!("{:?} don't have a corresponding syntaxkind", token_kind);
                panic!("{}", msg)
            }
        }
    }
}
