use enumflags2::{BitFlags, bitflags};

use crate::span::Span;

#[derive(Debug, Clone)]
pub struct Module {
    pub span: Span,
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub enum LiteralKind {
    Int(i64),
    Float(f64),
}

#[derive(Debug, Clone)]
pub struct Literal {
    pub span: Span,
    pub raw: String,
    pub kind: LiteralKind,
}

#[derive(Debug, Clone)]
pub struct Ident {
    pub span: Span,
    pub value: String,
}

#[derive(Debug, Clone)]
pub enum BinanyOp {
    Equality,
    Inequality,
    GtEq,
    LtEq,
    Pow,
    Lshift,
    Rshift,
    And,
    Or,
    Nullish,
    Gt,
    Lt,
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Bwand,
    Bwor,
    Bwxor,
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Plus,
    Neg,
    Not,
    Bwnot,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(Literal),
    Ident(Ident),
    Assign(Box<Expression>, Box<Expression>),
    AssignOp(BinanyOp, Box<Expression>, Box<Expression>),
    BinaryOp(BinanyOp, Box<Expression>, Box<Expression>),
    UnaryOp(UnaryOp, Box<Expression>),
    EarlyRet(Box<Expression>),
    Field(Box<Expression>, Box<Expression>),
}

impl Expression {
    pub fn span(&self) -> Span {
        match self {
            Self::Literal(lit) => lit.span,
            Self::Ident(ident) => ident.span,
            Self::Assign(left, right)
            | Self::AssignOp(_, left, right)
            | Self::BinaryOp(_, left, right)
            | Self::Field(left, right) => left.span().extent_right(right.span()),
            Self::UnaryOp(_, expr) => expr.span(),
            Self::EarlyRet(expr) => expr.span(),
        }
    }
}

#[bitflags]
#[repr(u8)]
#[derive(Debug, Clone, Copy)]
pub enum Modifier {
    Mutability,
}

#[derive(Debug, Clone)]
pub struct Arg {
    pub modifiers: BitFlags<Modifier>,
    pub name: Ident,
    pub ty: Expression,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FuncDeclaration {
    pub name: Ident,
    pub args: Vec<Arg>,
    pub return_ty: Expression,
    pub body: Vec<Statement>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct VarDeclaration {
    pub modifiers: BitFlags<Modifier>,
    pub name: Ident,
    pub ty: Option<Expression>,
    pub value: Expression,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Statement {
    FuncDeclaration(FuncDeclaration),
    VarDeclaration(VarDeclaration),
}
