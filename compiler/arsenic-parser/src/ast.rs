use crate::span::Span;

pub struct Module {
    pub span: Span,
    pub statements: Vec<Statement>,
}

pub enum LiteralKind {
    Int(i64),
    Float(f64),
}

pub struct Literal {
    pub span: Span,
    pub raw: String,
    pub kind: LiteralKind,
}

pub struct Ident {
    pub span: Span,
    pub value: String,
}

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

pub enum UnaryOp {
    Plus,
    Neg,
    Not,
    Bwnot,
}

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

pub struct LetDeclaration {
    pub name: Ident,
    pub ty: Option<Expression>,
    pub value: Expression,
    pub span: Span
}

pub enum Statement {
    LetDeclaration(LetDeclaration)
}
