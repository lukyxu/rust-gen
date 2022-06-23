use crate::ast::stmt::Stmt;
use crate::ast::ty::{FloatTy, IntTy, Ty, UIntTy};

use crate::ast::op::{BinaryOp, UnaryOp};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq)]
#[non_exhaustive]
/// Rust expression
pub enum Expr {
    /// Literal such as `1_u32`, `"foo"`
    Literal(LitExpr),
    /// Binary operation such as `a + b`, `a * b`
    Binary(BinaryExpr),
    /// Unary operation such as `!x`
    Unary(UnaryExpr),
    /// Cast expression such as `x as u64`
    Cast(CastExpr),
    /// If expression with optional `else` block
    /// `if expr { block } else { expr }`
    If(IfExpr),
    /// Block expression
    Block(BlockExpr),
    /// A variable access such as `x` (Similar to Rust Path in Rust compiler).
    Ident(IdentExpr),
    /// Tuple literal expression such as `(1_u32, "hello")`.
    Tuple(TupleExpr),
    /// Assignment expression such as `(1_u32, "hello")`.
    Assign(AssignExpr),
    /// Array literal expression such as `[1_u32, 2_u32, 3_u32]`.
    Array(ArrayExpr),
    /// Index expression with squared brackets such as `array[5]`.
    Index(IndexExpr),
    /// Field expression representing access to structs and tuples such as `struct.field`.
    Field(FieldExpr),
    /// Struct literal expression such as `S { field1: value1, field2: value2 }` and `S(5_u32, "hello")`.
    Struct(StructExpr),
    /// Reference expression such as `&a` or `&mut a`.
    Reference(ReferenceExpr), // TODO: Path, Box
    /// Function call expression such as `f()`
    FunctionCall(FunctionCallExpr),
}

impl Expr {
    pub fn bool(b: bool) -> Expr {
        Expr::Literal(LitExpr::Bool(b))
    }

    pub fn i8(i: i8) -> Expr {
        LitIntExpr::new(i as u128, IntTy::I8.into()).into()
    }

    pub fn u8(u: u8) -> Expr {
        LitIntExpr::new(u as u128, UIntTy::U8.into()).into()
    }

    pub fn u32(u: u32) -> Expr {
        LitIntExpr::new(u as u128, UIntTy::U32.into()).into()
    }

    pub fn u128(u: u128) -> Expr {
        LitIntExpr::new(u, UIntTy::U128.into()).into()
    }

    pub fn kind(&self) -> ExprKind {
        match self {
            Expr::Literal(_) => ExprKind::Literal,
            Expr::Binary(_) => ExprKind::Binary,
            Expr::Unary(_) => ExprKind::Unary,
            Expr::Cast(_) => ExprKind::Cast,
            Expr::If(_) => ExprKind::If,
            Expr::Block(_) => ExprKind::Block,
            Expr::Ident(_) => ExprKind::Ident,
            Expr::Tuple(_) => ExprKind::Literal,
            Expr::Assign(_) => ExprKind::Assign,
            Expr::Array(_) => ExprKind::Literal,
            Expr::Index(_) => ExprKind::Index,
            Expr::Field(_) => ExprKind::Field,
            Expr::Struct(_) => ExprKind::Literal,
            Expr::Reference(_) => ExprKind::Reference,
            Expr::FunctionCall(_) => ExprKind::FunctionCall,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LitExpr {
    // TODO: Support different styles of Strings such as raw strings `r##"foo"##`
    #[allow(dead_code)]
    /// String literal.
    Str(String),
    #[allow(dead_code)]
    /// Byte literal.
    Byte(u8),
    #[allow(dead_code)]
    /// Char literal.
    Char(char),
    /// Integer literal.
    Int(LitIntExpr),
    #[allow(dead_code)]
    /// Float literal.
    Float(String, LitFloatTy),
    /// Boolean literal.
    Bool(bool),
}

impl From<LitExpr> for Expr {
    fn from(expr: LitExpr) -> Expr {
        Expr::Literal(expr)
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum LitIntTy {
    Signed(IntTy),
    Unsigned(UIntTy),
}

#[derive(Debug, Copy, Clone, PartialEq)]
/// Literal integer expression of a `LitIntTy` type.
/// The actual value of the integer is represented as the u128 value casted with respect to the type.
pub struct LitIntExpr {
    pub value: u128,
    pub ty: LitIntTy,
}

impl From<LitIntExpr> for Expr {
    fn from(expr: LitIntExpr) -> Expr {
        Expr::Literal(expr.into())
    }
}

impl From<LitIntExpr> for LitExpr {
    fn from(expr: LitIntExpr) -> LitExpr {
        LitExpr::Int(expr)
    }
}

impl LitIntExpr {
    pub fn new(value: u128, ty: LitIntTy) -> LitIntExpr {
        LitIntExpr { value, ty }
    }

    pub fn cast(self, ty: LitIntTy) -> LitIntExpr {
        match ty {
            LitIntTy::Signed(ty) => LitIntExpr::new(ty.cast_value(self.value), ty.into()),
            LitIntTy::Unsigned(ty) => LitIntExpr::new(ty.cast_value(self.value), ty.into()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
#[allow(dead_code)]
pub enum LitFloatTy {
    /// Float literal with suffix such as `1f32`, `1E10f32`.
    Suffixed(FloatTy),
    /// Float literal without suffix such as `1.0`, `1.0E10` (To remove once floats are implemented).
    Unsuffixed,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpr {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub op: BinaryOp,
}

impl From<BinaryExpr> for Expr {
    fn from(expr: BinaryExpr) -> Expr {
        Expr::Binary(expr)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpr {
    pub expr: Box<Expr>,
    pub op: UnaryOp,
}

impl From<UnaryExpr> for Expr {
    fn from(expr: UnaryExpr) -> Expr {
        Expr::Unary(expr)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CastExpr {
    pub expr: Box<Expr>,
    pub ty: Ty,
}

impl From<CastExpr> for Expr {
    fn from(expr: CastExpr) -> Expr {
        Expr::Cast(expr)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfExpr {
    pub condition: Box<Expr>,
    pub then: Box<BlockExpr>,
    pub otherwise: Option<Box<BlockExpr>>,
}

impl From<IfExpr> for Expr {
    fn from(expr: IfExpr) -> Expr {
        Expr::If(expr)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlockExpr {
    pub stmts: Vec<Stmt>,
}

impl From<BlockExpr> for Expr {
    fn from(expr: BlockExpr) -> Expr {
        Expr::Block(expr)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IdentExpr {
    pub name: String,
}

impl From<IdentExpr> for Expr {
    fn from(expr: IdentExpr) -> Expr {
        Expr::Ident(expr)
    }
}

impl From<IdentExpr> for PlaceExpr {
    fn from(expr: IdentExpr) -> PlaceExpr {
        PlaceExpr::Ident(expr)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TupleExpr {
    pub tuple: Vec<Expr>,
}

impl From<TupleExpr> for Expr {
    fn from(expr: TupleExpr) -> Expr {
        Expr::Tuple(expr)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum PlaceExpr {
    Field(FieldExpr),
    Index(IndexExpr),
    Ident(IdentExpr),
}

impl From<PlaceExpr> for Expr {
    fn from(place: PlaceExpr) -> Expr {
        match place {
            PlaceExpr::Field(expr) => expr.into(),
            PlaceExpr::Index(expr) => expr.into(),
            PlaceExpr::Ident(expr) => expr.into(),
        }
    }
}

impl TryFrom<Expr> for PlaceExpr {
    type Error = &'static str;

    fn try_from(expr: Expr) -> Result<PlaceExpr, Self::Error> {
        match expr {
            Expr::Ident(expr) => Ok(expr.into()),
            Expr::Index(expr) => Ok(expr.into()),
            Expr::Field(expr) => Ok(expr.into()),
            _ => Err("Cannot convert expr to place expr"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssignExpr {
    pub place: PlaceExpr,
    pub rhs: Box<Expr>,
}

impl From<AssignExpr> for Expr {
    fn from(expr: AssignExpr) -> Expr {
        Expr::Assign(expr)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayExpr {
    pub array: Vec<Expr>,
}

impl From<ArrayExpr> for Expr {
    fn from(expr: ArrayExpr) -> Expr {
        Expr::Array(expr)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Member {
    Named(String),
    Unnamed(usize),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldExpr {
    pub base: Box<Expr>,
    pub member: Member,
}

impl From<FieldExpr> for Expr {
    fn from(expr: FieldExpr) -> Expr {
        Expr::Field(expr)
    }
}

impl From<FieldExpr> for PlaceExpr {
    fn from(expr: FieldExpr) -> PlaceExpr {
        PlaceExpr::Field(expr)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IndexExpr {
    pub base: Box<Expr>,
    pub index: Box<Expr>,
}

impl From<IndexExpr> for Expr {
    fn from(expr: IndexExpr) -> Expr {
        Expr::Index(expr)
    }
}

impl From<IndexExpr> for PlaceExpr {
    fn from(expr: IndexExpr) -> PlaceExpr {
        PlaceExpr::Index(expr)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum StructExpr {
    /// Tuple struct such as `S { field1: value1, field2: value2 }`.
    Tuple(TupleStructExpr),
    /// Field struct such as `S(5_u32, "hello")`.
    Field(FieldStructExpr),
}

impl From<StructExpr> for Expr {
    fn from(expr: StructExpr) -> Expr {
        Expr::Struct(expr)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TupleStructExpr {
    pub struct_name: String,
    pub fields: TupleExpr,
}

impl From<TupleStructExpr> for StructExpr {
    fn from(expr: TupleStructExpr) -> StructExpr {
        StructExpr::Tuple(expr)
    }
}

impl From<TupleStructExpr> for Expr {
    fn from(expr: TupleStructExpr) -> Expr {
        Expr::Struct(StructExpr::Tuple(expr))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldStructExpr {
    pub struct_name: String,
    pub fields: Vec<Field>,
}

impl From<FieldStructExpr> for StructExpr {
    fn from(expr: FieldStructExpr) -> StructExpr {
        StructExpr::Field(expr)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub name: String,
    pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReferenceExpr {
    pub mutability: bool,
    pub expr: Box<Expr>,
}

impl From<ReferenceExpr> for Expr {
    fn from(expr: ReferenceExpr) -> Expr {
        Expr::Reference(expr)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCallExpr {
    pub name: String,
}

impl From<FunctionCallExpr> for Expr {
    fn from(expr: FunctionCallExpr) -> Expr {
        Expr::FunctionCall(expr)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
#[non_exhaustive]
pub enum ExprKind {
    Literal,
    Binary,
    Unary,
    Cast,
    If,
    Block,
    Ident,
    Assign,
    Index,
    Field,
    Reference,
    FunctionCall,
    __Nonexhaustive,
}
