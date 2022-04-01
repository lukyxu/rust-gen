use crate::ast::stmt::{ExprStmt, Stmt};
use crate::ast::ty::{FloatTy, IntTy, Ty, UIntTy};
use crate::Context;
use rand::Rng;

pub enum Expr {
    /// Literal such as `1`, `"foo"`
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
    Block(BlockExpr), // TODO: Path, Assign, Arrays, Box, Tuples
}

impl Expr {
    pub fn generate_expr(ctx: &mut Context, res_type: &Ty) -> Expr {
        let expr_kind = ctx.choose_expr_kind();
        match expr_kind {
            ExprKind::Literal => LitExpr::generate_expr(ctx, res_type),
            _ => panic!(),
        }
    }
}

pub enum LitExpr {
    // TODO: Support different styles of Strings such as raw strings `r##"foo"##`
    Str(String),
    Byte(u8),
    Char(char),
    Int(u128, LitExprTy),
    Float(String, LitFloatTy),
    Bool(bool),
}

impl From<LitExpr> for Expr {
    fn from(expr: LitExpr) -> Self {
        Expr::Literal(expr)
    }
}

impl LitExpr {
    pub fn generate_expr(ctx: &mut Context, res_type: &Ty) -> Expr {
        match res_type {
            // TODO: Try to clean this up
            Ty::Int(t) => {
                let val = t.rand_val(ctx);
                let expr_type = if ctx.choose_unsuffixed_int() {
                    LitExprTy::Signed(t.clone())
                } else {
                    LitExprTy::Unsuffixed
                };
                LitExpr::Int(val, expr_type).into()
            }
            Ty::UInt(t) => {
                let val = t.rand_val(ctx);
                let expr_type = if ctx.choose_unsuffixed_int() {
                    LitExprTy::Unsigned(t.clone())
                } else {
                    LitExprTy::Unsuffixed
                };
                LitExpr::Int(val, expr_type).into()
            },
            Ty::Tuple(_) => {
                panic!()
            }
            _ => panic!(),
        }
    }
}

pub enum LitExprTy {
    /// `64_i32`
    Signed(IntTy),
    /// `64_u32`
    Unsigned(UIntTy),
    /// `64`
    Unsuffixed(IntTy),
}

pub enum LitFloatTy {
    /// Float literal with suffix such as `1f32`, `1E10f32`
    Suffixed(FloatTy),
    /// Float literal without suffix such as `1.0`, `1.0E10`
    Unsuffixed,
}

pub struct BinaryExpr {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub op: BinaryOp,
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

pub struct UnaryExpr {
    pub expr: Box<Expr>,
    pub op: UnaryOp,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Deref,
    Not,
    Neg,
}

pub struct CastExpr {
    pub expr: Box<Expr>,
    pub ty: Ty,
}

pub struct IfExpr {
    pub condition: Box<Expr>,
    pub then: Box<Stmt>,
    pub otherwise: Option<Box<Stmt>>,
}

pub struct BlockExpr {
    pub stmts: Vec<Stmt>,
}

impl BlockExpr {
    pub fn generate_expr(ctx: &mut Context, res_type: &Ty) -> BlockExpr {
        let mut stmts: Vec<Stmt> = Vec::new();
        let mut num_stmts = ctx.choose_num_stmts();
        if !res_type.is_unit() {
            num_stmts -= 1
        }
        for _ in 0..num_stmts {
            // TODO: Make sure these statements are not expression statements
            stmts.push(Stmt::generate_non_expr_stmt(ctx))
        }
        if !res_type.is_unit() {
            stmts.push(Stmt::generate_expr_stmt(ctx, res_type));
        }
        BlockExpr { stmts }
    }
}

impl From<BlockExpr> for Expr {
    fn from(block_expr: BlockExpr) -> Self {
        Expr::Block(block_expr)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ExprKind {
    Literal,
    Binary,
    Unary,
    Cast,
    If,
    Block,
}
