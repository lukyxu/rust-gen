use crate::ast::stmt::{ExprStmt, Stmt};
use crate::ast::ty::{FloatTy, IntTy, Ty, UIntTy};
use crate::Context;
use either::Either;
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
    /// A variable access such as `x` (Equivalent to Rust Path in Rust compiler)
    Ident(IdentExpr),
}

impl Expr {
    pub fn generate_expr(ctx: &mut Context, res_type: &Ty) -> Option<Expr> {
        let mut res: Option<Expr> = None;
        let mut num_failed_attempts = 0;
        while res.is_none() && num_failed_attempts < ctx.policy.max_expr_attempts {
            let expr_kind = ctx.choose_expr_kind();
            res = match expr_kind {
                ExprKind::Literal => LitExpr::generate_expr(ctx, res_type),
                ExprKind::If => IfExpr::generate_expr(ctx, res_type),
                ExprKind::Binary => BinaryExpr::generate_expr(ctx, res_type),
                ExprKind::Ident => IdentExpr::generate_expr(ctx, res_type),
                _ => panic!(),
            };
            num_failed_attempts += 1
        }
        res
    }

    pub fn generate_expr_safe(ctx: &mut Context, res_type: &Ty) -> Expr {
        let expr = Expr::generate_expr(ctx, &res_type);
        match expr {
            None => panic!("Failed to generate non expression statement"),
            Some(expr) => expr,
        }
    }
}

#[derive(Debug, Clone)]
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
    pub fn generate_expr(ctx: &mut Context, res_type: &Ty) -> Option<Expr> {
        match res_type {
            Ty::Bool => {
                // TODO(1): Add boolean
                Some(LitExpr::Bool(true).into())
            }
            Ty::Int(t) => {
                let val = t.rand_val(ctx);
                let expr_type = if matches!(t, IntTy::I32) && ctx.choose_unsuffixed_int() {
                    LitExprTy::Unsuffixed
                } else {
                    LitExprTy::Signed(t.clone())
                };
                Some(LitExpr::Int(val, expr_type).into())
            }
            Ty::UInt(t) => {
                let val = t.rand_val(ctx);
                Some(LitExpr::Int(val, LitExprTy::Unsigned(t.clone())).into())
            }
            Ty::Tuple(_) => {
                panic!()
            }
            _ => panic!(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum LitExprTy {
    /// `64_i32`
    Signed(IntTy),
    /// `64_u32`
    Unsigned(UIntTy),
    /// `64`
    /// Defaults to i32
    Unsuffixed,
}

#[derive(Debug, Clone)]
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

impl BinaryExpr {
    pub fn generate_expr(ctx: &mut Context, res_type: &Ty) -> Option<Expr> {
        // Binary op depth
        match res_type {
            Ty::Bool => None,
            Ty::Int(t) => {
                let op = ctx.choose_binary_int_op();
                let lhs = Expr::generate_expr(ctx, res_type);
                let lhs = if let Some(expr) = lhs {
                    Box::new(expr)
                } else {
                    return None;
                };
                let rhs = Expr::generate_expr(ctx, res_type);
                let rhs = if let Some(expr) = rhs {
                    Box::new(expr)
                } else {
                    return None;
                };
                Some(Expr::Binary(BinaryExpr { lhs, rhs, op }))
            }
            Ty::UInt(t) => {
                let val = t.rand_val(ctx);
                Some(LitExpr::Int(val, LitExprTy::Unsigned(t.clone())).into())
            }
            _ => panic!(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
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

// TODO: Improve IfExpr formatting
pub struct IfExpr {
    pub condition: Box<Expr>,
    pub then: Box<Stmt>,
    pub otherwise: Option<Box<Stmt>>,
}

impl IfExpr {
    pub fn generate_expr(ctx: &mut Context, res_type: &Ty) -> Option<Expr> {
        if ctx.if_else_depth + 1 > ctx.policy.max_if_else_depth {
            return None;
        }
        ctx.if_else_depth += 1;
        let cond = Expr::generate_expr(ctx, &Ty::Bool);
        let if_expr = match cond {
            None => None,
            Some(cond) => {
                let (then, otherwise) = if res_type.is_unit() {
                    let otherwise = if ctx.choose_otherwise_if_stmt() {
                        Some(Box::new(Stmt::generate_non_expr_stmt(ctx)))
                    } else {
                        None
                    };
                    (Box::new(Stmt::generate_non_expr_stmt(ctx)), otherwise)
                } else {
                    (
                        Box::new(Stmt::generate_expr_stmt(ctx, res_type)),
                        Some(Box::new(Stmt::generate_expr_stmt(ctx, res_type))),
                    )
                };
                Some(Expr::If(IfExpr {
                    condition: Box::new(cond),
                    then,
                    otherwise,
                }))
            }
        };
        ctx.if_else_depth -= 1;
        if_expr
    }
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

#[derive(Debug, Clone)]
pub struct IdentExpr {
    pub name: String,
    pub ty: Ty,
}

impl IdentExpr {
    fn generate_expr(ctx: &mut Context, res_type: &Ty) -> Option<Expr> {
        if let Some(ident_expr) = ctx.choose_ident_expr_by_type(res_type) {
            Some(Expr::Ident(ident_expr))
        } else {
            None
        }
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
    Ident,
}
