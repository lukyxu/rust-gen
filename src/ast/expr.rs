use crate::ast::expr::EvalExprError::{MinMulOverflow, Overflow, ZeroDiv};
use crate::ast::expr::LitExprTy::{Signed, Unsigned, Unsuffixed};
use crate::ast::stmt::Stmt;
use crate::ast::ty::IntTy::*;

use crate::ast::ty::UIntTy::*;
use crate::ast::ty::{FloatTy, IntTy, Ty, UIntTy};
use crate::Context;
use std::{isize, u32, usize};

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
                ExprKind::Block => Some(BlockExpr::generate_expr(ctx, res_type).into()),
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
                Some(LitExpr::Bool(ctx.choose_boolean_true()).into())
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
        if ctx.arith_depth + 1 > ctx.policy.max_arith_depth {
            return None;
        }
        ctx.arith_depth += 1;
        // Binary op depth
        let res = match res_type {
            Ty::Bool => {
                let op = ctx.choose_binary_bool_op();
                let lhs = Expr::generate_expr(ctx, res_type);
                let lhs = if let Some(expr) = lhs {
                    Box::new(expr)
                } else {
                    ctx.arith_depth -= 1;
                    return None;
                };
                let rhs = Expr::generate_expr(ctx, res_type);
                let rhs = if let Some(expr) = rhs {
                    Box::new(expr)
                } else {
                    ctx.arith_depth -= 1;
                    return None;
                };
                Some(Expr::Binary(BinaryExpr { lhs, rhs, op }))
            }
            Ty::Int(_t) => {
                let op = ctx.choose_binary_int_op();
                let lhs = Expr::generate_expr(ctx, res_type);
                let lhs = if let Some(expr) = lhs {
                    Box::new(expr)
                } else {
                    ctx.arith_depth -= 1;
                    return None;
                };
                let rhs = Expr::generate_expr(ctx, res_type);
                let rhs = if let Some(expr) = rhs {
                    Box::new(expr)
                } else {
                    ctx.arith_depth -= 1;
                    return None;
                };
                Some(Expr::Binary(BinaryExpr { lhs, rhs, op }))
            }
            Ty::UInt(t) => {
                let val = t.rand_val(ctx);
                Some(LitExpr::Int(val, LitExprTy::Unsigned(t.clone())).into())
            }
            _ => panic!(),
        };
        ctx.arith_depth -= 1;
        res
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

macro_rules! apply_int {
    ($fn_name: ident, $op_name: ident) => {
        fn $fn_name(
            self,
            lhs_u128: u128,
            lhs: LitExprTy,
            rhs_u128: u128,
            rhs: LitExprTy,
        ) -> Result<LitExpr, EvalExprError> {
            match (&lhs, &rhs) {
                (Signed(I8), Signed(I8)) => i8::$op_name(lhs_u128 as i8, rhs_u128 as i8),
                (Signed(I16), Signed(I16)) => i16::$op_name(lhs_u128 as i16, rhs_u128 as i16),
                (Signed(I32), Signed(I32)) => i32::$op_name(lhs_u128 as i32, rhs_u128 as i32),
                (Signed(I32), Unsuffixed) => i32::$op_name(lhs_u128 as i32, rhs_u128 as i32),
                (Unsuffixed, Signed(I32)) => i32::$op_name(lhs_u128 as i32, rhs_u128 as i32),
                (Signed(I64), Signed(I64)) => i64::$op_name(lhs_u128 as i64, rhs_u128 as i64),
                (Signed(I128), Signed(I128)) => i128::$op_name(lhs_u128 as i128, rhs_u128 as i128),
                (Signed(ISize), Signed(ISize)) => {
                    isize::$op_name(lhs_u128 as isize, rhs_u128 as isize)
                }
                (Unsigned(U8), Unsigned(U8)) => u8::$op_name(lhs_u128 as u8, rhs_u128 as u8),
                (Unsigned(U16), Unsigned(U16)) => u16::$op_name(lhs_u128 as u16, rhs_u128 as u16),
                (Unsigned(U32), Unsigned(U32)) => u32::$op_name(lhs_u128 as u32, rhs_u128 as u32),
                (Unsigned(U64), Unsigned(U64)) => u64::$op_name(lhs_u128 as u64, rhs_u128 as u64),
                (Unsigned(U128), Unsigned(U128)) => {
                    u128::$op_name(lhs_u128 as u128, rhs_u128 as u128)
                }
                (Unsigned(USize), Unsigned(USize)) => {
                    usize::$op_name(lhs_u128 as usize, rhs_u128 as usize)
                }
                (Unsuffixed, Unsuffixed) => (i32::$op_name(lhs_u128 as i32, rhs_u128 as i32)),
                _ => panic!(),
            }
        }
    };
}

impl BinaryOp {
    pub fn apply(self, lhs: LitExpr, rhs: LitExpr) -> Result<LitExpr, EvalExprError> {
        use LitExpr::*;
        match (lhs, rhs) {
            (Int(lhs_u128, lhs_ty), Int(rhs_u128, rhs_ty)) => {
                self.apply_int(lhs_u128, lhs_ty, rhs_u128, rhs_ty)
            }
            (Bool(lhs), Bool(rhs)) => Ok(self.apply_bool(lhs, rhs)),
            _ => panic!("Non integer/booleans"),
        }
    }
    apply_int!(apply_add, expr_add);
    apply_int!(apply_sub, expr_sub);
    apply_int!(apply_mul, expr_mul);
    apply_int!(apply_div, expr_div);
    fn apply_int(
        self,
        lhs_u128: u128,
        lhs: LitExprTy,
        rhs_u128: u128,
        rhs: LitExprTy,
    ) -> Result<LitExpr, EvalExprError> {
        match self {
            BinaryOp::Add => self.apply_add(lhs_u128, lhs, rhs_u128, rhs),
            BinaryOp::Sub => self.apply_sub(lhs_u128, lhs, rhs_u128, rhs),
            BinaryOp::Mul => self.apply_mul(lhs_u128, lhs, rhs_u128, rhs),
            BinaryOp::Div => self.apply_div(lhs_u128, lhs, rhs_u128, rhs),
            _ => panic!("Undefined operation on ints"),
        }
    }

    fn apply_bool(self, lhs: bool, rhs: bool) -> LitExpr {
        match self {
            BinaryOp::And => LitExpr::Bool(lhs && rhs),
            BinaryOp::Or => LitExpr::Bool(lhs || rhs),
            _ => panic!(),
        }
    }
}

trait Literal<T> {
    fn expr_add(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError>;
    fn expr_sub(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError>;
    fn expr_mul(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError>;
    fn expr_div(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError>;
}

macro_rules! literal {
    ($rust_ty: ident, $ty: expr) => {
        impl Literal<$rust_ty> for $rust_ty {
            fn expr_add(lhs: $rust_ty, rhs: $rust_ty) -> Result<LitExpr, EvalExprError> {
                if let Some(res) = lhs.checked_add(rhs) {
                    Ok(LitExpr::Int(res as u128, $ty))
                } else {
                    Err(Overflow)
                }
            }

            fn expr_sub(lhs: $rust_ty, rhs: $rust_ty) -> Result<LitExpr, EvalExprError> {
                if let Some(res) = lhs.checked_sub(rhs) {
                    Ok(LitExpr::Int(res as u128, $ty))
                } else {
                    Err(Overflow)
                }
            }

            fn expr_mul(lhs: $rust_ty, rhs: $rust_ty) -> Result<LitExpr, EvalExprError> {
                if let Some(res) = lhs.checked_mul(rhs) {
                    Ok(LitExpr::Int(res as u128, $ty))
                } else {
                    let is_signed = $rust_ty::MIN < 0;
                    if is_signed
                        && (((lhs == $rust_ty::MIN) && rhs + 1 == 0)
                            || (rhs == $rust_ty::MIN && lhs + 1 == 0))
                    {
                        Err(MinMulOverflow)
                    } else {
                        Err(Overflow)
                    }
                }
            }

            fn expr_div(lhs: $rust_ty, rhs: $rust_ty) -> Result<LitExpr, EvalExprError> {
                if let Some(res) = lhs.checked_div(rhs) {
                    Ok(LitExpr::Int(res as u128, $ty))
                } else {
                    if rhs == 0 {
                        Err(ZeroDiv)
                    } else {
                        Err(Overflow)
                    }
                }
            }
        }
    };
}

literal!(i8, Signed(I8));
literal!(i16, Signed(I16));
literal!(i32, Signed(I32));
literal!(i64, Signed(I64));
literal!(i128, Signed(I128));
literal!(isize, Signed(ISize));
literal!(u8, Unsigned(U8));
literal!(u16, Unsigned(U16));
literal!(u32, Unsigned(U32));
literal!(u64, Unsigned(U64));
literal!(u128, Unsigned(U128));
literal!(usize, Unsigned(USize));

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
        let outer_symbol_table = ctx.type_symbol_table.clone();
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
        ctx.type_symbol_table = outer_symbol_table;
        ctx.if_else_depth -= 1;
        if_expr
    }
}

pub struct BlockExpr {
    pub stmts: Vec<Stmt>,
}

impl BlockExpr {
    // TODO: Make this return an optional
    pub fn generate_expr(ctx: &mut Context, res_type: &Ty) -> BlockExpr {
        let mut stmts: Vec<Stmt> = Vec::new();
        let outer_symbol_table = ctx.type_symbol_table.clone();
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
        ctx.type_symbol_table = outer_symbol_table;
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

#[derive(Copy, Clone)]
pub enum EvalExprError {
    Overflow,
    MinMulOverflow,
    ZeroDiv,
}
