use crate::ast::expr::EvalExprError::{MinMulOverflow, SignedOverflow, UnsignedOverflow, ZeroDiv};
use crate::ast::expr::LitExprTy::{Signed, Unsigned, Unsuffixed};
use crate::ast::stmt::Stmt;
use crate::ast::ty::IntTy::*;

use crate::ast::ty::UIntTy::*;
use crate::ast::ty::{FloatTy, IntTy, Ty, UIntTy};
use crate::Context;
use num_traits::{AsPrimitive, CheckedRem, PrimInt, WrappingAdd};
use rand::prelude::SliceRandom;

use std::mem::swap;
use std::{isize, u32, usize};

#[derive(Debug, Clone, PartialEq)]
#[non_exhaustive]
pub enum Expr {
    /// Literal such as `1`, `"foo"`
    Literal(LitExpr),
    /// Binary operation such as `a + b`, `a * b`
    Binary(BinaryExpr),
    /// Unary operation such as `!x`
    Unary(UnaryExpr),
    /// Cast expression such as `x as u64`
    #[allow(dead_code)]
    Cast(CastExpr),
    /// If expression with optional `else` block
    /// `if expr { block } else { expr }`
    If(IfExpr),
    /// Block expression
    Block(BlockExpr), // TODO: Path, Assign, Arrays, Box, Tuples
    /// A variable access such as `x` (Equivalent to Rust Path in Rust compiler)
    Ident(IdentExpr),
    Tuple(TupleExpr),
    Assign(AssignExpr),
    Array(ArrayExpr),
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
                ExprKind::Block => BlockExpr::generate_expr(ctx, res_type),
                ExprKind::Assign => AssignExpr::generate_expr(ctx, res_type),
                ExprKind::Unary => UnaryExpr::generate_expr(ctx, res_type),
                ExprKind::Cast => CastExpr::generate_expr(ctx, res_type),
                _ => panic!("ExprKind {:?} not supported yet", expr_kind),
            };
            num_failed_attempts += 1
        }
        res
    }

    pub fn generate_expr_safe(ctx: &mut Context, res_type: &Ty) -> Expr {
        let expr = Expr::generate_expr(ctx, res_type);
        match expr {
            None => panic!("Failed to generate non expression statement"),
            Some(expr) => expr,
        }
    }
}

#[cfg(test)]
impl Expr {
    pub fn bool(b: bool) -> Expr {
        Expr::Literal(LitExpr::Bool(b))
    }

    pub fn i8(i: i8) -> Expr {
        Expr::Literal(LitExpr::Int(i as u128, LitExprTy::Signed(IntTy::I8)))
    }

    pub fn u8(u: u8) -> Expr {
        Expr::Literal(LitExpr::Int(u as u128, LitExprTy::Unsigned(UIntTy::U8)))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LitExpr {
    // TODO: Support different styles of Strings such as raw strings `r##"foo"##`
    #[allow(dead_code)]
    Str(String),
    #[allow(dead_code)]
    Byte(u8),
    #[allow(dead_code)]
    Char(char),
    Int(u128, LitExprTy),
    #[allow(dead_code)]
    Float(String, LitFloatTy),
    Bool(bool),
}

impl From<LitExpr> for Expr {
    fn from(expr: LitExpr) -> Self {
        Expr::Literal(expr)
    }
}

impl From<LitExpr> for EvalExpr {
    fn from(expr: LitExpr) -> Self {
        EvalExpr::Literal(expr)
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
                    LitExprTy::Signed(*t)
                };
                Some(LitExpr::Int(val, expr_type).into())
            }
            Ty::UInt(t) => {
                let val = t.rand_val(ctx);
                Some(LitExpr::Int(val, LitExprTy::Unsigned(*t)).into())
            }
            tuple @ Ty::Tuple(_) => TupleExpr::generate_expr(ctx, tuple),
            array @ Ty::Array(..) => ArrayExpr::generate_expr(ctx, array),
            _ => panic!(
                "Literal type for {} not supported yet",
                res_type.to_string()
            ),
        }
    }
    pub fn cast(self, res_type: &Ty) -> LitExpr {
        if let LitExpr::Int(u128, _) = self {
            match res_type {
                Ty::Int(s_int) => LitExpr::Int(s_int.recast(u128), LitExprTy::Signed(*s_int)),
                Ty::UInt(u_int) => LitExpr::Int(u_int.recast(u128), LitExprTy::Unsigned(*u_int)),
                _ => panic!(),
            }
        } else {
            panic!()
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum LitExprTy {
    /// `64_i32`
    Signed(IntTy),
    /// `64_u32`
    Unsigned(UIntTy),
    /// `64`
    /// Defaults to i32
    Unsuffixed,
}

#[derive(Debug, Clone, PartialEq)]
#[allow(dead_code)]
pub enum LitFloatTy {
    /// Float literal with suffix such as `1f32`, `1E10f32`
    Suffixed(FloatTy),
    /// Float literal without suffix such as `1.0`, `1.0E10`
    Unsuffixed,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpr {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub op: BinaryOp,
}

impl BinaryExpr {
    pub fn generate_expr(ctx: &mut Context, res_type: &Ty) -> Option<Expr> {
        if ctx.arith_depth > ctx.policy.max_arith_depth {
            return None;
        }
        ctx.arith_depth += 1;
        // Binary op depth
        let res = BinaryExpr::generate_expr_internal(ctx, res_type);
        ctx.arith_depth -= 1;
        res
    }

    fn generate_expr_internal(ctx: &mut Context, res_type: &Ty) -> Option<Expr> {
        let op = match res_type {
            Ty::Bool => ctx.choose_binary_bool_op(),
            Ty::Int(_) | Ty::UInt(_) => ctx.choose_binary_int_op(),
            Ty::Tuple(_) | Ty::Array(..) => return None,
            _ => panic!(
                "Binary operations for {} not supported",
                res_type.to_string()
            ),
        };
        let lhs = Box::new(Expr::generate_expr(ctx, res_type)?);
        let rhs = Box::new(Expr::generate_expr(ctx, res_type)?);
        Some(Expr::Binary(BinaryExpr { lhs, rhs, op }))
    }
    pub fn fix(&mut self, error: &EvalExprError, lhs: &mut EvalExpr, rhs: &mut EvalExpr) {
        if let EvalExprError::UnsignedOverflow = error {
            if self.op == BinaryOp::Sub {
                swap(&mut self.lhs, &mut self.rhs);
                swap(lhs, rhs)
            }
        }
        self.op = self.replacement_op(error)
    }

    pub fn replacement_op(&self, error: &EvalExprError) -> BinaryOp {
        match self.op {
            BinaryOp::Add => BinaryOp::Sub,
            BinaryOp::Sub => match error {
                SignedOverflow => BinaryOp::Add,
                UnsignedOverflow => BinaryOp::Sub,
                _ => panic!(),
            },
            BinaryOp::Mul => {
                if let EvalExprError::MinMulOverflow = error {
                    BinaryOp::Sub
                } else {
                    // Signed/Unsigned Overflow
                    BinaryOp::Div
                }
            }
            BinaryOp::Div | BinaryOp::Rem => {
                if let EvalExprError::ZeroDiv = error {
                    BinaryOp::Mul
                } else {
                    // Signed Overflow
                    BinaryOp::Sub
                }
            }
            _ => panic!(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
#[non_exhaustive]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    And,
    Or,
    // BitXor,
    // BitAnd,
    // BitOr,
    // Shl,
    // Shr,
    // Eq,
    // Lq,
    // Le,
    // Ne,
    // Ge,
    // Gt
}

impl ToString for BinaryOp {
    fn to_string(&self) -> String {
        match self {
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::Rem => "%",
            BinaryOp::And => "&&",
            BinaryOp::Or => "||",
        }
        .to_owned()
    }
}

macro_rules! apply_int {
    ($fn_name: ident, $op_name: ident) => {
        fn $fn_name(
            self,
            lhs_u128: u128,
            lhs: &LitExprTy,
            rhs_u128: u128,
            rhs: &LitExprTy,
        ) -> Result<LitExpr, EvalExprError> {
            match (lhs, rhs) {
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
                _ => panic!("Mismatch type in binary operation {:?} {:?}", lhs, rhs),
            }
        }
    };
}

impl BinaryOp {
    // TODO: try refactor this
    pub fn short_circuit_rhs(self, lhs: &EvalExpr) -> bool {
        let short_circuit_and = matches!(
            (self, lhs),
            (BinaryOp::And, EvalExpr::Literal(LitExpr::Bool(false)))
        );
        let short_circuit_or = matches!(
            (self, lhs),
            (BinaryOp::Or, EvalExpr::Literal(LitExpr::Bool(true)))
        );
        short_circuit_and || short_circuit_or
    }

    pub fn apply(self, lhs: &EvalExpr, rhs: &EvalExpr) -> Result<EvalExpr, EvalExprError> {
        match (self, lhs, rhs) {
            (_, EvalExpr::Literal(lhs), EvalExpr::Literal(rhs)) => {
                let res: Result<LitExpr, EvalExprError> = self.apply_lit(lhs, rhs);
                match res {
                    Ok(lit_expr) => Ok(lit_expr.into()),
                    Err(error) => Err(error),
                }
            }
            (BinaryOp::Div, _, EvalExpr::Literal(rhs))
            | (BinaryOp::Rem, _, EvalExpr::Literal(rhs)) => {
                if let LitExpr::Int(0, _) = rhs {
                    Err(ZeroDiv)
                } else if let LitExpr::Int(_, _) = rhs {
                    Ok(EvalExpr::Literal(rhs.clone()))
                } else {
                    Ok(EvalExpr::Unknown)
                }
            }
            _ => Ok(EvalExpr::Unknown),
        }
    }

    pub fn apply_lit(self, lhs: &LitExpr, rhs: &LitExpr) -> Result<LitExpr, EvalExprError> {
        use LitExpr::*;
        match (lhs, rhs) {
            (Int(lhs_u128, lhs_ty), Int(rhs_u128, rhs_ty)) => {
                self.apply_int(*lhs_u128, lhs_ty, *rhs_u128, rhs_ty)
            }
            (Bool(lhs), Bool(rhs)) => Ok(self.apply_bool(*lhs, *rhs)),
            _ => panic!("Non integer/booleans"),
        }
    }
    apply_int!(apply_add, expr_add);
    apply_int!(apply_sub, expr_sub);
    apply_int!(apply_mul, expr_mul);
    apply_int!(apply_div, expr_div);
    apply_int!(apply_rem, expr_rem);

    fn apply_int(
        self,
        lhs_u128: u128,
        lhs: &LitExprTy,
        rhs_u128: u128,
        rhs: &LitExprTy,
    ) -> Result<LitExpr, EvalExprError> {
        match self {
            BinaryOp::Add => self.apply_add(lhs_u128, lhs, rhs_u128, rhs),
            BinaryOp::Sub => self.apply_sub(lhs_u128, lhs, rhs_u128, rhs),
            BinaryOp::Mul => self.apply_mul(lhs_u128, lhs, rhs_u128, rhs),
            BinaryOp::Div => self.apply_div(lhs_u128, lhs, rhs_u128, rhs),
            BinaryOp::Rem => self.apply_rem(lhs_u128, lhs, rhs_u128, rhs),
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

trait Literal<
    T: PrimInt
        + Copy
        + AsPrimitive<u128>
        + WrappingAdd<Output = T>
        + ByLitExprTy<T>
        + CheckedRem<Output = T>,
>
{
    fn expr_add(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError>;
    fn expr_sub(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError>;
    fn expr_mul(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError>;
    fn expr_div(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError>;
    fn expr_rem(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError>;
}

trait ByLitExprTy<T> {
    fn by_lit_expr_type() -> LitExprTy;
}

macro_rules! by_lit_expr_ty_impl {
    ($rust_ty: ident, $ty: expr) => {
        impl ByLitExprTy<$rust_ty> for $rust_ty {
            fn by_lit_expr_type() -> LitExprTy {
                $ty
            }
        }
    };
}

impl<
        T: PrimInt
            + Copy
            + AsPrimitive<u128>
            + WrappingAdd<Output = T>
            + ByLitExprTy<T>
            + CheckedRem<Output = T>,
    > Literal<T> for T
{
    fn expr_add(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError> {
        if let Some(res) = lhs.checked_add(&rhs) {
            Ok(LitExpr::Int(res.as_(), T::by_lit_expr_type()))
        } else {
            Err(EvalExprError::overflow_error::<T>())
        }
    }

    fn expr_sub(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError> {
        if let Some(res) = lhs.checked_sub(&rhs) {
            Ok(LitExpr::Int(res.as_(), T::by_lit_expr_type()))
        } else {
            Err(EvalExprError::overflow_error::<T>())
        }
    }

    fn expr_mul(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError> {
        if let Some(res) = lhs.checked_mul(&rhs) {
            Ok(LitExpr::Int(res.as_(), T::by_lit_expr_type()))
        } else {
            let is_signed = T::min_value() < T::zero();

            if is_signed
                && (((lhs == T::min_value()) && rhs.wrapping_add(&T::one()) == T::zero())
                    || (rhs == T::min_value() && lhs.wrapping_add(&T::one()) == T::zero()))
            {
                Err(MinMulOverflow)
            } else {
                Err(EvalExprError::overflow_error::<T>())
            }
        }
    }

    fn expr_div(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError> {
        if let Some(res) = lhs.checked_div(&rhs) {
            Ok(LitExpr::Int(res.as_(), T::by_lit_expr_type()))
        } else if rhs == T::zero() {
            Err(ZeroDiv)
        } else {
            assert!(T::min_value() < T::zero());
            Err(SignedOverflow)
        }
    }

    fn expr_rem(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError> {
        if let Some(res) = lhs.checked_rem(&rhs) {
            Ok(LitExpr::Int(res.as_(), T::by_lit_expr_type()))
        } else if rhs == T::zero() {
            Err(ZeroDiv)
        } else {
            assert!(T::min_value() < T::zero());
            Err(SignedOverflow)
        }
    }
}

by_lit_expr_ty_impl!(i8, Signed(I8));
by_lit_expr_ty_impl!(i16, Signed(I16));
by_lit_expr_ty_impl!(i32, Signed(I32));
by_lit_expr_ty_impl!(i64, Signed(I64));
by_lit_expr_ty_impl!(i128, Signed(I128));
by_lit_expr_ty_impl!(isize, Signed(ISize));
by_lit_expr_ty_impl!(u8, Unsigned(U8));
by_lit_expr_ty_impl!(u16, Unsigned(U16));
by_lit_expr_ty_impl!(u32, Unsigned(U32));
by_lit_expr_ty_impl!(u64, Unsigned(U64));
by_lit_expr_ty_impl!(u128, Unsigned(U128));
by_lit_expr_ty_impl!(usize, Unsigned(USize));

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpr {
    pub expr: Box<Expr>,
    pub op: UnaryOp,
}

impl UnaryExpr {
    #[allow(dead_code)]
    pub fn generate_expr(ctx: &mut Context, res_type: &Ty) -> Option<Expr> {
        if ctx.arith_depth > ctx.policy.max_arith_depth {
            return None;
        }
        ctx.arith_depth += 1;
        let res = UnaryExpr::generate_expr_internal(ctx, res_type);
        ctx.arith_depth -= 1;
        res
    }

    // TODO: generate_expr_internal
    #[allow(dead_code)]
    pub fn generate_expr_internal(ctx: &mut Context, res_type: &Ty) -> Option<Expr> {
        let op = match res_type {
            Ty::Bool => UnaryOp::Not,
            Ty::Int(_) => UnaryOp::Neg,
            _ => return None,
        };
        let expr = Box::new(Expr::generate_expr(ctx, res_type)?);
        Some(Expr::Unary(UnaryExpr { expr, op }))
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
#[allow(dead_code)]
pub enum UnaryOp {
    // TODO: Deref when adding pointer types
    Deref,
    Not,
    Neg,
}

impl ToString for UnaryOp {
    fn to_string(&self) -> String {
        match self {
            UnaryOp::Deref => "*",
            UnaryOp::Not => "!",
            UnaryOp::Neg => "-",
        }
        .to_owned()
    }
}

impl UnaryOp {
    pub fn apply(self, expr: &EvalExpr) -> Result<EvalExpr, EvalExprError> {
        match self {
            UnaryOp::Deref => todo!(),
            UnaryOp::Not => {
                if let EvalExpr::Literal(LitExpr::Bool(bool)) = *expr {
                    Ok(EvalExpr::Literal(LitExpr::Bool(!bool)))
                } else {
                    panic!()
                }
            }
            UnaryOp::Neg => {
                if let EvalExpr::Literal(LitExpr::Int(u128, ty @ Signed(int_type))) = expr {
                    let (u128, ty) = (*u128, *ty);
                    match int_type {
                        ISize => isize::checked_neg(u128 as isize)
                            .map(|isize| EvalExpr::Literal(LitExpr::Int(isize as u128, ty)))
                            .ok_or(EvalExprError::SignedOverflow),
                        I8 => i8::checked_neg(u128 as i8)
                            .map(|isize| EvalExpr::Literal(LitExpr::Int(isize as u128, ty)))
                            .ok_or(EvalExprError::SignedOverflow),
                        I16 => i16::checked_neg(u128 as i16)
                            .map(|isize| EvalExpr::Literal(LitExpr::Int(isize as u128, ty)))
                            .ok_or(EvalExprError::SignedOverflow),
                        I32 => i32::checked_neg(u128 as i32)
                            .map(|isize| EvalExpr::Literal(LitExpr::Int(isize as u128, ty)))
                            .ok_or(EvalExprError::SignedOverflow),
                        I64 => i64::checked_neg(u128 as i64)
                            .map(|isize| EvalExpr::Literal(LitExpr::Int(isize as u128, ty)))
                            .ok_or(EvalExprError::SignedOverflow),
                        I128 => i128::checked_neg(u128 as i128)
                            .map(|isize| EvalExpr::Literal(LitExpr::Int(isize as u128, ty)))
                            .ok_or(EvalExprError::SignedOverflow),
                    }
                } else if let EvalExpr::Literal(LitExpr::Int(u128, Unsuffixed)) = expr {
                    i32::checked_neg(*u128 as i32)
                        .map(|isize| {
                            EvalExpr::Literal(LitExpr::Int(
                                isize as u128,
                                LitExprTy::Signed(IntTy::I32),
                            ))
                        })
                        .ok_or(EvalExprError::SignedOverflow)
                } else if let EvalExpr::Unknown = expr {
                    Ok(EvalExpr::Unknown)
                } else {
                    panic!()
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CastExpr {
    pub expr: Box<Expr>,
    pub ty: Ty,
}

impl CastExpr {
    pub fn generate_expr(ctx: &mut Context, res_type: &Ty) -> Option<Expr> {
        if ctx.arith_depth > ctx.policy.max_arith_depth {
            return None;
        }
        ctx.arith_depth += 1;
        let res = CastExpr::generate_expr_internal(ctx, res_type);
        ctx.arith_depth -= 1;
        res
    }

    pub fn generate_expr_internal(ctx: &mut Context, res_type: &Ty) -> Option<Expr> {
        let source_type = ctx.choose_type();
        if !source_type.compatible_cast(res_type) {
            return None;
        }
        let expr = Box::new(Expr::generate_expr_safe(ctx, &source_type));
        Some(Expr::Cast(CastExpr {
            expr,
            ty: res_type.clone(),
        }))
    }
}

// TODO: Improve IfExpr formatting in printing
#[derive(Debug, Clone, PartialEq)]
pub struct IfExpr {
    pub condition: Box<Expr>,
    pub then: Box<BlockExpr>,
    pub otherwise: Option<Box<BlockExpr>>,
}

impl IfExpr {
    pub fn generate_expr(ctx: &mut Context, res_type: &Ty) -> Option<Expr> {
        if ctx.if_else_depth > ctx.policy.max_if_else_depth {
            return None;
        }
        let outer_symbol_table = ctx.type_symbol_table.clone();
        ctx.if_else_depth += 1;
        let cond = Expr::generate_expr(ctx, &Ty::Bool);
        let if_expr = match cond {
            None => None,
            Some(cond) => {
                let then = Box::new(BlockExpr::generate_block_expr(ctx, res_type).unwrap());
                let otherwise = if !res_type.is_unit() || ctx.choose_otherwise_if_stmt() {
                    Some(Box::new(
                        BlockExpr::generate_block_expr(ctx, res_type).unwrap(),
                    ))
                } else {
                    None
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

#[derive(Debug, Clone, PartialEq)]
pub struct BlockExpr {
    pub stmts: Vec<Stmt>,
}

impl BlockExpr {
    // TODO: Make this return an optional
    pub fn generate_expr(ctx: &mut Context, res_type: &Ty) -> Option<Expr> {
        let block_expr = BlockExpr::generate_block_expr(ctx, res_type);
        block_expr.map(Expr::Block)
    }
    pub fn generate_block_expr(ctx: &mut Context, res_type: &Ty) -> Option<BlockExpr> {
        if ctx.block_depth > ctx.policy.max_block_depth {
            return None;
        }
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
        Some(BlockExpr { stmts })
    }
}

impl From<BlockExpr> for Expr {
    fn from(block_expr: BlockExpr) -> Self {
        Expr::Block(block_expr)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IdentExpr {
    pub name: String,
    pub ty: Ty,
}

impl IdentExpr {
    fn generate_expr(ctx: &mut Context, res_type: &Ty) -> Option<Expr> {
        ctx.choose_ident_expr_by_type(res_type).map(Expr::Ident)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TupleExpr {
    pub tuple: Vec<Expr>,
}

impl TupleExpr {
    fn generate_expr(ctx: &mut Context, res_type: &Ty) -> Option<Expr> {
        if let Ty::Tuple(types) = res_type {
            let mut res = vec![];
            for ty in types {
                for _ in 0..ctx.policy.max_expr_attempts {
                    if let Some(expr) = Expr::generate_expr(ctx, ty) {
                        res.push(expr);
                        break;
                    }
                }
            }
            Some(Expr::Tuple(TupleExpr { tuple: res }))
        } else {
            panic!()
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssignExpr {
    pub name: String,
    pub rhs: Box<Expr>,
}

impl AssignExpr {
    fn generate_expr(ctx: &mut Context, res_type: &Ty) -> Option<Expr> {
        if *res_type != Ty::unit_type() {
            return None;
        };
        let ty = ctx.choose_type();
        let mut_ident_exprs = ctx.type_symbol_table.get_mut_ident_exprs_by_type(&ty);
        if mut_ident_exprs.is_empty() {
            return None;
        }
        let ident_expr = mut_ident_exprs.choose(&mut ctx.rng).unwrap().clone();

        Some(Expr::Assign(AssignExpr {
            name: ident_expr.name,
            rhs: Box::new(Expr::generate_expr_safe(ctx, &ident_expr.ty)),
        }))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayExpr {
    pub array: Vec<Expr>,
}

impl ArrayExpr {
    fn generate_expr(ctx: &mut Context, res_type: &Ty) -> Option<Expr> {
        if let Ty::Array(ty, count) = res_type {
            let mut res = vec![];
            for _ in 0..*count {
                for _ in 0..ctx.policy.max_expr_attempts {
                    if let Some(expr) = Expr::generate_expr(ctx, ty) {
                        res.push(expr);
                        break;
                    }
                }
            }
            Some(Expr::Array(ArrayExpr { array: res }))
        } else {
            panic!()
        }
    }
}

// TODO: Add tuple here instead of literal
#[derive(Debug, Clone, Copy)]
#[non_exhaustive]
pub enum ExprKind {
    Literal,
    Binary,
    Unary,
    #[allow(dead_code)]
    Cast,
    If,
    Block,
    Ident,
    #[allow(dead_code)]
    Assign,
    __Nonexhaustive,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum EvalExprError {
    SignedOverflow,
    UnsignedOverflow,
    MinMulOverflow,
    ZeroDiv,
}

impl EvalExprError {
    pub fn overflow_error<T: PrimInt>() -> EvalExprError {
        let is_signed = T::min_value() < T::zero();
        if is_signed {
            SignedOverflow
        } else {
            UnsignedOverflow
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum EvalExpr {
    /// Literal such as `1`, `"foo"`
    Literal(LitExpr),
    Tuple(Vec<EvalExpr>),
    Unknown,
}

impl EvalExpr {
    pub fn unit_expr() -> EvalExpr {
        EvalExpr::Tuple(vec![])
    }
    pub fn cast(self, res_type: &Ty) -> EvalExpr {
        if let EvalExpr::Literal(lit_expr) = self {
            EvalExpr::Literal(lit_expr.cast(res_type))
        } else if let EvalExpr::Unknown = self {
            self
        } else {
            panic!()
        }
    }
    pub fn get_type(&self) -> Ty {
        match self {
            EvalExpr::Literal(lit) => match lit {
                LitExpr::Str(_) => todo!(),
                LitExpr::Byte(_) => todo!(),
                LitExpr::Char(_) => todo!(),
                LitExpr::Int(_, int_ty) => match int_ty {
                    Signed(t) => Ty::Int(*t),
                    Unsigned(t) => Ty::UInt(*t),
                    Unsuffixed => todo!(),
                },
                LitExpr::Float(_, _) => todo!(),
                LitExpr::Bool(_) => Ty::Bool,
            },
            _ => todo!(),
        }
    }
}

impl TryFrom<Expr> for EvalExpr {
    type Error = &'static str;

    fn try_from(value: Expr) -> Result<Self, Self::Error> {
        match value {
            Expr::Literal(lit) => Ok(EvalExpr::Literal(lit)),
            _ => Err("Unimplemented/Unsuccessful"),
        }
    }
}

#[cfg(test)]
impl EvalExpr {
    pub fn bool(b: bool) -> EvalExpr {
        EvalExpr::Literal(LitExpr::Bool(b))
    }

    pub fn i8(i: i8) -> EvalExpr {
        EvalExpr::Literal(LitExpr::Int(i as u128, LitExprTy::Signed(IntTy::I8)))
    }

    pub fn u8(u: u8) -> EvalExpr {
        EvalExpr::Literal(LitExpr::Int(u as u128, LitExprTy::Unsigned(UIntTy::U8)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::expr::EvalExprError::{
        MinMulOverflow, SignedOverflow, UnsignedOverflow, ZeroDiv,
    };
    use crate::ast::expr::{BinaryOp, EvalExprError, UnaryOp};

    #[test]
    fn unary_expr_ok_not() {
        for b in [true, false] {
            assert_eq!(
                UnaryOp::Not.apply(&EvalExpr::bool(b)),
                Ok(EvalExpr::bool(!b))
            )
        }
    }
    #[test]
    fn unary_expr_ok_neg() {
        // i = -127..=127
        for i in i8::MIN + 1..=i8::MAX {
            assert_eq!(UnaryOp::Neg.apply(&EvalExpr::i8(i)), Ok(EvalExpr::i8(-i)))
        }
    }
    #[test]
    fn unary_expr_fail_neg_signed_min_val() {
        // i = -128
        let i = i8::MIN;
        assert_eq!(
            UnaryOp::Neg.apply(&EvalExpr::i8(i)),
            Err(EvalExprError::SignedOverflow)
        )
    }

    #[test]
    fn binary_expr_ok_signed_add() {
        assert_eq!(
            BinaryOp::Add.apply(&EvalExpr::i8(-5), &EvalExpr::i8(12)),
            Ok(EvalExpr::i8(7))
        )
    }

    #[test]
    fn binary_expr_ok_unsigned_add() {
        assert_eq!(
            BinaryOp::Add.apply(&EvalExpr::u8(5), &EvalExpr::u8(12)),
            Ok(EvalExpr::u8(17))
        )
    }

    #[test]
    fn binary_expr_fail_overflow_signed_add() {
        assert_eq!(
            BinaryOp::Add.apply(&EvalExpr::i8(127), &EvalExpr::i8(127)),
            Err(EvalExprError::SignedOverflow)
        )
    }

    #[test]
    fn binary_expr_fail_overflow_unsigned_add() {
        assert_eq!(
            BinaryOp::Add.apply(&EvalExpr::u8(255), &EvalExpr::u8(255)),
            Err(EvalExprError::UnsignedOverflow)
        )
    }

    #[test]
    fn binary_expr_ok_signed_sub() {
        assert_eq!(
            BinaryOp::Sub.apply(&EvalExpr::i8(-12), &EvalExpr::i8(16)),
            Ok(EvalExpr::i8(-28))
        )
    }

    #[test]
    fn binary_expr_fail_overflow_signed_sub() {
        assert_eq!(
            BinaryOp::Sub.apply(&EvalExpr::i8(-128), &EvalExpr::i8(127)),
            Err(EvalExprError::SignedOverflow)
        )
    }

    #[test]
    fn binary_expr_ok_signed_mul() {
        assert_eq!(
            BinaryOp::Mul.apply(&EvalExpr::i8(4), &EvalExpr::i8(5)),
            Ok(EvalExpr::i8(20))
        )
    }

    #[test]
    fn binary_expr_fail_signed_min_mul_overflow() {
        assert_eq!(
            BinaryOp::Mul.apply(&EvalExpr::i8(i8::MIN), &EvalExpr::i8(-1)),
            Err(MinMulOverflow)
        )
    }

    #[test]
    fn binary_expr_fail_signed_min_mul_overflow_1() {
        assert_eq!(
            BinaryOp::Mul.apply(&EvalExpr::i8(-1), &EvalExpr::i8(i8::MIN)),
            Err(MinMulOverflow)
        )
    }

    #[test]
    fn binary_expr_fail_signed_mul_overflow() {
        assert_eq!(
            BinaryOp::Mul.apply(&EvalExpr::i8(12), &EvalExpr::i8(-15)),
            Err(SignedOverflow)
        )
    }

    #[test]
    fn binary_expr_fail_unsigned_mul_overflow() {
        assert_eq!(
            BinaryOp::Mul.apply(&EvalExpr::u8(12), &EvalExpr::u8(30)),
            Err(UnsignedOverflow)
        )
    }

    #[test]
    fn binary_expr_ok_signed_div() {
        assert_eq!(
            BinaryOp::Div.apply(&EvalExpr::i8(120), &EvalExpr::i8(4)),
            Ok(EvalExpr::i8(30))
        )
    }

    #[test]
    fn binary_expr_fail_signed_div_overflow() {
        assert_eq!(
            BinaryOp::Div.apply(&EvalExpr::i8(i8::MIN), &EvalExpr::i8(-1)),
            Err(SignedOverflow)
        )
    }

    #[test]
    fn binary_expr_fail_signed_div_zero() {
        assert_eq!(
            BinaryOp::Div.apply(&EvalExpr::i8(12), &EvalExpr::i8(0)),
            Err(ZeroDiv)
        )
    }

    #[test]
    fn cast_expr_ok() {
        let expr = LitExpr::Int(-27_i8 as u128, LitExprTy::Signed(IntTy::I8));
        assert_eq!(
            expr.cast(&Ty::UInt(UIntTy::U32))
                .cast(&Ty::UInt(UIntTy::U64)),
            LitExpr::Int(4294967269, Unsigned(UIntTy::U64))
        );
    }
}
