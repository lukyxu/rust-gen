use crate::ast::expr::EvalExprError::{MinMulOverflow, Overflow, ZeroDiv};
use crate::ast::expr::LitExprTy::{Signed, Unsigned, Unsuffixed};
use crate::ast::stmt::Stmt;
use crate::ast::ty::IntTy::*;

use crate::ast::ty::UIntTy::*;
use crate::ast::ty::{FloatTy, IntTy, Ty, UIntTy};
use crate::Context;
use rand::prelude::SliceRandom;
use std::{isize, u32, usize};
use num_traits::{AsPrimitive, PrimInt, WrappingAdd};

#[derive(Debug, Clone)]
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
    Tuple(TupleExpr),
    Assign(AssignExpr),
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
            tuple @ Ty::Tuple(_) => TupleExpr::generate_expr(ctx, tuple),
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

#[derive(Debug, Clone)]
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
            Ty::Bool => {
                ctx.choose_binary_bool_op()
            }
            Ty::Int(_) => {
                ctx.choose_binary_int_op()
            }
            // TODO: UInt binary expressions
            Ty::Tuple(_) | Ty::UInt(_) => return None,
            _ => panic!(),
        };
        let lhs = Box::new(Expr::generate_expr(ctx, res_type)?);
        let rhs = Box::new(Expr::generate_expr(ctx, res_type)?);
        Some(Expr::Binary(BinaryExpr { lhs, rhs, op }))
    }

    pub fn replacement_op(&self, error: &EvalExprError) -> BinaryOp {
        match self.op {
            BinaryOp::Add => BinaryOp::Sub,
            BinaryOp::Sub => BinaryOp::Add,
            BinaryOp::Mul => {
                if let EvalExprError::MinMulOverflow = error {
                    BinaryOp::Sub
                } else {
                    BinaryOp::Div
                }
            }
            BinaryOp::Div => {
                if let EvalExprError::ZeroDiv = error {
                    BinaryOp::Mul
                } else {
                    BinaryOp::Sub
                }
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
    // Rem,
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
        return short_circuit_and || short_circuit_or;
    }

    pub fn apply_res_expr(self, lhs: &EvalExpr, rhs: &EvalExpr) -> Result<EvalExpr, EvalExprError> {
        if let (EvalExpr::Literal(lhs), EvalExpr::Literal(rhs)) = (lhs, rhs) {
            let res: Result<LitExpr, EvalExprError> = self.apply(lhs, rhs);
            return match res {
                Ok(lit_expr) => Ok(EvalExpr::Literal(lit_expr)),
                Err(error) => Err(error),
            };
        } else if let (BinaryOp::Div, EvalExpr::Literal(rhs)) = (&self, rhs) {
            // Special case when rhs evaluates to zero but lhs is unknown
            // TODO: Tidy this code up
            if let LitExpr::Int(0, _) = rhs {
                return Err(ZeroDiv);
            } else if let LitExpr::Int(_, _) = rhs {
                return Ok(EvalExpr::Literal(rhs.clone()));
            };
        };
        return Ok(EvalExpr::Unknown);
    }

    pub fn apply(self, lhs: &LitExpr, rhs: &LitExpr) -> Result<LitExpr, EvalExprError> {
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

trait Literal<T: PrimInt + Copy + AsPrimitive<u128> + WrappingAdd<Output = T> + ByLitExprTy<T>> {
    fn expr_add(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError>;
    fn expr_sub(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError>;
    fn expr_mul(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError>;
    fn expr_div(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError>;
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

impl <T: PrimInt + Copy + AsPrimitive<u128> + WrappingAdd<Output = T> + ByLitExprTy<T>> Literal<T> for T {
    fn expr_add(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError> {
        if let Some(res) = lhs.checked_add(&rhs) {
            Ok(LitExpr::Int(res.as_(), T::by_lit_expr_type()))
        } else {
            Err(Overflow)
        }
    }

    fn expr_sub(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError> {
        if let Some(res) = lhs.checked_sub(&rhs) {
            Ok(LitExpr::Int(res.as_(), T::by_lit_expr_type()))
        } else {
            Err(Overflow)
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
                Err(Overflow)
            }
        }
    }

    fn expr_div(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError> {
        if let Some(res) = lhs.checked_div(&rhs) {
            Ok(LitExpr::Int(res.as_(), T::by_lit_expr_type()))
        } else {
            if rhs == T::zero() {
                Err(ZeroDiv)
            } else {
                Err(Overflow)
            }
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

#[derive(Debug, Clone)]
pub struct UnaryExpr {
    pub expr: Box<Expr>,
    pub op: UnaryOp,
}

impl UnaryExpr {
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
    pub fn generate_expr_internal(ctx: &mut Context, res_type: &Ty) -> Option<Expr> {
        match res_type {
            Ty::Bool | Ty::Int(_) | Ty::UInt(_) => {
                Expr::generate_expr(ctx, res_type)
            }
            _ => {
                None
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    // TODO: Deref when adding pointer types
    Deref,
    Not,
    Neg,
}

#[derive(Debug, Clone)]
pub struct CastExpr {
    pub expr: Box<Expr>,
    pub ty: Ty,
}

// TODO: Improve IfExpr formatting in printing
#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct BlockExpr {
    pub stmts: Vec<Stmt>,
}

impl BlockExpr {
    // TODO: Make this return an optional
    pub fn generate_expr(ctx: &mut Context, res_type: &Ty) -> Option<Expr> {
        let block_expr = BlockExpr::generate_block_expr(ctx, res_type);
        match block_expr {
            None => None,
            Some(block_expr) => Some(Expr::Block(block_expr)),
        }
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

#[derive(Debug, Clone)]
pub struct TupleExpr {
    pub tuple: Vec<Box<Expr>>,
}

impl TupleExpr {
    fn generate_expr(ctx: &mut Context, res_type: &Ty) -> Option<Expr> {
        if let Ty::Tuple(types) = res_type {
            let mut res = vec![];
            for ty in types {
                for _ in 0..ctx.policy.max_expr_attempts {
                    if let Some(expr) = Expr::generate_expr(ctx, ty) {
                        res.push(Box::new(expr));
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

#[derive(Debug, Clone)]
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

// TODO: Add tuple here instead of literal
#[derive(Debug, Clone, Copy)]
pub enum ExprKind {
    Literal,
    Binary,
    Unary,
    Cast,
    If,
    Block,
    Ident,
    Assign,
}

#[derive(Debug, Copy, Clone)]
pub enum EvalExprError {
    Overflow,
    MinMulOverflow,
    ZeroDiv,
}

#[derive(Debug, Clone)]
pub enum EvalExpr {
    /// Literal such as `1`, `"foo"`
    Literal(LitExpr),
    Tuple(Vec<EvalExpr>),
    Unknown,
}

impl EvalExpr {
    pub fn unit_expr() -> EvalExpr {
        return EvalExpr::Tuple(vec![]);
    }
}
