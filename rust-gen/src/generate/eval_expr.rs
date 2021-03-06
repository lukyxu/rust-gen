//! Evaluated expression representation and calculations.

use crate::ast::expr::LitIntTy::{Signed, Unsigned};
use crate::ast::expr::{
    ArrayExpr, BinaryExpr, Expr, Field, FieldStructExpr, LitExpr, LitIntExpr, LitIntTy, Member,
    StructExpr, TupleExpr, TupleStructExpr,
};
use crate::ast::op::{BinaryOp, UnaryOp};
#[cfg(test)]
use crate::ast::ty::IntTy;
use crate::ast::ty::IntTy::{ISize, I128, I16, I32, I64, I8};
use crate::ast::ty::UIntTy;
use crate::ast::ty::UIntTy::{USize, U128, U16, U32, U64, U8};
use crate::ast::ty::{GTy, PrimTy, Ty};
use crate::generate::eval_expr::EvalExprError::{
    MinMulOverflow, SignedOverflow, UnsignedOverflow, ZeroDiv,
};
use crate::wrapping::{WrappingDiv, WrappingRem};
use num_traits::{
    AsPrimitive, CheckedRem, PrimInt, WrappingAdd, WrappingMul, WrappingShl, WrappingShr,
    WrappingSub,
};

use std::{i128, i32, i64, u128, u16, u64};

#[derive(Debug, Clone, PartialEq)]
/// Evaluated Rust expression
pub enum EvalExpr {
    /// Evaluated literal such as `1`, `"foo"`
    Literal(LitExpr),
    /// Evaluated tuple such as `(1_u32, "hello")`
    Tuple(EvalTupleExpr),
    /// Evaluated array such as `[1_u32, 2_u32, 3_u32]`
    Array(EvalArrayExpr),
    /// Evaluated struct such as `S { field1: value1, field2: value2 }` and `S(5_u32, "hello")`
    Struct(EvalStructExpr),
    /// Reference
    Reference(EvalReferenceExpr),
    /// Unknown evaluation (Not currently used but can be used to indicate that a value is unknown)
    Unknown,
}

impl EvalExpr {
    pub fn unit_expr() -> EvalExpr {
        EvalExpr::Tuple(EvalTupleExpr { tuple: vec![] })
    }
    pub fn cast(self, res_type: &Ty) -> Option<EvalExpr> {
        if let EvalExpr::Literal(lit_expr) = self {
            Some(EvalExpr::Literal(lit_expr.cast(res_type)?))
        } else if let EvalExpr::Unknown = self {
            Some(self)
        } else {
            None
        }
    }
    pub fn get_type(&self) -> Ty {
        match self {
            EvalExpr::Literal(lit) => match lit {
                LitExpr::Int(lit_int) => match lit_int.ty {
                    Signed(t) => t.into(),
                    Unsigned(t) => t.into(),
                },
                LitExpr::Bool(_) => PrimTy::Bool.into(),
                LitExpr::Str(_) | LitExpr::Byte(_) | LitExpr::Char(_) | LitExpr::Float(_, _) => {
                    todo!()
                }
            },
            _ => todo!(),
        }
    }
}

impl From<EvalExpr> for Expr {
    fn from(expr: EvalExpr) -> Self {
        match expr {
            EvalExpr::Literal(expr) => Expr::Literal(expr),
            EvalExpr::Tuple(expr) => Expr::Tuple(expr.into()),
            EvalExpr::Array(expr) => Expr::Array(expr.into()),
            EvalExpr::Struct(expr) => Expr::Struct(expr.into()),
            EvalExpr::Reference(_expr) => unimplemented!(),
            EvalExpr::Unknown => panic!(),
        }
    }
}

impl From<LitExpr> for EvalExpr {
    fn from(expr: LitExpr) -> EvalExpr {
        EvalExpr::Literal(expr)
    }
}

impl From<LitIntExpr> for EvalExpr {
    fn from(expr: LitIntExpr) -> EvalExpr {
        EvalExpr::Literal(expr.into())
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
        LitIntExpr::new(i as u128, IntTy::I8.into()).into()
    }

    pub fn u8(u: u8) -> EvalExpr {
        LitIntExpr::new(u as u128, LitIntTy::Unsigned(UIntTy::U8)).into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct EvalTupleExpr {
    pub tuple: Vec<EvalExpr>,
}

impl From<EvalTupleExpr> for TupleExpr {
    fn from(expr: EvalTupleExpr) -> TupleExpr {
        TupleExpr {
            tuple: expr.tuple.into_iter().map(|expr| expr.into()).collect(),
        }
    }
}

impl From<EvalTupleExpr> for EvalExpr {
    fn from(expr: EvalTupleExpr) -> EvalExpr {
        EvalExpr::Tuple(expr)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct EvalArrayExpr {
    pub array: Vec<EvalExpr>,
}

impl From<EvalArrayExpr> for ArrayExpr {
    fn from(expr: EvalArrayExpr) -> ArrayExpr {
        ArrayExpr {
            array: expr.array.into_iter().map(|expr| expr.into()).collect(),
        }
    }
}

impl From<EvalArrayExpr> for EvalExpr {
    fn from(expr: EvalArrayExpr) -> EvalExpr {
        EvalExpr::Array(expr)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum EvalStructExpr {
    Tuple(EvalTupleStructExpr),
    Field(EvalFieldStructExpr),
}

impl From<EvalStructExpr> for StructExpr {
    fn from(expr: EvalStructExpr) -> StructExpr {
        match expr {
            EvalStructExpr::Tuple(expr) => StructExpr::Tuple(expr.into()),
            EvalStructExpr::Field(expr) => StructExpr::Field(expr.into()),
        }
    }
}

impl From<EvalStructExpr> for EvalExpr {
    fn from(expr: EvalStructExpr) -> EvalExpr {
        EvalExpr::Struct(expr)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct EvalTupleStructExpr {
    pub struct_name: String,
    pub expr: EvalTupleExpr,
}

impl From<EvalTupleStructExpr> for TupleStructExpr {
    fn from(expr: EvalTupleStructExpr) -> TupleStructExpr {
        TupleStructExpr {
            struct_name: expr.struct_name,
            fields: expr.expr.into(),
        }
    }
}

impl From<EvalTupleStructExpr> for EvalStructExpr {
    fn from(expr: EvalTupleStructExpr) -> EvalStructExpr {
        EvalStructExpr::Tuple(expr)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct EvalFieldStructExpr {
    pub struct_name: String,
    pub fields: Vec<EvalField>,
}

impl From<EvalFieldStructExpr> for FieldStructExpr {
    fn from(expr: EvalFieldStructExpr) -> FieldStructExpr {
        FieldStructExpr {
            struct_name: expr.struct_name,
            fields: expr.fields.into_iter().map(|field| field.into()).collect(),
        }
    }
}

impl From<EvalFieldStructExpr> for EvalStructExpr {
    fn from(expr: EvalFieldStructExpr) -> EvalStructExpr {
        EvalStructExpr::Field(expr)
    }
}

impl EvalFieldStructExpr {
    pub fn get_field_by_name(&self, name: &str) -> Option<EvalField> {
        self.fields.iter().find(|field| field.name == name).cloned()
    }

    pub fn get_mut_field_by_name(&mut self, name: &str) -> Option<&mut EvalField> {
        self.fields.iter_mut().find(|field| field.name == name)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct EvalField {
    pub name: String,
    pub expr: EvalExpr,
}

impl From<EvalField> for Field {
    fn from(expr: EvalField) -> Field {
        Field {
            name: expr.name,
            expr: expr.expr.into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct EvalReferenceExpr {
    pub expr: Box<EvalExpr>,
}

impl From<EvalReferenceExpr> for EvalExpr {
    fn from(expr: EvalReferenceExpr) -> EvalExpr {
        EvalExpr::Reference(expr)
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum EvalExprError {
    // TODO: Add TypeError
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

impl LitExpr {
    pub fn cast(self, res_type: &Ty) -> Option<LitExpr> {
        if let LitExpr::Int(lit_int_expr) = self {
            match res_type {
                GTy::Prim(PrimTy::Int(s_int)) => Some(lit_int_expr.cast((*s_int).into()).into()),
                GTy::Prim(PrimTy::UInt(u_int)) => Some(lit_int_expr.cast((*u_int).into()).into()),
                _ => None,
            }
        } else {
            None
        }
    }
}

impl BinaryExpr {
    pub fn fix(&mut self, error: EvalExprError) {
        self.op = self.replacement_op(error);
    }

    pub fn replacement_op(&self, error: EvalExprError) -> BinaryOp {
        match self.op {
            BinaryOp::Add => match error {
                SignedOverflow => BinaryOp::Sub,
                UnsignedOverflow => BinaryOp::WrappingAdd,
                _ => panic!(),
            },
            BinaryOp::Sub => match error {
                SignedOverflow => BinaryOp::Add,
                UnsignedOverflow => BinaryOp::WrappingSub,
                _ => panic!(),
            },
            BinaryOp::Mul => {
                if let MinMulOverflow = error {
                    BinaryOp::Sub
                } else {
                    // Signed/Unsigned Overflow
                    BinaryOp::Div
                }
            }
            BinaryOp::Div | BinaryOp::Rem | BinaryOp::WrappingDiv | BinaryOp::WrappingRem => {
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

macro_rules! apply_int {
    ($fn_name: ident, $op_name: ident) => {
        fn $fn_name(self, lhs: &LitIntExpr, rhs: &LitIntExpr) -> Result<LitExpr, EvalExprError> {
            match (lhs.ty, rhs.ty) {
                (Signed(I8), Signed(I8)) => i8::$op_name(lhs.value as i8, rhs.value as i8),
                (Signed(I16), Signed(I16)) => i16::$op_name(lhs.value as i16, rhs.value as i16),
                (Signed(I32), Signed(I32)) => i32::$op_name(lhs.value as i32, rhs.value as i32),
                (Signed(I64), Signed(I64)) => i64::$op_name(lhs.value as i64, rhs.value as i64),
                (Signed(I128), Signed(I128)) => {
                    i128::$op_name(lhs.value as i128, rhs.value as i128)
                }
                (Signed(ISize), Signed(ISize)) => {
                    isize::$op_name(lhs.value as isize, rhs.value as isize)
                }
                (Unsigned(U8), Unsigned(U8)) => u8::$op_name(lhs.value as u8, rhs.value as u8),
                (Unsigned(U16), Unsigned(U16)) => u16::$op_name(lhs.value as u16, rhs.value as u16),
                (Unsigned(U32), Unsigned(U32)) => u32::$op_name(lhs.value as u32, rhs.value as u32),
                (Unsigned(U64), Unsigned(U64)) => u64::$op_name(lhs.value as u64, rhs.value as u64),
                (Unsigned(U128), Unsigned(U128)) => {
                    u128::$op_name(lhs.value as u128, rhs.value as u128)
                }
                (Unsigned(USize), Unsigned(USize)) => {
                    usize::$op_name(lhs.value as usize, rhs.value as usize)
                }
                _ => panic!("Mismatch type in binary operation {:?} {:?}", lhs, rhs),
            }
        }
    };
}

macro_rules! apply_int_f_rhs {
    ($fn_name: ident, $op_name: ident, $rhs_ty: ty) => {
        fn $fn_name(self, lhs: &LitIntExpr, rhs: $rhs_ty) -> Result<LitExpr, EvalExprError> {
            match lhs.ty {
                Signed(I8) => i8::$op_name(lhs.value as i8, rhs),
                Signed(I16) => i16::$op_name(lhs.value as i16, rhs),
                Signed(I32) => i32::$op_name(lhs.value as i32, rhs),
                Signed(I64) => i64::$op_name(lhs.value as i64, rhs),
                Signed(I128) => i128::$op_name(lhs.value as i128, rhs),
                Signed(ISize) => isize::$op_name(lhs.value as isize, rhs),
                Unsigned(U8) => u8::$op_name(lhs.value as u8, rhs),
                Unsigned(U16) => u16::$op_name(lhs.value as u16, rhs),
                Unsigned(U32) => u32::$op_name(lhs.value as u32, rhs),
                Unsigned(U64) => u64::$op_name(lhs.value as u64, rhs),
                Unsigned(U128) => u128::$op_name(lhs.value as u128, rhs),
                Unsigned(USize) => usize::$op_name(lhs.value as usize, rhs),
            }
        }
    };
}

impl BinaryOp {
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
            (BinaryOp::Div | BinaryOp::Rem, _, EvalExpr::Literal(rhs)) => {
                if let LitExpr::Int(LitIntExpr { value: 0, .. }) = rhs {
                    Err(ZeroDiv)
                } else if let LitExpr::Int(_) = rhs {
                    Ok(EvalExpr::Literal(rhs.clone()))
                } else {
                    Ok(EvalExpr::Unknown)
                }
            }
            _ => Ok(EvalExpr::Unknown),
        }
    }

    pub fn apply_lit(self, lhs: &LitExpr, rhs: &LitExpr) -> Result<LitExpr, EvalExprError> {
        use LitExpr::{Bool, Int};
        match (lhs, rhs, self) {
            (
                Int(lhs),
                Int(LitIntExpr {
                    ty: LitIntTy::Unsigned(UIntTy::U32),
                    value,
                }),
                BinaryOp::WrappingShl,
            ) => self.apply_wrapping_shl(lhs, *value as u32),
            (
                Int(lhs),
                Int(LitIntExpr {
                    ty: LitIntTy::Unsigned(UIntTy::U32),
                    value,
                }),
                BinaryOp::WrappingShr,
            ) => {
                // self.apply_wrapping_shr
                self.apply_wrapping_shr(lhs, *value as u32)
            }
            (Int(lhs), Int(rhs), _) => self.apply_int(lhs, rhs),
            (Bool(lhs), Bool(rhs), _) => Ok(self.apply_bool(*lhs, *rhs)),
            _ => panic!("Non integer/booleans"),
        }
    }
    apply_int!(apply_add, expr_add);
    apply_int!(apply_sub, expr_sub);
    apply_int!(apply_mul, expr_mul);
    apply_int!(apply_div, expr_div);
    apply_int!(apply_rem, expr_rem);
    apply_int!(apply_bit_xor, expr_bit_xor);
    apply_int!(apply_bit_and, expr_bit_and);
    apply_int!(apply_bit_or, expr_bit_or);
    apply_int!(apply_eq, expr_eq);
    apply_int!(apply_ne, expr_ne);
    apply_int!(apply_lq, expr_lq);
    apply_int!(apply_le, expr_le);
    apply_int!(apply_ge, expr_ge);
    apply_int!(apply_gt, expr_gt);
    apply_int!(apply_wrapping_add, expr_wrapping_add);
    apply_int!(apply_wrapping_sub, expr_wrapping_sub);
    apply_int!(apply_wrapping_mul, expr_wrapping_mul);
    apply_int!(apply_wrapping_div, expr_wrapping_div);
    apply_int!(apply_wrapping_rem, expr_wrapping_rem);

    fn apply_int(self, lhs: &LitIntExpr, rhs: &LitIntExpr) -> Result<LitExpr, EvalExprError> {
        assert!(!(lhs.ty != rhs.ty), "Incompatible types");
        match self {
            BinaryOp::Add => self.apply_add(lhs, rhs),
            BinaryOp::Sub => self.apply_sub(lhs, rhs),
            BinaryOp::Mul => self.apply_mul(lhs, rhs),
            BinaryOp::Div => self.apply_div(lhs, rhs),
            BinaryOp::Rem => self.apply_rem(lhs, rhs),
            BinaryOp::BitXor => self.apply_bit_xor(lhs, rhs),
            BinaryOp::BitAnd => self.apply_bit_and(lhs, rhs),
            BinaryOp::BitOr => self.apply_bit_or(lhs, rhs),
            BinaryOp::Eq => self.apply_eq(lhs, rhs),
            BinaryOp::Ne => self.apply_ne(lhs, rhs),
            BinaryOp::Lq => self.apply_lq(lhs, rhs),
            BinaryOp::Le => self.apply_le(lhs, rhs),
            BinaryOp::Ge => self.apply_ge(lhs, rhs),
            BinaryOp::Gt => self.apply_gt(lhs, rhs),
            BinaryOp::WrappingAdd => self.apply_wrapping_add(lhs, rhs),
            BinaryOp::WrappingSub => self.apply_wrapping_sub(lhs, rhs),
            BinaryOp::WrappingMul => self.apply_wrapping_mul(lhs, rhs),
            BinaryOp::WrappingDiv => self.apply_wrapping_div(lhs, rhs),
            BinaryOp::WrappingRem => self.apply_wrapping_rem(lhs, rhs),

            _ => panic!("Undefined operation on ints"),
        }
    }

    fn apply_bool(self, lhs: bool, rhs: bool) -> LitExpr {
        match self {
            BinaryOp::And => LitExpr::Bool(lhs && rhs),
            BinaryOp::Or => LitExpr::Bool(lhs || rhs),
            BinaryOp::Eq => LitExpr::Bool(lhs == rhs),
            BinaryOp::Ne => LitExpr::Bool(lhs != rhs),
            _ => panic!(),
        }
    }

    apply_int_f_rhs!(apply_wrapping_shl, expr_wrapping_shl, u32);
    apply_int_f_rhs!(apply_wrapping_shr, expr_wrapping_shr, u32);
}

trait Literal<
    T: PrimInt
        + Copy
        + AsPrimitive<u128>
        + WrappingAdd<Output = T>
        + ByLitIntTy<T>
        + CheckedRem<Output = T>,
>
{
    fn expr_add(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError>;
    fn expr_sub(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError>;
    fn expr_mul(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError>;
    fn expr_div(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError>;
    fn expr_rem(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError>;
    fn expr_bit_xor(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError>;
    fn expr_bit_and(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError>;
    fn expr_bit_or(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError>;
    fn expr_eq(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError>;
    fn expr_ne(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError>;
    fn expr_lq(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError>;
    fn expr_le(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError>;
    fn expr_ge(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError>;
    fn expr_gt(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError>;
    fn expr_wrapping_add(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError>;
    fn expr_wrapping_sub(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError>;
    fn expr_wrapping_mul(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError>;
    fn expr_wrapping_div(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError>;
    fn expr_wrapping_rem(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError>;
    fn expr_wrapping_shl(lhs: T, rhs: u32) -> Result<LitExpr, EvalExprError>;
    fn expr_wrapping_shr(lhs: T, rhs: u32) -> Result<LitExpr, EvalExprError>;
}

trait ByLitIntTy<T> {
    fn by_lit_expr_type() -> LitIntTy;
}

macro_rules! by_lit_expr_ty_impl {
    ($rust_ty: ident, $ty: expr) => {
        impl ByLitIntTy<$rust_ty> for $rust_ty {
            fn by_lit_expr_type() -> LitIntTy {
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
            + WrappingSub<Output = T>
            + WrappingMul<Output = T>
            + WrappingDiv<Output = T>
            + WrappingRem<Output = T>
            + WrappingShl<Output = T>
            + WrappingShr<Output = T>
            + ByLitIntTy<T>
            + CheckedRem<Output = T>,
    > Literal<T> for T
{
    fn expr_add(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError> {
        if let Some(res) = lhs.checked_add(&rhs) {
            Ok(LitIntExpr::new(res.as_(), T::by_lit_expr_type()).into())
        } else {
            Err(EvalExprError::overflow_error::<T>())
        }
    }

    fn expr_sub(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError> {
        if let Some(res) = lhs.checked_sub(&rhs) {
            Ok(LitIntExpr::new(res.as_(), T::by_lit_expr_type()).into())
        } else {
            Err(EvalExprError::overflow_error::<T>())
        }
    }

    fn expr_mul(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError> {
        if let Some(res) = lhs.checked_mul(&rhs) {
            Ok(LitIntExpr::new(res.as_(), T::by_lit_expr_type()).into())
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
            Ok(LitIntExpr::new(res.as_(), T::by_lit_expr_type()).into())
        } else if rhs == T::zero() {
            Err(ZeroDiv)
        } else {
            assert!(T::min_value() < T::zero());
            Err(SignedOverflow)
        }
    }

    fn expr_rem(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError> {
        if let Some(res) = lhs.checked_rem(&rhs) {
            Ok(LitIntExpr::new(res.as_(), T::by_lit_expr_type()).into())
        } else if rhs == T::zero() {
            Err(ZeroDiv)
        } else {
            assert!(T::min_value() < T::zero());
            Err(SignedOverflow)
        }
    }

    fn expr_bit_xor(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError> {
        Ok(LitIntExpr::new(lhs.bitxor(rhs).as_(), T::by_lit_expr_type()).into())
    }

    fn expr_bit_and(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError> {
        Ok(LitIntExpr::new(lhs.bitand(rhs).as_(), T::by_lit_expr_type()).into())
    }

    fn expr_bit_or(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError> {
        Ok(LitIntExpr::new(lhs.bitor(rhs).as_(), T::by_lit_expr_type()).into())
    }

    fn expr_eq(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError> {
        Ok(LitExpr::Bool(lhs == rhs))
    }

    fn expr_ne(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError> {
        Ok(LitExpr::Bool(lhs != rhs))
    }

    fn expr_lq(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError> {
        Ok(LitExpr::Bool(lhs < rhs))
    }

    fn expr_le(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError> {
        Ok(LitExpr::Bool(lhs <= rhs))
    }

    fn expr_ge(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError> {
        Ok(LitExpr::Bool(lhs >= rhs))
    }

    fn expr_gt(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError> {
        Ok(LitExpr::Bool(lhs > rhs))
    }

    fn expr_wrapping_add(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError> {
        Ok(LitIntExpr::new(lhs.wrapping_add(&rhs).as_(), T::by_lit_expr_type()).into())
    }
    fn expr_wrapping_sub(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError> {
        Ok(LitIntExpr::new(lhs.wrapping_sub(&rhs).as_(), T::by_lit_expr_type()).into())
    }
    fn expr_wrapping_mul(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError> {
        Ok(LitIntExpr::new(lhs.wrapping_mul(&rhs).as_(), T::by_lit_expr_type()).into())
    }
    fn expr_wrapping_div(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError> {
        if rhs.is_zero() {
            return Err(ZeroDiv);
        }
        Ok(LitIntExpr::new(lhs.wrapping_div(&rhs).as_(), T::by_lit_expr_type()).into())
    }
    fn expr_wrapping_rem(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError> {
        if rhs.is_zero() {
            return Err(ZeroDiv);
        }
        Ok(LitIntExpr::new(lhs.wrapping_rem(&rhs).as_(), T::by_lit_expr_type()).into())
    }
    fn expr_wrapping_shl(lhs: T, rhs: u32) -> Result<LitExpr, EvalExprError> {
        Ok(LitIntExpr::new(lhs.wrapping_shl(rhs).as_(), T::by_lit_expr_type()).into())
    }
    fn expr_wrapping_shr(lhs: T, rhs: u32) -> Result<LitExpr, EvalExprError> {
        Ok(LitIntExpr::new(lhs.wrapping_shr(rhs).as_(), T::by_lit_expr_type()).into())
    }
}

pub enum EvalPlaceExpr {
    Ident(String),
    Field(Box<EvalPlaceExpr>, Member),
    Index(Box<EvalPlaceExpr>, usize),
}

impl EvalPlaceExpr {
    pub fn name(&self) -> String {
        match self {
            EvalPlaceExpr::Ident(name) => name.clone(),
            EvalPlaceExpr::Field(expr, _) | EvalPlaceExpr::Index(expr, _) => expr.name(),
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

impl UnaryOp {
    pub fn apply(self, expr: &EvalExpr) -> Result<EvalExpr, EvalExprError> {
        match self {
            UnaryOp::Deref => {
                if let EvalExpr::Reference(reference_expr) = expr {
                    Ok(*reference_expr.expr.clone())
                } else {
                    panic!()
                }
            }
            UnaryOp::Not => {
                if let EvalExpr::Literal(LitExpr::Bool(bool)) = *expr {
                    Ok(EvalExpr::Literal(LitExpr::Bool(!bool)))
                } else {
                    panic!()
                }
            }
            UnaryOp::Neg => {
                // LitExpr::Int(u128, ty @ Signed(int_type))
                if let EvalExpr::Literal(LitExpr::Int(LitIntExpr {
                    value: u128,
                    ty: ty @ Signed(int_type),
                })) = expr
                {
                    let (u128, ty) = (*u128, *ty);
                    match int_type {
                        ISize => isize::checked_neg(u128 as isize)
                            .map(|isize| {
                                EvalExpr::Literal(LitIntExpr::new(isize as u128, ty).into())
                            })
                            .ok_or(EvalExprError::SignedOverflow),
                        I8 => i8::checked_neg(u128 as i8)
                            .map(|i8| EvalExpr::Literal(LitIntExpr::new(i8 as u128, ty).into()))
                            .ok_or(EvalExprError::SignedOverflow),
                        I16 => i16::checked_neg(u128 as i16)
                            .map(|i16| EvalExpr::Literal(LitIntExpr::new(i16 as u128, ty).into()))
                            .ok_or(EvalExprError::SignedOverflow),
                        I32 => i32::checked_neg(u128 as i32)
                            .map(|i32| EvalExpr::Literal(LitIntExpr::new(i32 as u128, ty).into()))
                            .ok_or(EvalExprError::SignedOverflow),
                        I64 => i64::checked_neg(u128 as i64)
                            .map(|i64| EvalExpr::Literal(LitIntExpr::new(i64 as u128, ty).into()))
                            .ok_or(EvalExprError::SignedOverflow),
                        I128 => i128::checked_neg(u128 as i128)
                            .map(|i128| EvalExpr::Literal(LitIntExpr::new(i128 as u128, ty).into()))
                            .ok_or(EvalExprError::SignedOverflow),
                    }
                } else if let EvalExpr::Unknown = expr {
                    Ok(EvalExpr::Unknown)
                } else {
                    panic!()
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::expr::LitIntTy::Unsigned;
    use crate::ast::expr::*;
    use crate::ast::op::{BinaryOp, UnaryOp};
    use crate::ast::ty::{IntTy, UIntTy};
    use crate::generate::eval_expr::EvalExprError::{
        MinMulOverflow, SignedOverflow, UnsignedOverflow, ZeroDiv,
    };
    use crate::generate::eval_expr::{EvalExpr, EvalExprError};

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
        let expr: LitExpr = LitIntExpr::new(-27_i8 as u128, LitIntTy::Signed(IntTy::I8)).into();
        assert_eq!(
            expr.cast(&UIntTy::U32.into())
                .expect("Unable to cast")
                .cast(&UIntTy::U64.into())
                .expect("Unable to cast"),
            LitExpr::Int(LitIntExpr {
                value: 4294967269,
                ty: Unsigned(UIntTy::U64)
            })
        );
    }
}
