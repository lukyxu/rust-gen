use crate::ast::eval_expr::EvalExprError::{
    MinMulOverflow, SignedOverflow, UnsignedOverflow, ZeroDiv,
};
use crate::ast::expr::LitIntTy::{Signed, Unsigned};
use crate::ast::expr::{BinaryExpr, Expr, LitExpr, LitIntExpr, LitIntTy};
use crate::ast::op::{BinaryOp, UnaryOp};
use crate::ast::ty::IntTy::{ISize, I128, I16, I32, I64, I8};
#[cfg(test)]
use crate::ast::ty::UIntTy;
use crate::ast::ty::UIntTy::{USize, U128, U16, U32, U64, U8};
use crate::ast::ty::{PrimTy, Ty};
use num_traits::{AsPrimitive, CheckedRem, PrimInt, WrappingAdd};
use std::mem::swap;

#[derive(Debug, Clone, PartialEq)]
pub enum EvalExpr {
    /// Literal such as `1`, `"foo"`
    Literal(LitExpr),
    Tuple(EvalTupleExpr),
    Array(EvalArrayExpr),
    Struct(EvalStructExpr),
    Unknown,
}

impl EvalExpr {
    pub fn unit_expr() -> EvalExpr {
        EvalExpr::Tuple(EvalTupleExpr { tuple: vec![] })
    }
    pub fn cast(self, res_type: &Ty) -> Option<EvalExpr> {
        if let EvalExpr::Literal(lit_expr) = self {
            Some(EvalExpr::Literal(lit_expr.cast(res_type)))
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

impl From<LitExpr> for EvalExpr {
    fn from(expr: LitExpr) -> Self {
        EvalExpr::Literal(expr)
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
        EvalExpr::Literal(LitExpr::Int(LitIntExpr {
            value: i as u128,
            ty: LitIntTy::Signed(IntTy::I8),
        }))
    }

    pub fn u8(u: u8) -> EvalExpr {
        EvalExpr::Literal(LitExpr::Int(LitIntExpr {
            value: u as u128,
            ty: LitIntTy::Unsigned(UIntTy::U8),
        }))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct EvalTupleExpr {
    pub tuple: Vec<EvalExpr>,
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

impl From<EvalStructExpr> for EvalExpr {
    fn from(expr: EvalStructExpr) -> EvalExpr {
        EvalExpr::Struct(expr)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct EvalTupleStructExpr {
    pub expr: EvalTupleExpr,
}

impl From<EvalTupleStructExpr> for EvalStructExpr {
    fn from(expr: EvalTupleStructExpr) -> EvalStructExpr {
        EvalStructExpr::Tuple(expr)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct EvalFieldStructExpr {
    pub fields: Vec<EvalField>,
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
}

#[derive(Debug, Clone, PartialEq)]
pub struct EvalField {
    pub name: String,
    pub expr: EvalExpr,
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

impl LitExpr {
    pub fn cast(self, res_type: &Ty) -> LitExpr {
        if let LitExpr::Int(lit_int_expr) = self {
            match res_type {
                Ty::Prim(PrimTy::Int(s_int)) => {
                    LitIntExpr::new(s_int.recast(lit_int_expr.value), LitIntTy::Signed(*s_int))
                        .into()
                }
                Ty::Prim(PrimTy::UInt(u_int)) => {
                    LitIntExpr::new(u_int.recast(lit_int_expr.value), LitIntTy::Unsigned(*u_int))
                        .into()
                }
                _ => panic!(),
            }
        } else {
            panic!()
        }
    }
}

impl BinaryExpr {
    pub fn fix(&mut self, error: EvalExprError, lhs: &mut EvalExpr, rhs: &mut EvalExpr) {
        if let EvalExprError::UnsignedOverflow = error {
            if self.op == BinaryOp::Sub {
                swap(&mut self.lhs, &mut self.rhs);
                swap(lhs, rhs);
            }
        }
        self.op = self.replacement_op(error);
    }

    pub fn replacement_op(&self, error: EvalExprError) -> BinaryOp {
        match self.op {
            BinaryOp::Add => BinaryOp::Sub,
            BinaryOp::Sub => match error {
                SignedOverflow => BinaryOp::Add,
                UnsignedOverflow => BinaryOp::Sub,
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

macro_rules! apply_int {
    ($fn_name: ident, $op_name: ident) => {
        fn $fn_name(
            self,
            lhs_u128: u128,
            lhs: LitIntTy,
            rhs_u128: u128,
            rhs: LitIntTy,
        ) -> Result<LitExpr, EvalExprError> {
            match (lhs, rhs) {
                (Signed(I8), Signed(I8)) => i8::$op_name(lhs_u128 as i8, rhs_u128 as i8),
                (Signed(I16), Signed(I16)) => i16::$op_name(lhs_u128 as i16, rhs_u128 as i16),
                (Signed(I32), Signed(I32)) => i32::$op_name(lhs_u128 as i32, rhs_u128 as i32),
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
                _ => panic!("Mismatch type in binary operation {:?} {:?}", lhs, rhs),
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
        match (lhs, rhs) {
            (Int(lhs), Int(rhs)) => self.apply_int(lhs, rhs),
            (Bool(lhs), Bool(rhs)) => Ok(self.apply_bool(*lhs, *rhs)),
            _ => panic!("Non integer/booleans"),
        }
    }
    apply_int!(apply_add, expr_add);
    apply_int!(apply_sub, expr_sub);
    apply_int!(apply_mul, expr_mul);
    apply_int!(apply_div, expr_div);
    apply_int!(apply_rem, expr_rem);

    fn apply_int(self, lhs: &LitIntExpr, rhs: &LitIntExpr) -> Result<LitExpr, EvalExprError> {
        match self {
            BinaryOp::Add => self.apply_add(lhs.value, lhs.ty, rhs.value, rhs.ty),
            BinaryOp::Sub => self.apply_sub(lhs.value, lhs.ty, rhs.value, rhs.ty),
            BinaryOp::Mul => self.apply_mul(lhs.value, lhs.ty, rhs.value, rhs.ty),
            BinaryOp::Div => self.apply_div(lhs.value, lhs.ty, rhs.value, rhs.ty),
            BinaryOp::Rem => self.apply_rem(lhs.value, lhs.ty, rhs.value, rhs.ty),
            BinaryOp::Eq => Ok(LitExpr::Bool(lhs.value == rhs.value)),
            BinaryOp::Ne => Ok(LitExpr::Bool(lhs.value != lhs.value)),
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
            + ByLitIntTy<T>
            + CheckedRem<Output = T>,
    > Literal<T> for T
{
    fn expr_add(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError> {
        if let Some(res) = lhs.checked_add(&rhs) {
            Ok(LitIntExpr {
                value: res.as_(),
                ty: T::by_lit_expr_type(),
            }
            .into())
        } else {
            Err(EvalExprError::overflow_error::<T>())
        }
    }

    fn expr_sub(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError> {
        if let Some(res) = lhs.checked_sub(&rhs) {
            Ok(LitIntExpr {
                value: res.as_(),
                ty: T::by_lit_expr_type(),
            }
            .into())
        } else {
            Err(EvalExprError::overflow_error::<T>())
        }
    }

    fn expr_mul(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError> {
        if let Some(res) = lhs.checked_mul(&rhs) {
            Ok(LitIntExpr {
                value: res.as_(),
                ty: T::by_lit_expr_type(),
            }
            .into())
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
            Ok(LitIntExpr {
                value: res.as_(),
                ty: T::by_lit_expr_type(),
            }
            .into())
        } else if rhs == T::zero() {
            Err(ZeroDiv)
        } else {
            assert!(T::min_value() < T::zero());
            Err(SignedOverflow)
        }
    }

    fn expr_rem(lhs: T, rhs: T) -> Result<LitExpr, EvalExprError> {
        if let Some(res) = lhs.checked_rem(&rhs) {
            Ok(LitIntExpr {
                value: res.as_(),
                ty: T::by_lit_expr_type(),
            }
            .into())
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
    use crate::ast::eval_expr::EvalExprError::{
        MinMulOverflow, SignedOverflow, UnsignedOverflow, ZeroDiv,
    };
    use crate::ast::eval_expr::{EvalExpr, EvalExprError};
    use crate::ast::expr::LitIntTy::Unsigned;
    use crate::ast::expr::*;
    use crate::ast::op::{BinaryOp, UnaryOp};
    use crate::ast::ty::{IntTy, UIntTy};

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
            expr.cast(&UIntTy::U32.into()).cast(&UIntTy::U64.into()),
            LitExpr::Int(LitIntExpr {
                value: 4294967269,
                ty: Unsigned(UIntTy::U64)
            })
        );
    }
}
