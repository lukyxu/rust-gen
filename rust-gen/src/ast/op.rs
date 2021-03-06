//! Unary and binary operation nodes.

use crate::ast::ty::{GTy, PrimTy, ReferenceTy, Ty, UIntTy};
use crate::context::Context;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
#[non_exhaustive]
/// Represents a binary operation between two arguments of a valid type.
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    And,
    Or,
    BitXor,
    BitAnd,
    BitOr,
    // Shl,
    // Shr,
    Eq,
    Lq,
    Le,
    Ne,
    Ge,
    Gt,

    WrappingAdd,
    WrappingSub,
    WrappingMul,
    WrappingDiv,
    WrappingRem,
    WrappingShl,
    WrappingShr,
}

impl ToString for BinaryOp {
    fn to_string(&self) -> String {
        match self {
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::Rem => "%",
            BinaryOp::BitXor => "^",
            BinaryOp::BitAnd => "&",
            BinaryOp::BitOr => "|",
            BinaryOp::And => "&&",
            BinaryOp::Or => "||",
            BinaryOp::Eq => "==",
            BinaryOp::Lq => "<",
            BinaryOp::Le => "<=",
            BinaryOp::Ne => "!=",
            BinaryOp::Ge => ">=",
            BinaryOp::Gt => ">",
            BinaryOp::WrappingAdd => "wrapping_add",
            BinaryOp::WrappingSub => "wrapping_sub",
            BinaryOp::WrappingMul => "wrapping_mul",
            BinaryOp::WrappingDiv => "wrapping_div",
            BinaryOp::WrappingRem => "wrapping_rem",
            BinaryOp::WrappingShl => "wrapping_shl",
            BinaryOp::WrappingShr => "wrapping_shr",
        }
        .to_owned()
    }
}

impl BinaryOp {
    pub fn is_function_call(&self) -> bool {
        matches!(
            self,
            BinaryOp::WrappingAdd
                | BinaryOp::WrappingSub
                | BinaryOp::WrappingMul
                | BinaryOp::WrappingDiv
                | BinaryOp::WrappingRem
                | BinaryOp::WrappingShl
                | BinaryOp::WrappingShr
        )
    }

    pub fn get_compatible_return_types(&self, ctx: &Context) -> Vec<Ty> {
        match self {
            BinaryOp::Add
            | BinaryOp::Sub
            | BinaryOp::Mul
            | BinaryOp::Div
            | BinaryOp::Rem
            | BinaryOp::BitXor
            | BinaryOp::BitAnd
            | BinaryOp::BitOr
            | BinaryOp::WrappingAdd
            | BinaryOp::WrappingSub
            | BinaryOp::WrappingMul
            | BinaryOp::WrappingDiv
            | BinaryOp::WrappingRem
            | BinaryOp::WrappingShl
            | BinaryOp::WrappingShr => PrimTy::int_types(ctx).into_iter().map(From::from).collect(),
            BinaryOp::And
            | BinaryOp::Or
            | BinaryOp::Eq
            | BinaryOp::Ne
            | BinaryOp::Le
            | BinaryOp::Lq
            | BinaryOp::Ge
            | BinaryOp::Gt => {
                vec![PrimTy::Bool.into()]
            }
        }
    }

    /// Returns compatible argument types of a binary operation for a given operation.
    /// Both arguments in binary operation must be of the same type.
    pub fn get_compatible_lhs_arg_types(&self, res_type: &Ty, ctx: &Context) -> Vec<Ty> {
        match self {
            BinaryOp::Add
            | BinaryOp::Sub
            | BinaryOp::Mul
            | BinaryOp::Div
            | BinaryOp::Rem
            | BinaryOp::BitXor
            | BinaryOp::BitAnd
            | BinaryOp::BitOr
            | BinaryOp::WrappingAdd
            | BinaryOp::WrappingSub
            | BinaryOp::WrappingMul
            | BinaryOp::WrappingDiv
            | BinaryOp::WrappingRem
            | BinaryOp::WrappingShl
            | BinaryOp::WrappingShr => {
                vec![res_type.clone()]
            }
            BinaryOp::And | BinaryOp::Or => {
                vec![PrimTy::Bool.into()]
            }
            BinaryOp::Eq | BinaryOp::Ne => PrimTy::int_types(ctx)
                .into_iter()
                .map(From::from)
                .chain(std::iter::once(GTy::Prim(PrimTy::Bool)))
                .collect(),
            BinaryOp::Le | BinaryOp::Lq | BinaryOp::Ge | BinaryOp::Gt => {
                PrimTy::int_types(ctx).into_iter().map(From::from).collect()
            }
        }
    }

    pub fn get_compatible_rhs_arg_types(&self, lhs_arg: &Ty) -> Vec<Ty> {
        match self {
            BinaryOp::WrappingShl | BinaryOp::WrappingShr => {
                vec![UIntTy::U32.into()]
            }
            _ => vec![lhs_arg.clone()],
        }
    }

    pub fn can_short_circuit(&self) -> bool {
        match self {
            BinaryOp::And | BinaryOp::Or => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
pub enum UnaryOp {
    // TODO: Deref when adding pointer types
    #[allow(dead_code)]
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
    pub fn get_compatible_return_types(&self, ctx: &Context) -> Vec<Ty> {
        match self {
            UnaryOp::Deref => PrimTy::int_types(ctx)
                .into_iter()
                .map(From::from)
                .chain(std::iter::once(GTy::Prim(PrimTy::Bool)))
                .collect(),
            UnaryOp::Not => vec![PrimTy::Bool.into()],
            UnaryOp::Neg => PrimTy::int_types(ctx)
                .into_iter()
                .filter(|x| matches!(x, PrimTy::Int(_)))
                .map(From::from)
                .collect(),
        }
    }

    pub fn get_compatible_arg_types(&self, res_type: &Ty) -> Vec<Ty> {
        match self {
            UnaryOp::Deref => vec![
                GTy::Reference(ReferenceTy::new(res_type.clone(), false, None)),
                GTy::Reference(ReferenceTy::new(res_type.clone(), false, None)),
            ],
            UnaryOp::Not => vec![PrimTy::Bool.into()],
            UnaryOp::Neg => vec![res_type.clone()],
        }
    }
}
