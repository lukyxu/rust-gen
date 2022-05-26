use crate::ast::ty::{PrimTy, ReferenceTy, Ty};
use crate::context::Context;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
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
    // BitXor,
    // BitAnd,
    // BitOr,
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
        }
        .to_owned()
    }
}

impl BinaryOp {
    pub fn is_function_call(&self) -> bool {
        match self {
            BinaryOp::WrappingAdd
            | BinaryOp::WrappingSub
            | BinaryOp::WrappingMul
            | BinaryOp::WrappingDiv
            | BinaryOp::WrappingRem => true,
            _ => false,
        }
    }

    pub fn get_compatible_return_types(&self, ctx: &Context) -> Vec<Ty> {
        match self {
            BinaryOp::Add
            | BinaryOp::Sub
            | BinaryOp::Mul
            | BinaryOp::Div
            | BinaryOp::Rem
            | BinaryOp::WrappingAdd
            | BinaryOp::WrappingSub
            | BinaryOp::WrappingMul
            | BinaryOp::WrappingDiv
            | BinaryOp::WrappingRem => PrimTy::int_types(ctx).into_iter().map(From::from).collect(),
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
    pub fn get_compatible_arg_types(&self, res_type: &Ty, ctx: &Context) -> Vec<Ty> {
        match self {
            BinaryOp::Add
            | BinaryOp::Sub
            | BinaryOp::Mul
            | BinaryOp::Div
            | BinaryOp::Rem
            | BinaryOp::WrappingAdd
            | BinaryOp::WrappingSub
            | BinaryOp::WrappingMul
            | BinaryOp::WrappingDiv
            | BinaryOp::WrappingRem => {
                vec![res_type.clone()]
            }
            BinaryOp::And | BinaryOp::Or => {
                vec![PrimTy::Bool.into()]
            }
            BinaryOp::Eq | BinaryOp::Ne => PrimTy::int_types(ctx)
                .into_iter()
                .map(From::from)
                .chain(std::iter::once(Ty::Prim(PrimTy::Bool)))
                .collect(),
            BinaryOp::Le | BinaryOp::Lq | BinaryOp::Ge | BinaryOp::Gt => {
                PrimTy::int_types(ctx).into_iter().map(From::from).collect()
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
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
                .chain(std::iter::once(Ty::Prim(PrimTy::Bool)))
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
                Ty::Reference(ReferenceTy {
                    mutability: false,
                    lifetime: None,
                    elem: Box::new(res_type.clone()),
                }),
                Ty::Reference(ReferenceTy {
                    mutability: false,
                    lifetime: None,
                    elem: Box::new(res_type.clone()),
                }),
            ],
            UnaryOp::Not => vec![PrimTy::Bool.into()],
            UnaryOp::Neg => vec![res_type.clone()],
        }
    }
}
