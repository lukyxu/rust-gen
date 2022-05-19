use crate::ast::ty::{PrimTy, Ty};
use serde::{Deserialize, Serialize};
use crate::context::Context;

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
        }
        .to_owned()
    }
}

impl BinaryOp {
    pub fn get_compatible_return_types(&self, ctx: &Context) -> Vec<Ty> {
        match self {
            BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Rem => {
                PrimTy::int_types(ctx).into_iter().map(From::from).collect()
            }
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
            BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Rem => {
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
