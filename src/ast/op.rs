use crate::ast::ty::{PrimTy, Ty};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
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
    Eq,
    // Lq,
    // Le,
    Ne,
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
            BinaryOp::Eq => "==",
            BinaryOp::Ne => "!=",
        }
        .to_owned()
    }
}

impl BinaryOp {
    pub fn get_compatible_arg_type(&self, res_type: &Ty) -> Vec<Ty> {
        match self {
            BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Rem => {
                vec![res_type.clone()]
            }
            BinaryOp::And | BinaryOp::Or => {
                vec![Ty::Prim(PrimTy::Bool)]
            }
            BinaryOp::Eq | BinaryOp::Ne => {
                let mut y: Vec<Ty> = PrimTy::int_types().into_iter().map(From::from).collect();
                y.append(&mut vec![Ty::Prim(PrimTy::Bool)]);
                y
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
