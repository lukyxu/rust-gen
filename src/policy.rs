use crate::ast::expr::{BinaryOp, ExprKind};
use crate::ast::stmt::StmtKind;
use crate::ast::ty::{IntTy, Ty, UIntTy};
use rand::distributions::Uniform;

pub struct Policy {
    pub stmt_dist: Vec<(StmtKind, f64)>,
    pub type_dist: Vec<(Ty, f64)>,
    pub expr_dist: Vec<(ExprKind, f64)>,
    pub binary_int_op_dist: Vec<(BinaryOp, f64)>,
    pub binary_bool_op_dist: Vec<(BinaryOp, f64)>,
    // TODO: See if we can make this distribution generalized
    pub num_stmt_dist: Uniform<usize>,

    pub unsuffixed_int_prob: f64,
    pub otherwise_if_stmt_prob: f64,
    pub bool_true_prob: f64,

    pub max_if_else_depth: u32,
    // pub max_block_depth: u32,
    pub max_arith_depth: u32,
    pub max_expr_attempts: u32,
}

impl Default for Policy {
    // TODO: Add assertions in policy after
    fn default() -> Self {
        Policy {
            stmt_dist: vec![
                (StmtKind::Local, 5.0),
                (StmtKind::Semi, 1.0),
                // (StmtKind::Expr, 0.0): Must be 0
            ],
            type_dist: vec![
                (Ty::Int(IntTy::I8), 3.0),
                (Ty::Int(IntTy::I16), 3.0),
                (Ty::Int(IntTy::I32), 3.0),
                (Ty::Int(IntTy::I64), 1.0),
                (Ty::Int(IntTy::ISize), 1.0),
                (Ty::UInt(UIntTy::U8), 3.0),
                (Ty::UInt(UIntTy::U16), 3.0),
                (Ty::UInt(UIntTy::U32), 3.0),
                (Ty::UInt(UIntTy::U64), 1.0),
                (Ty::UInt(UIntTy::USize), 1.0),
            ],
            expr_dist: vec![
                (ExprKind::Literal, 3.0),
                (ExprKind::If, 2.0),
                (ExprKind::Binary, 2.0),
                (ExprKind::Ident, 2.0),
                (ExprKind::Block, 0.0),
                // (ExprKind::Unary, 1.0),
            ],
            binary_int_op_dist: vec![
                (BinaryOp::Add, 1.0),
                (BinaryOp::Sub, 1.0),
                (BinaryOp::Mul, 1.0),
                (BinaryOp::Div, 1.0),
            ],
            binary_bool_op_dist: vec![(BinaryOp::And, 1.0), (BinaryOp::Or, 1.0)],
            num_stmt_dist: Uniform::new_inclusive(5, 10),
            unsuffixed_int_prob: 0.5,
            otherwise_if_stmt_prob: 0.5,
            bool_true_prob: 0.5,

            max_if_else_depth: 3,
            // TODO: Add max block depth
            // max_block_depth: 4,
            max_arith_depth: 5,
            max_expr_attempts: 50,
        }
    }
}