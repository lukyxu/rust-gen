use crate::ast::expr::{BinaryOp, ExprKind, UnaryOp};
use crate::ast::stmt::StmtKind;
use std::collections::HashMap;

#[derive(Default, Debug)]
pub struct Statistics {
    pub main_fn_stmts: usize,
    pub total_stmts: usize,
    pub stmt_counter: HashMap<StmtKind, usize>,
    pub total_exprs: usize,
    pub failed_expr_generations: usize,
    pub expr_counter: HashMap<ExprKind, usize>,
    pub bin_op_counter: HashMap<BinaryOp, usize>,
    pub un_op_counter: HashMap<UnaryOp, usize>,
    pub max_failed_generation_depth: usize,
}
