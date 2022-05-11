use crate::ast::expr::ExprKind;
use crate::ast::stmt::StmtKind;
use crate::ast::ty::TyKind;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use crate::ast::op::{BinaryOp, UnaryOp};

#[derive(Default, Debug, Deserialize, Serialize)]
pub struct Statistics {
    pub main_fn_stmts: usize,

    pub stmt_counter: HashMap<StmtKind, usize>,
    pub expr_counter: HashMap<ExprKind, usize>,
    pub ty_counter: HashMap<TyKind, usize>,
    pub failed_expr_generations: usize,

    pub bin_op_counter: HashMap<BinaryOp, usize>,
    pub un_op_counter: HashMap<UnaryOp, usize>,
    pub max_failed_generation_depth: usize,
}

#[derive(Default, Debug, Deserialize, Serialize)]
pub struct FullStatistics {
    pub total_stmts: usize,
    pub total_exprs: usize,
    pub total_tys: usize,

    #[serde(flatten)]
    pub statistics: Statistics,
}

impl From<Statistics> for FullStatistics {
    fn from(stats: Statistics) -> FullStatistics {
        FullStatistics {
            total_stmts: stats.stmt_counter.values().sum(),
            total_exprs: stats.expr_counter.values().sum(),
            total_tys: stats.ty_counter.values().sum(),
            statistics: stats,
        }
    }
}
