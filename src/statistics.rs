use crate::ast::expr::ExprKind;
use crate::ast::op::{BinaryOp, UnaryOp};
use crate::ast::stmt::StmtKind;
use crate::ast::ty::TyKind;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use crate::ast::item::ItemKind;

#[derive(Default, Debug, Deserialize, Serialize)]
pub struct Statistics {
    pub main_fn_stmts: usize,

    pub item_counter: HashMap<ItemKind, usize>,
    pub stmt_counter: HashMap<StmtKind, usize>,
    pub expr_counter: HashMap<ExprKind, usize>,
    pub failed_item_counter: HashMap<ItemKind, usize>,
    pub ty_counter: HashMap<TyKind, usize>,
    pub failed_expr_generations: usize,

    pub bin_op_counter: HashMap<BinaryOp, usize>,
    pub un_op_counter: HashMap<UnaryOp, usize>,
    pub max_failed_generation_depth: usize,
}

#[derive(Default, Debug, Deserialize, Serialize)]
pub struct FullStatistics {
    pub total_successful_items: usize,
    pub total_successful_stmts: usize,
    pub total_successful_exprs: usize,
    pub total_successful_tys: usize,

    #[serde(flatten)]
    pub statistics: Statistics,
}

impl From<Statistics> for FullStatistics {
    fn from(stats: Statistics) -> FullStatistics {
        FullStatistics {
            total_successful_items: stats.item_counter.values().sum(),
            total_successful_stmts: stats.stmt_counter.values().sum(),
            total_successful_exprs: stats.expr_counter.values().sum(),
            total_successful_tys: stats.ty_counter.values().sum(),
            statistics: stats,
        }
    }
}
