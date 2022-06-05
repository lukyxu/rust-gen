use crate::ast::expr::ExprKind;
use crate::ast::item::ItemKind;
use crate::ast::op::{BinaryOp, UnaryOp};
use crate::ast::stmt::StmtKind;
use crate::ast::ty::TyKind;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

#[derive(Default, Debug, Clone, Deserialize, Serialize)]
pub struct Statistics {
    pub main_fn_stmts: usize,

    pub successful_item_counter: BTreeMap<ItemKind, usize>,
    pub successful_stmt_counter: BTreeMap<StmtKind, usize>,
    pub successful_expr_counter: BTreeMap<ExprKind, usize>,
    pub successful_ty_counter: BTreeMap<TyKind, usize>,
    pub failed_item_counter: BTreeMap<ItemKind, usize>,
    pub failed_stmt_counter: BTreeMap<StmtKind, usize>,
    pub failed_expr_counter: BTreeMap<ExprKind, usize>,
    pub failed_ty_counter: BTreeMap<TyKind, usize>,

    pub bin_op_counter: BTreeMap<BinaryOp, usize>,
    pub un_op_counter: BTreeMap<UnaryOp, usize>,
    pub max_failed_item_depth: usize,
    pub max_failed_stmt_depth: usize,
    pub max_failed_expr_depth: usize,
    pub max_failed_ty_depth: usize,
}

#[derive(Default, Debug, Deserialize, Serialize)]
pub struct FullStatistics {
    pub total_successful_items: usize,
    pub total_successful_stmts: usize,
    pub total_successful_exprs: usize,
    pub total_successful_tys: usize,

    pub total_failed_items: usize,
    pub total_failed_stmts: usize,
    pub total_failed_exprs: usize,
    pub total_failed_tys: usize,

    pub statistics: Statistics,
}

impl From<Statistics> for FullStatistics {
    fn from(stats: Statistics) -> FullStatistics {
        FullStatistics {
            total_successful_items: stats.successful_item_counter.values().sum(),
            total_successful_stmts: stats.successful_stmt_counter.values().sum(),
            total_successful_exprs: stats.successful_expr_counter.values().sum(),
            total_successful_tys: stats.successful_ty_counter.values().sum(),
            total_failed_items: stats.failed_item_counter.values().sum(),
            total_failed_stmts: stats.failed_stmt_counter.values().sum(),
            total_failed_exprs: stats.failed_expr_counter.values().sum(),
            total_failed_tys: stats.failed_ty_counter.values().sum(),
            statistics: stats,
        }
    }
}