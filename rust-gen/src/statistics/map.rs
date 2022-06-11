use crate::ast::expr::ExprKind;
use crate::ast::item::ItemKind;
use crate::ast::op::{BinaryOp, UnaryOp};
use crate::ast::stmt::StmtKind;
use crate::ast::ty::TyKind;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

#[derive(Default, Debug, Clone, Deserialize, Serialize)]
pub struct StatisticsMap {
    pub item_counter: BTreeMap<ItemKind, usize>,
    pub stmt_counter: BTreeMap<StmtKind, usize>,
    pub expr_counter: BTreeMap<ExprKind, usize>,
    pub ty_counter: BTreeMap<TyKind, usize>,
    pub bin_op_counter: BTreeMap<BinaryOp, usize>,
    pub un_op_counter: BTreeMap<UnaryOp, usize>,
}

#[derive(Default, Debug, Clone, Deserialize, Serialize)]
pub struct FullStatisticsMap {
    pub total_items: usize,
    pub total_stmts: usize,
    pub total_exprs: usize,
    pub total_tys: usize,
    pub total_binary_ops: usize,
    pub total_unary_ops: usize,
    pub statistics_mapping: StatisticsMap,
}

impl From<StatisticsMap> for FullStatisticsMap {
    fn from(stats: StatisticsMap) -> FullStatisticsMap {
        FullStatisticsMap {
            total_items: stats.item_counter.values().sum(),
            total_stmts: stats.stmt_counter.values().sum(),
            total_exprs: stats.expr_counter.values().sum(),
            total_tys: stats.ty_counter.values().sum(),
            total_binary_ops: stats.bin_op_counter.values().sum(),
            total_unary_ops: stats.un_op_counter.values().sum(),
            statistics_mapping: stats,
        }
    }
}
