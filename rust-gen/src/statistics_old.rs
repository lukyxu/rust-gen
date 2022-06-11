use crate::ast::expr::ExprKind;
use crate::ast::item::ItemKind;
use crate::ast::op::{BinaryOp, UnaryOp};
use crate::ast::stmt::StmtKind;
use crate::ast::ty::TyKind;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use crate::statistics::generation::GenerationStatistics;

#[derive(Default, Debug, Clone, Deserialize, Serialize)]
pub struct FullStatistics {
    pub total_successful_items: usize,
    pub total_successful_stmts: usize,
    pub total_successful_exprs: usize,
    pub total_successful_tys: usize,

    pub total_failed_items: usize,
    pub total_failed_stmts: usize,
    pub total_failed_exprs: usize,
    pub total_failed_tys: usize,

    pub statistics: GenerationStatistics,
}

impl From<GenerationStatistics> for FullStatistics {
    fn from(stats: GenerationStatistics) -> FullStatistics {
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
