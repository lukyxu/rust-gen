use std::collections::BTreeMap;
use crate::ast::expr::ExprKind;
use crate::ast::item::ItemKind;
use crate::ast::op::{BinaryOp, UnaryOp};
use crate::ast::stmt::StmtKind;
use crate::ast::ty::TyKind;
use serde::{Deserialize, Serialize};

#[derive(Default, Debug, Clone, Deserialize, Serialize)]
pub struct GenerationStatistics {
    pub successful_item_counter: BTreeMap<ItemKind, usize>,
    pub successful_stmt_counter: BTreeMap<StmtKind, usize>,
    pub successful_expr_counter: BTreeMap<ExprKind, usize>,
    pub successful_ty_counter: BTreeMap<TyKind, usize>,
    pub failed_item_counter: BTreeMap<ItemKind, usize>,
    pub failed_stmt_counter: BTreeMap<StmtKind, usize>,
    pub failed_expr_counter: BTreeMap<ExprKind, usize>,
    pub failed_ty_counter: BTreeMap<TyKind, usize>,

    pub successful_bin_op_counter: BTreeMap<BinaryOp, usize>,
    pub successful_un_op_counter: BTreeMap<UnaryOp, usize>,
    pub failed_bin_op_counter: BTreeMap<BinaryOp, usize>,
    pub failed_un_op_counter: BTreeMap<UnaryOp, usize>,

    pub max_failed_item_depth: usize,
    pub max_failed_stmt_depth: usize,
    pub max_failed_expr_depth: usize,
    pub max_failed_ty_depth: usize,
}
