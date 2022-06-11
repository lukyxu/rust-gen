use std::collections::BTreeMap;
use crate::ast::expr::ExprKind;
use crate::ast::item::ItemKind;
use crate::ast::op::{BinaryOp, UnaryOp};
use crate::ast::stmt::StmtKind;
use crate::ast::ty::TyKind;
use serde::{Deserialize, Serialize};

#[derive(Default, Debug, Clone, Deserialize, Serialize)]
pub struct StatisticsMapping {
    pub successful_item_counter: BTreeMap<ItemKind, usize>,
    pub successful_stmt_counter: BTreeMap<StmtKind, usize>,
    pub successful_expr_counter: BTreeMap<ExprKind, usize>,
    pub successful_ty_counter: BTreeMap<TyKind, usize>,

    pub bin_op_counter: BTreeMap<BinaryOp, usize>,
    pub un_op_counter: BTreeMap<UnaryOp, usize>,
}
