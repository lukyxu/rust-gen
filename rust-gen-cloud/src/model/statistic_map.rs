use crate::model::last_insert_id;
use crate::schema::statistics_map;
use chrono::NaiveDateTime;
use diesel::{insert_into, select, MysqlConnection, RunQueryDsl};
use rust_gen::ast::expr::ExprKind;
use rust_gen::ast::item::ItemKind;
use rust_gen::ast::op::{BinaryOp, UnaryOp};
use rust_gen::ast::stmt::StmtKind;
use rust_gen::ast::ty::TyKind;
use rust_gen::statistics::map::FullStatisticsMap;
use std::collections::BTreeMap;

#[derive(Insertable, Queryable)]
#[diesel(primary_key(run_id))]
#[table_name = "statistics_map"]
pub struct StatisticsMapInfo {
    #[diesel(deserialize_as = "i32")]
    pub statistics_map_id: Option<i32>,
    pub total_items: u64,
    pub item_struct: u64,
    pub item_function: u64,
    pub total_stmts: u64,
    pub stmt_local: u64,
    pub stmt_semi: u64,
    pub stmt_expr: u64,
    pub total_exprs: u64,
    pub expr_literal: u64,
    pub expr_binary: u64,
    pub expr_unary: u64,
    pub expr_cast: u64,
    pub expr_if: u64,
    pub expr_block: u64,
    pub expr_ident: u64,
    pub expr_assign: u64,
    pub expr_index: u64,
    pub expr_field: u64,
    pub expr_reference: u64,
    pub total_tys: u64,
    pub ty_unit: u64,
    pub ty_prim: u64,
    pub ty_tuple: u64,
    pub ty_array: u64,
    pub ty_struct: u64,
    pub ty_reference: u64,
    pub total_binary_ops: u64,
    pub binary_op_add: u64,
    pub binary_op_sub: u64,
    pub binary_op_mul: u64,
    pub binary_op_div: u64,
    pub binary_op_and: u64,
    pub binary_op_or: u64,
    pub binary_op_bit_xor: u64,
    pub binary_op_bit_and: u64,
    pub binary_op_bit_or: u64,
    pub binary_op_eq: u64,
    pub binary_op_lq: u64,
    pub binary_op_le: u64,
    pub binary_op_ne: u64,
    pub binary_op_ge: u64,
    pub binary_op_gt: u64,
    pub binary_op_wrapping_add: u64,
    pub binary_op_wrapping_sub: u64,
    pub binary_op_wrapping_mul: u64,
    pub binary_op_wrapping_div: u64,
    pub binary_op_wrapping_rem: u64,
    pub binary_op_wrapping_shl: u64,
    pub binary_op_wrapping_shr: u64,
    pub total_unary_ops: u64,
    pub unary_op_deref: u64,
    pub unary_op_not: u64,
    pub unary_op_neg: u64,
    #[diesel(deserialize_as = "NaiveDateTime")]
    pub created_at: Option<NaiveDateTime>,
    #[diesel(deserialize_as = "NaiveDateTime")]
    pub updated_at: Option<NaiveDateTime>,
}

fn get_counter_value<K: Ord>(counter: &BTreeMap<K, usize>, key: &K) -> u64 {
    *counter.get(key).unwrap_or(&0) as u64
}

impl From<FullStatisticsMap> for StatisticsMapInfo {
    fn from(full_stats: FullStatisticsMap) -> StatisticsMapInfo {
        let map = full_stats.statistics_mapping;
        StatisticsMapInfo {
            statistics_map_id: None,
            total_items: full_stats.total_items as u64,
            item_struct: get_counter_value(&map.item_counter, &ItemKind::Struct),
            item_function: get_counter_value(&map.item_counter, &ItemKind::Function),
            total_stmts: full_stats.total_stmts as u64,
            stmt_local: get_counter_value(&map.stmt_counter, &StmtKind::Local),
            stmt_semi: get_counter_value(&map.stmt_counter, &StmtKind::Semi),
            stmt_expr: get_counter_value(&map.stmt_counter, &StmtKind::Expr),
            total_exprs: full_stats.total_exprs as u64,
            expr_literal: get_counter_value(&map.expr_counter, &ExprKind::Literal),
            expr_binary: get_counter_value(&map.expr_counter, &ExprKind::Binary),
            expr_unary: get_counter_value(&map.expr_counter, &ExprKind::Unary),
            expr_cast: get_counter_value(&map.expr_counter, &ExprKind::Cast),
            expr_if: get_counter_value(&map.expr_counter, &ExprKind::If),
            expr_block: get_counter_value(&map.expr_counter, &ExprKind::Block),
            expr_ident: get_counter_value(&map.expr_counter, &ExprKind::Ident),
            expr_assign: get_counter_value(&map.expr_counter, &ExprKind::Assign),
            expr_index: get_counter_value(&map.expr_counter, &ExprKind::Index),
            expr_field: get_counter_value(&map.expr_counter, &ExprKind::Field),
            expr_reference: get_counter_value(&map.expr_counter, &ExprKind::Reference),
            total_tys: full_stats.total_tys as u64,
            ty_unit: get_counter_value(&map.ty_counter, &TyKind::Unit),
            ty_prim: get_counter_value(&map.ty_counter, &TyKind::Prim),
            ty_tuple: get_counter_value(&map.ty_counter, &TyKind::Tuple),
            ty_array: get_counter_value(&map.ty_counter, &TyKind::Array),
            ty_struct: get_counter_value(&map.ty_counter, &TyKind::Struct),
            ty_reference: get_counter_value(&map.ty_counter, &TyKind::Reference),
            total_binary_ops: full_stats.total_binary_ops as u64,
            binary_op_add: get_counter_value(&map.bin_op_counter, &BinaryOp::Add),
            binary_op_sub: get_counter_value(&map.bin_op_counter, &BinaryOp::Sub),
            binary_op_mul: get_counter_value(&map.bin_op_counter, &BinaryOp::Mul),
            binary_op_div: get_counter_value(&map.bin_op_counter, &BinaryOp::Div),
            binary_op_and: get_counter_value(&map.bin_op_counter, &BinaryOp::And),
            binary_op_or: get_counter_value(&map.bin_op_counter, &BinaryOp::Or),
            binary_op_bit_xor: 0,
            binary_op_bit_and: 0,
            binary_op_bit_or: 0,
            binary_op_eq: get_counter_value(&map.bin_op_counter, &BinaryOp::Eq),
            binary_op_lq: get_counter_value(&map.bin_op_counter, &BinaryOp::Le),
            binary_op_le: get_counter_value(&map.bin_op_counter, &BinaryOp::Le),
            binary_op_ne: get_counter_value(&map.bin_op_counter, &BinaryOp::Ne),
            binary_op_ge: get_counter_value(&map.bin_op_counter, &BinaryOp::Ge),
            binary_op_gt: get_counter_value(&map.bin_op_counter, &BinaryOp::Gt),
            binary_op_wrapping_add: get_counter_value(&map.bin_op_counter, &BinaryOp::WrappingAdd),
            binary_op_wrapping_sub: get_counter_value(&map.bin_op_counter, &BinaryOp::WrappingSub),
            binary_op_wrapping_mul: get_counter_value(&map.bin_op_counter, &BinaryOp::WrappingMul),
            binary_op_wrapping_div: get_counter_value(&map.bin_op_counter, &BinaryOp::WrappingDiv),
            binary_op_wrapping_rem: get_counter_value(&map.bin_op_counter, &BinaryOp::WrappingRem),
            binary_op_wrapping_shl: get_counter_value(&map.bin_op_counter, &BinaryOp::WrappingShl),
            binary_op_wrapping_shr: get_counter_value(&map.bin_op_counter, &BinaryOp::WrappingShr),
            total_unary_ops: full_stats.total_unary_ops as u64,
            unary_op_deref: get_counter_value(&map.un_op_counter, &UnaryOp::Deref),
            unary_op_not: get_counter_value(&map.un_op_counter, &UnaryOp::Not),
            unary_op_neg: get_counter_value(&map.un_op_counter, &UnaryOp::Neg),
            created_at: None,
            updated_at: None,
        }
    }
}

impl StatisticsMapInfo {
    pub fn insert_new(&self, connection: &MysqlConnection) -> i32 {
        use crate::schema::statistics_map::dsl::statistics_map;
        insert_into(statistics_map)
            .values(self)
            .execute(connection)
            .unwrap();
        select(last_insert_id).first(connection).unwrap()
    }
}
