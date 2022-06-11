use crate::ast::expr::Expr;
use crate::ast::item::Item;
use crate::ast::op::{BinaryOp, UnaryOp};
use crate::ast::stmt::Stmt;
use crate::statistics::program::ProgramStatistics;
use crate::visitor::base_visitor::Visitor;

#[derive(Default)]
pub struct StatisticsVisitor {
    statistics: ProgramStatistics,
}

impl Visitor for StatisticsVisitor {
    fn visit_item(&mut self, item: &mut Item) {
        *self
            .statistics
            .mapping
            .item_counter
            .entry(item.kind())
            .or_insert(0) += 1;
    }

    fn visit_stmt(&mut self, stmt: &mut Stmt) {
        *self
            .statistics
            .mapping
            .stmt_counter
            .entry(stmt.kind())
            .or_insert(0) += 1;
    }

    fn visit_expr(&mut self, expr: &mut Expr) {
        *self
            .statistics
            .mapping
            .expr_counter
            .entry(expr.kind())
            .or_insert(0) += 1;
    }

    fn visit_unary_op(&mut self, op: &mut UnaryOp) {
        *self
            .statistics
            .mapping
            .un_op_counter
            .entry(*op)
            .or_insert(0) += 1;
    }

    fn visit_binary_op(&mut self, op: &mut BinaryOp) {
        *self
            .statistics
            .mapping
            .bin_op_counter
            .entry(*op)
            .or_insert(0) += 1;
    }
}
