use crate::ast::expr::{Expr};
use crate::ast::item::Item;
use crate::ast::op::{BinaryOp, UnaryOp};
use crate::ast::stmt::Stmt;
use crate::ast::ty::TyKind;
use crate::statistics::program::ProgramStatistics;
use crate::visitor::base_visitor::{Visitor, walk_expr, walk_item, walk_stmt};

#[derive(Default)]
pub struct StatisticsVisitor {
    pub statistics: ProgramStatistics,
}

impl Visitor for StatisticsVisitor {
    fn visit_item(&mut self, item: &mut Item) {
        *self
            .statistics
            .mapping
            .item_counter
            .entry(item.kind())
            .or_insert(0) += 1;
        walk_item(self, item);
    }

    fn visit_stmt(&mut self, stmt: &mut Stmt) {
        *self
            .statistics
            .mapping
            .stmt_counter
            .entry(stmt.kind())
            .or_insert(0) += 1;
        walk_stmt(self, stmt);
    }

    fn visit_expr(&mut self, expr: &mut Expr) {
        *self
            .statistics
            .mapping
            .expr_counter
            .entry(expr.kind())
            .or_insert(0) += 1;
        let ty_kind = match expr {
            Expr::Literal(_) => Some(TyKind::Prim),
            Expr::Tuple(tuple) => {
                Some(if tuple.tuple.is_empty() { TyKind::Unit } else { TyKind::Tuple })
            }
            Expr::Array(_) => Some(TyKind::Array),
            Expr::Struct(_) => Some(TyKind::Struct),
            Expr::Reference(_) => Some(TyKind::Reference),
            _ => None
        };
        if let Some(ty_kind) = ty_kind {
            *self
                .statistics
                .mapping
                .ty_counter
                .entry(ty_kind)
                .or_insert(0) += 1;
        }
        walk_expr(self, expr);
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
