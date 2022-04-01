use std::collections::HashMap;
use crate::ast::expr::{BinaryExpr, BinaryOp, BlockExpr, CastExpr, EvalExprError, Expr, IdentExpr, IfExpr, LitExpr, LitExprTy, UnaryExpr, UnaryOp};
use crate::{Function, Visitor};
use crate::ast::stmt::{DeclLocalStmt, ExprStmt, InitLocalStmt, SemiStmt, Stmt};
use crate::ast::ty::Ty;

#[derive(Clone, Default)]
pub struct ExprVisitor {
    expr: Option<LitExpr>,
    error: Option<EvalExprError>,
    symbol_table: ExprSymbolTable
}

impl Visitor for ExprVisitor {
    fn visit_literal_expr(&mut self, expr: &mut LitExpr) {
        self.expr = Some((*expr).clone())
    }
    fn visit_binary_expr(&mut self, expr: &mut BinaryExpr) {
        let mut lhs_visitor = ExprVisitor::default();
        lhs_visitor.visit_expr(&mut expr.lhs);
        let lhs = if let Some(lhs) = lhs_visitor.expr {
            lhs
        } else {
            panic!()
        };
        let mut rhs_visitor = ExprVisitor::default();
        rhs_visitor.visit_expr(&mut expr.rhs);
        let rhs = if let Some(rhs) = rhs_visitor.expr {
            rhs
        } else {
            panic!()
        };
        let res = expr.op.apply(lhs, rhs);
        let error = match res {
            Ok(lit) => {
                self.expr = Some(lit);
                return
            }
            Err(err) => {
                err
            }
        };
        match expr.op {
            BinaryOp::Add => {expr.op = BinaryOp::Sub}
            BinaryOp::Sub => {expr.op = BinaryOp::Add}
            BinaryOp::Mul => {
                expr.op = if let EvalExprError::ZeroDiv = error {
                    BinaryOp::Mul
                } else {
                    BinaryOp::Sub
                }
            }
            BinaryOp::Div => {
                expr.op = if let EvalExprError::MinMulOverflow = error {
                    BinaryOp::Sub
                } else {
                    BinaryOp::Div
                }
            }
            _ => panic!()
        }
        self.visit_binary_expr(expr)
    }
    // fn visit_unary_expr(&mut self, expr: &mut UnaryExpr) {
    //     walk_unary_expr(self, expr)
    // }
    // fn visit_cast_expr(&mut self, expr: &mut CastExpr) {
    //     walk_cast_expr(self, expr)
    // }
    // fn visit_if_expr(&mut self, expr: &mut IfExpr) {
    //     walk_if_expr(self, expr)
    // }
    // fn visit_block_expr(&mut self, expr: &mut BlockExpr) {
    //     walk_block_expr(self, expr)
    // }
    // fn visit_ident_expr(&mut self, expr: &mut IdentExpr) {
    //     walk_ident_expr(self, expr)
    // }
}

#[derive(Debug, Default, Clone)]
pub struct ExprSymbolTable {
    expr_mapping: HashMap<String, LitExpr>,
}