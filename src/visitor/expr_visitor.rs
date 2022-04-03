use crate::ast::expr::{
    BinaryExpr, BinaryOp, EvalExprError, Expr, IdentExpr, IfExpr, LitExpr,
};
use crate::ast::stmt::{InitLocalStmt};
use crate::Visitor;
use std::collections::HashMap;

#[derive(Clone, Default)]
pub struct ExprVisitor {
    expr: Option<LitExpr>,
    symbol_table: ExprSymbolTable,
    local_symbol_table: ExprSymbolTable,
    prev_local_symbol_table: Vec<ExprSymbolTable>,
}

impl ExprVisitor {
    fn safe_expr_visit(&mut self, expr: &mut Expr) -> LitExpr {
        self.expr = None;
        self.visit_expr(expr);
        return self.expr.clone().unwrap();
    }

    fn add_expr(&mut self, key: &String, value: &LitExpr) {
        self.symbol_table.add_expr(key.clone(), value.clone());
        self.local_symbol_table.add_expr(key.clone(), value.clone());
    }

    fn enter_scope(&mut self) {
        self.prev_local_symbol_table
            .push(self.local_symbol_table.clone());
        self.local_symbol_table = ExprSymbolTable::default();
    }
    fn exit_scope(&mut self) {
        self.local_symbol_table = self.prev_local_symbol_table.pop().unwrap()
    }
}

impl Visitor for ExprVisitor {
    fn visit_literal_expr(&mut self, expr: &mut LitExpr) {
        self.expr = Some((*expr).clone())
    }

    // TODO: I think I can do this with only the default visitor (No need for lhs/rhs visitor)
    // TODO: Can reduce the amount of duplication in the if let Some(lhs) = lhs_visitor.expr
    fn visit_binary_expr(&mut self, expr: &mut BinaryExpr) {
        let lhs = self.safe_expr_visit(&mut expr.lhs);
        let rhs = self.safe_expr_visit(&mut expr.rhs);
        let res = expr.op.apply(lhs, rhs);
        let error = match res {
            Ok(lit) => {
                self.expr = Some(lit);
                return;
            }
            Err(err) => err,
        };
        match expr.op {
            BinaryOp::Add => expr.op = BinaryOp::Sub,
            BinaryOp::Sub => expr.op = BinaryOp::Add,
            BinaryOp::Mul => {
                expr.op = if let EvalExprError::ZeroDiv = error {
                    BinaryOp::Sub
                } else {
                    BinaryOp::Div
                }
            }
            BinaryOp::Div => {
                expr.op = if let EvalExprError::MinMulOverflow = error {
                    BinaryOp::Mul
                } else {
                    BinaryOp::Sub
                }
            }
            _ => panic!(),
        }
        self.visit_binary_expr(expr)
    }
    // fn visit_unary_expr(&mut self, expr: &mut UnaryExpr) {
    //     walk_unary_expr(self, expr)
    // }
    // fn visit_cast_expr(&mut self, expr: &mut CastExpr) {
    //     walk_cast_expr(self, expr)
    // }
    // TODO: Have Two visitors, one for symbol table and one for testing expressions?
    fn visit_if_expr(&mut self, expr: &mut IfExpr) {
        let lit = self.safe_expr_visit(&mut expr.condition);
        if !matches!(lit, LitExpr::Bool(_)) {
            panic!()
        }
        self.visit_stmt(&mut expr.then);
        if let Some(otherwise) = &mut expr.otherwise {
            self.visit_stmt(otherwise)
        }
    }
    // fn visit_block_expr(&mut self, expr: &mut BlockExpr) {
    //     walk_block_expr(self, expr)
    // }
    fn visit_ident_expr(&mut self, expr: &mut IdentExpr) {
        if let Some(expr) = self.symbol_table.get_literal_expr_by_name(&expr.name) {
            self.expr = Some(expr.clone())
        } else {
            panic!("Expression not in symbol table")
        }
    }

    fn visit_local_init_stmt(&mut self, stmt: &mut InitLocalStmt) {
        let expr = self.safe_expr_visit(&mut stmt.rhs);
        self.add_expr(&stmt.name, &expr);
    }
}

#[derive(Debug, Default, Clone)]
pub struct ExprSymbolTable {
    expr_mapping: HashMap<String, LitExpr>,
}

impl ExprSymbolTable {
    pub fn get_literal_expr_by_name(&self, name: &String) -> Option<LitExpr> {
        if let Some(expr) = self.expr_mapping.get(name) {
            Some(expr.clone())
        } else {
            None
        }
    }

    pub fn add_expr(&mut self, key: String, value: LitExpr) {
        self.expr_mapping.insert(key, value);
    }

    pub fn merge_symbol_table(&self, other: &ExprSymbolTable) -> ExprSymbolTable {
        ExprSymbolTable {
            expr_mapping: self
                .expr_mapping
                .clone()
                .into_iter()
                .chain(other.clone().expr_mapping)
                .collect(),
        }
    }
}
