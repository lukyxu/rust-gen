use crate::ast::expr::{
    BinaryExpr, BinaryOp, EvalExprError, Expr, IdentExpr, IfExpr, LitExpr,
};
use crate::ast::stmt::{InitLocalStmt};
use crate::Visitor;
use std::collections::HashMap;

type ResExpr = Option<LitExpr>;

#[derive(Clone, Default)]
pub struct ExprVisitor {
    expr: Option<ResExpr>,
    deadcode_check_mode: bool,
    full_symbol_table: ExprSymbolTable,
    local_symbol_table: ExprSymbolTable,
    prev_local_symbol_table: Vec<ExprSymbolTable>,
}

impl ExprVisitor {
    fn safe_expr_visit(&mut self, expr: &mut Expr) -> ResExpr {
        self.expr = None;
        self.visit_expr(expr);
        return self.expr.clone().unwrap();
    }

    fn add_expr(&mut self, key: &String, value: &ResExpr) {
            if !self.deadcode_check_mode {
                self.full_symbol_table.add_expr(key.clone(), value.clone());
            }
            self.local_symbol_table.add_expr(key.clone(), value.clone());
    }

    fn symbol_table(&self) -> &ExprSymbolTable {
        if self.deadcode_check_mode {
            &self.local_symbol_table
        } else {
            &self.full_symbol_table
        }
    }
}

impl Visitor for ExprVisitor {
    fn enter_scope(&mut self) {
        self.prev_local_symbol_table
            .push(self.local_symbol_table.clone());
        self.local_symbol_table = ExprSymbolTable::default();
    }

    fn exit_scope(&mut self) {
        self.local_symbol_table = self.prev_local_symbol_table.pop().unwrap()
    }
    fn visit_local_init_stmt(&mut self, stmt: &mut InitLocalStmt) {
        let res_expr = self.safe_expr_visit(&mut stmt.rhs);
        self.add_expr(&stmt.name, &res_expr);
    }
    fn visit_literal_expr(&mut self, expr: &mut LitExpr) {
        let res_expr = Some((*expr).clone());
        self.expr = Some(res_expr)
    }

    // TODO: I think I can do this with only the default visitor (No need for lhs/rhs visitor)
    // TODO: Can reduce the amount of duplication in the if let Some(lhs) = lhs_visitor.expr
    fn visit_binary_expr(&mut self, expr: &mut BinaryExpr) {
        let lhs = self.safe_expr_visit(&mut expr.lhs);
        let rhs = self.safe_expr_visit(&mut expr.rhs);
        if let (Some(lhs), Some(rhs)) = (lhs, rhs) {
            let res = expr.op.apply(lhs, rhs);
            let error = match res {
                Ok(lit) => {
                    let res_expr = Some(lit);
                    self.expr = Some(res_expr);
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
        if !matches!(lit, Some(LitExpr::Bool(_))) && !matches!(lit, None) {
            panic!()
        }
        let prev_deadcode_check_move = self.deadcode_check_mode;
        if let Some(LitExpr::Bool(true)) = lit {
            self.deadcode_check_mode = prev_deadcode_check_move;
        } else {
            self.deadcode_check_mode = true;
        }
        self.enter_scope();
        self.visit_stmt(&mut expr.then);
        self.exit_scope();
        if let Some(otherwise) = &mut expr.otherwise {
            if let Some(LitExpr::Bool(false)) = lit {
                self.deadcode_check_mode = prev_deadcode_check_move;
            } else {
                self.deadcode_check_mode = true;
            }
            self.visit_stmt(otherwise)
        }
        self.deadcode_check_mode = prev_deadcode_check_move
    }
    // fn visit_block_expr(&mut self, expr: &mut BlockExpr) {
    //     walk_block_expr(self, expr)
    // }
    fn visit_ident_expr(&mut self, expr: &mut IdentExpr) {
        if let Some(expr) = self.symbol_table().get_expr_by_name(&expr.name) {
            self.expr = Some(expr.clone());
            // When we are not in deadcode check mode then the result expression
            // should always evaluate to a literal expression
            assert!(self.deadcode_check_mode || !matches!(expr, None))
        } else {
            assert!(self.deadcode_check_mode);
            // Not in the local symbol table
            let res_expr = None;
            self.expr = Some(res_expr)
            // panic!("Expression not in symbol table")
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct ExprSymbolTable {
    expr_mapping: HashMap<String, ResExpr>,
}

impl ExprSymbolTable {
    pub fn get_expr_by_name(&self, name: &String) -> Option<ResExpr> {
        if let Some(expr) = self.expr_mapping.get(name) {
            Some(expr.clone())
        } else {
            None
        }
    }

    pub fn add_expr(&mut self, key: String, value: ResExpr) {
        self.expr_mapping.insert(key, value);
    }
}
