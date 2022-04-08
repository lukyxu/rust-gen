use crate::ast::expr::{AssignExpr, BinaryExpr, BinaryOp, BlockExpr, CastExpr, EvalExprError, Expr, IdentExpr, IfExpr, LitExpr, ResExpr, TupleExpr, UnaryExpr, UnaryOp};
use crate::ast::stmt::{DeclLocalStmt, ExprStmt, InitLocalStmt, SemiStmt, Stmt};
use crate::{Function, Visitor};
use std::collections::HashMap;
use crate::ast::ty::Ty;

#[derive(Clone, Default)]
pub struct ExprVisitor {
    expr: Option<ResExpr>,
    deadcode_mode: bool,
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
        if !self.deadcode_mode {
            self.full_symbol_table.add_expr(key.clone(), value.clone());
        }
        self.local_symbol_table.add_expr(key.clone(), value.clone());
    }

    fn symbol_table(&self) -> &ExprSymbolTable {
        if self.deadcode_mode {
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
    // TODO: visit_local_init_stmt & visit_semi_stmt
    fn visit_local_init_stmt(&mut self, stmt: &mut InitLocalStmt) {
        let res_expr = self.safe_expr_visit(&mut stmt.rhs);
        self.add_expr(&stmt.name, &res_expr);
    }

    fn visit_literal_expr(&mut self, expr: &mut LitExpr) {
        let res_expr = Some((*expr).clone());
        self.expr = Some(res_expr)
    }

    fn visit_binary_expr(&mut self, expr: &mut BinaryExpr) {
        let lhs = self.safe_expr_visit(&mut expr.lhs);
        if expr.op.short_circuit_rhs(&lhs) {
            let prev_deadcode_mode = self.deadcode_mode;
            self.deadcode_mode = true;
            self.safe_expr_visit(&mut expr.rhs);
            self.expr = Some(lhs);
            self.deadcode_mode = prev_deadcode_mode;
            return;
        }
        let rhs = self.safe_expr_visit(&mut expr.rhs);
        let mut res = expr.op.apply_res_expr(&lhs, &rhs);
        if let Err(err) = &res {
            expr.op = expr.replacement_op(err);
            res = expr.op.apply_res_expr(&lhs, &rhs);
        };
        self.expr = Some(res.unwrap())
    }

    // fn visit_unary_expr(&mut self, expr: &mut UnaryExpr) {
    //     walk_unary_expr(self, expr)
    // }
    // fn visit_cast_expr(&mut self, expr: &mut CastExpr) {
    //     walk_cast_expr(self, expr)
    // }
    fn visit_if_expr(&mut self, expr: &mut IfExpr) {
        let lit = self.safe_expr_visit(&mut expr.condition);
        if !matches!(lit, Some(LitExpr::Bool(_))) && !matches!(lit, None) {
            panic!()
        }
        let prev_deadcode_check_move = self.deadcode_mode;
        if let Some(LitExpr::Bool(true)) = lit {
            self.deadcode_mode = prev_deadcode_check_move;
        } else {
            self.deadcode_mode = true;
        }
        self.visit_block_expr(&mut expr.then);
        // true_expr and false_expr can be none
        let true_expr: ResExpr = self.expr.clone().unwrap();
        let false_expr: Option<ResExpr> = if let Some(otherwise) = &mut expr.otherwise {
            if let Some(LitExpr::Bool(false)) = lit {
                self.deadcode_mode = prev_deadcode_check_move;
            } else {
                self.deadcode_mode = true;
            }
            self.visit_block_expr(otherwise);
            Some(self.expr.clone().unwrap())
        } else {
            let res_expr = Some(LitExpr::Tuple(vec![]));
            Some(res_expr)
        };

        self.deadcode_mode = prev_deadcode_check_move;

        if let Some(LitExpr::Bool(false)) = lit {
            self.expr = false_expr
        } else {
            self.expr = Some(true_expr)
        }
    }
    // fn visit_block_expr(&mut self, expr: &mut BlockExpr) {
    //     walk_block_expr(self, expr)
    // }
    fn visit_ident_expr(&mut self, expr: &mut IdentExpr) {
        if let Some(expr) = self.symbol_table().get_expr_by_name(&expr.name) {
            self.expr = Some(expr.clone());
            // When we are not in deadcode check mode then the result expression
            // should always evaluate to a literal expression
            assert!(self.deadcode_mode || !matches!(expr, None))
        } else {
            assert!(self.deadcode_mode);
            // Not in the local symbol table
            let res_expr = None;
            self.expr = Some(res_expr)
            // panic!("Expression not in symbol table")
        }
    }

    fn visit_tuple_expr(&mut self, expr: &mut TupleExpr) {
        let mut res: Vec<LitExpr> = vec![];
        let mut return_none = false;
        for inner_expr in &mut expr.tuple {
            let res_expr = self.safe_expr_visit(inner_expr);
            if let Some(lit_expr) = res_expr {
                res.push(lit_expr)
            } else {
                return_none = true
            }
        }
        let res_expr: ResExpr = if return_none {
            None
        } else {
            Some(LitExpr::Tuple(res))
        };
        self.expr = Some(res_expr)
    }

    // TODO: Implement local decl stmt
    fn visit_local_decl_stmt(&mut self, _stmt: &mut DeclLocalStmt) {
        let res_expr = Some(LitExpr::Tuple(vec![]));
        self.expr = Some(res_expr)
    }

    fn visit_assign_expr(&mut self, expr: &mut AssignExpr) {
        let res_expr = self.safe_expr_visit(&mut expr.rhs);
        self.add_expr(&expr.name, &res_expr);

        let res_expr = Some(LitExpr::Tuple(vec![]));
        self.expr = Some(res_expr)
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
