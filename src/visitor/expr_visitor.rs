use crate::ast::expr::{
    AssignExpr, BinaryExpr, CastExpr, EvalExpr, Expr, IdentExpr, IfExpr, LitExpr, TupleExpr,
    UnaryExpr, UnaryOp,
};
use crate::ast::stmt::{DeclLocalStmt, InitLocalStmt, SemiStmt};
use crate::visitor::base_visitor;
use crate::Visitor;
use std::collections::HashMap;

#[derive(Clone, Default)]
pub struct ExprVisitor {
    expr: Option<EvalExpr>,
    deadcode_mode: bool,
    full_symbol_table: ExprSymbolTable,
    local_symbol_table: ExprSymbolTable,
    prev_local_symbol_table: Vec<ExprSymbolTable>,
}

impl ExprVisitor {
    fn safe_expr_visit(&mut self, expr: &mut Expr) -> EvalExpr {
        self.expr = None;
        self.visit_expr(expr);
        self.expr.clone().unwrap()
    }

    fn add_expr(&mut self, key: &str, value: &EvalExpr) {
        if !self.deadcode_mode {
            self.full_symbol_table.add_expr(key, value.clone());
        }
        self.local_symbol_table.add_expr(key, value.clone());
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

    // TODO: Implement local decl stmt
    fn visit_local_decl_stmt(&mut self, _stmt: &mut DeclLocalStmt) {
        self.expr = Some(EvalExpr::unit_expr())
    }

    fn visit_local_init_stmt(&mut self, stmt: &mut InitLocalStmt) {
        let res_expr = self.safe_expr_visit(&mut stmt.rhs);
        self.add_expr(&stmt.name, &res_expr);
        self.expr = Some(EvalExpr::unit_expr())
    }

    fn visit_semi_stmt(&mut self, stmt: &mut SemiStmt) {
        self.visit_expr(&mut stmt.expr);
        self.expr = Some(EvalExpr::unit_expr())
    }

    fn visit_expr(&mut self, expr: &mut Expr) {
        if let Expr::Unary(_) = expr {
            // visit_unary_expr can modify the expr to some other expr variant
            self.visit_unary_expr(expr)
        } else {
            base_visitor::walk_expr(self, expr)
        }
    }

    fn visit_literal_expr(&mut self, expr: &mut LitExpr) {
        self.expr = Some(EvalExpr::Literal(expr.clone()))
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
        let mut res = expr.op.apply(&lhs, &rhs);
        if let Err(err) = &res {
            expr.op = expr.replacement_op(err);
            res = expr.op.apply(&lhs, &rhs);
        };
        if let Err(_e) = res {
            println!(":/")
        };
        self.expr = Some(res.unwrap())
    }
    fn visit_unary_expr(&mut self, _expr: &mut UnaryExpr) {
        unreachable!()
    }
    // fn visit_cast_expr(&mut self, expr: &mut CastExpr) {
    //     walk_cast_expr(self, expr)
    // }
    fn visit_if_expr(&mut self, expr: &mut IfExpr) {
        let cond_expr = self.safe_expr_visit(&mut expr.condition);

        let prev_deadcode_check_move = self.deadcode_mode;
        self.deadcode_mode = match &cond_expr {
            EvalExpr::Literal(LitExpr::Bool(true)) => prev_deadcode_check_move,
            EvalExpr::Literal(LitExpr::Bool(false)) | EvalExpr::Unknown => true,
            _ => panic!(),
        };
        // TODO: convert this to safe_visit_expr
        self.visit_block_expr(&mut expr.then);
        // true_expr and false_expr can be none
        let true_expr: EvalExpr = self.expr.clone().unwrap();
        let false_expr: EvalExpr = if let Some(otherwise) = &mut expr.otherwise {
            self.deadcode_mode = match &cond_expr {
                EvalExpr::Literal(LitExpr::Bool(false)) => prev_deadcode_check_move,
                EvalExpr::Literal(LitExpr::Bool(true)) | EvalExpr::Unknown => true,
                _ => panic!(),
            };
            self.visit_block_expr(otherwise);
            self.expr.clone().unwrap()
        } else {
            EvalExpr::unit_expr()
        };

        self.deadcode_mode = prev_deadcode_check_move;

        self.expr = Some(match &cond_expr {
            EvalExpr::Literal(LitExpr::Bool(true)) => true_expr,
            EvalExpr::Literal(LitExpr::Bool(false)) => false_expr,
            EvalExpr::Unknown => EvalExpr::Unknown,
            _ => panic!(),
        });
    }

    // fn visit_block_expr(&mut self, expr: &mut BlockExpr) {
    //     walk_block_expr(self, expr)
    // }
    fn visit_ident_expr(&mut self, expr: &mut IdentExpr) {
        if let Some(expr) = self.symbol_table().get_expr_by_name(&expr.name) {
            self.expr = Some(expr.clone());
            // When we are not in deadcode check mode then the result expression
            // should never evaluated to unknown value
            assert!(self.deadcode_mode || !matches!(expr, EvalExpr::Unknown))
        } else {
            assert!(self.deadcode_mode);
            // Not in the local symbol table
            self.expr = Some(EvalExpr::Unknown)
        }
    }

    fn visit_tuple_expr(&mut self, expr: &mut TupleExpr) {
        let mut res: Vec<EvalExpr> = vec![];
        let mut return_none = false;
        for inner_expr in &mut expr.tuple {
            let res_expr = self.safe_expr_visit(inner_expr);
            if let EvalExpr::Unknown = res_expr {
                return_none = true;
                break;
            } else {
                res.push(res_expr)
            }
        }
        let res_expr: EvalExpr = if return_none {
            EvalExpr::Unknown
        } else {
            EvalExpr::Tuple(res)
        };
        self.expr = Some(res_expr)
    }

    fn visit_assign_expr(&mut self, expr: &mut AssignExpr) {
        let res_expr = self.safe_expr_visit(&mut expr.rhs);
        self.add_expr(&expr.name, &res_expr);

        self.expr = Some(EvalExpr::unit_expr())
    }
}

impl ExprVisitor {
    fn visit_unary_expr(&mut self, expr: &mut Expr) {
        if let Expr::Unary(unary_expr) = expr {
            let eval_expr = self.safe_expr_visit(&mut unary_expr.expr);
            let mut res = unary_expr.op.apply(&eval_expr);
            if res.is_err() {
                res = Ok(eval_expr);
                // TODO: See if you can improve this
                *expr = *unary_expr.clone().expr;
            }
            self.expr = Some(res.unwrap())
        } else {
            panic!()
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct ExprSymbolTable {
    expr_mapping: HashMap<String, EvalExpr>,
}

impl ExprSymbolTable {
    pub fn get_expr_by_name(&self, name: &String) -> Option<EvalExpr> {
        Some(self.expr_mapping.get(name)?.clone())
    }

    pub fn add_expr(&mut self, key: &str, value: EvalExpr) {
        self.expr_mapping.insert(key.to_owned(), value);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::expr::EvalExprError::{MinMulOverflow, Overflow, ZeroDiv};
    use crate::ast::expr::{BinaryOp, EvalExprError};

    #[test]
    fn unary_expr_ok_not() {
        for b in [true, false] {
            let mut expr = Expr::Unary(UnaryExpr {
                expr: Box::new(Expr::bool(b)),
                op: UnaryOp::Not,
            });
            let mut visitor = ExprVisitor::default();
            visitor.visit_unary_expr(&mut expr);
            let expr = visitor.expr;
            let expected_expr = Some(EvalExpr::bool(!b));
            assert_eq!(expr, expected_expr)
        }
    }
    #[test]
    fn unary_expr_ok_neg() {
        // i = -127..=127
        for i in i8::MIN + 1..=i8::MAX {
            assert_eq!(UnaryOp::Neg.apply(&EvalExpr::i8(i)), Ok(EvalExpr::i8(-i)))
        }
    }
    #[test]
    fn unary_expr_fail_neg_signed_min_val() {
        // i = -128
        let i = i8::MIN;
        assert_eq!(
            UnaryOp::Neg.apply(&EvalExpr::i8(i)),
            Err(EvalExprError::Overflow)
        )
    }

    #[test]
    fn binary_expr_ok_signed_add() {
        assert_eq!(
            BinaryOp::Add.apply(&EvalExpr::i8(5), &EvalExpr::i8(12)),
            Ok(EvalExpr::i8(17))
        )
    }

    #[test]
    fn binary_expr_fail_overflow_signed_add() {
        assert_eq!(
            BinaryOp::Add.apply(&EvalExpr::i8(127), &EvalExpr::i8(127)),
            Err(EvalExprError::Overflow)
        )
    }

    #[test]
    fn binary_expr_ok_signed_sub() {
        assert_eq!(
            BinaryOp::Sub.apply(&EvalExpr::i8(-12), &EvalExpr::i8(16)),
            Ok(EvalExpr::i8(-28))
        )
    }

    #[test]
    fn binary_expr_fail_overflow_signed_sub() {
        assert_eq!(
            BinaryOp::Sub.apply(&EvalExpr::i8(-128), &EvalExpr::i8(127)),
            Err(EvalExprError::Overflow)
        )
    }

    #[test]
    fn binary_expr_ok_signed_mul() {
        assert_eq!(
            BinaryOp::Mul.apply(&EvalExpr::i8(4), &EvalExpr::i8(5)),
            Ok(EvalExpr::i8(20))
        )
    }

    #[test]
    fn binary_expr_fail_signed_min_mul_overflow() {
        assert_eq!(
            BinaryOp::Mul.apply(&EvalExpr::i8(i8::MIN), &EvalExpr::i8(-1)),
            Err(MinMulOverflow)
        )
    }

    #[test]
    fn binary_expr_fail_signed_min_mul_overflow_1() {
        assert_eq!(
            BinaryOp::Mul.apply(&EvalExpr::i8(-1), &EvalExpr::i8(i8::MIN)),
            Err(MinMulOverflow)
        )
    }

    #[test]
    fn binary_expr_fail_signed_mul_overflow() {
        assert_eq!(
            BinaryOp::Mul.apply(&EvalExpr::i8(12), &EvalExpr::i8(-15)),
            Err(Overflow)
        )
    }

    #[test]
    fn binary_expr_ok_signed_div() {
        assert_eq!(
            BinaryOp::Div.apply(&EvalExpr::i8(120), &EvalExpr::i8(4)),
            Ok(EvalExpr::i8(30))
        )
    }

    #[test]
    fn binary_expr_fail_signed_div_overflow() {
        assert_eq!(
            BinaryOp::Div.apply(&EvalExpr::i8(i8::MIN), &EvalExpr::i8(-1)),
            Err(Overflow)
        )
    }

    #[test]
    fn binary_expr_fail_signed_div_zero() {
        assert_eq!(
            BinaryOp::Div.apply(&EvalExpr::i8(12), &EvalExpr::i8(0)),
            Err(ZeroDiv)
        )
    }
}
