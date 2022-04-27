use crate::ast::expr::{EvalExpr, LitExpr, LitExprTy};

use crate::ast::ty::UIntTy;
use crate::{ExprVisitor, Function, Visitor};

pub struct ChecksumEvalVisitor {
    expr_visitor: ExprVisitor,
    checksum_name: &'static str,
    pub res: Option<u128>,
}

impl ChecksumEvalVisitor {
    pub fn new() -> ChecksumEvalVisitor {
        ChecksumEvalVisitor {
            expr_visitor: ExprVisitor::new(),
            checksum_name: "checksum",
            res: None,
        }
    }
}

impl Visitor for ChecksumEvalVisitor {
    fn visit_function(&mut self, function: &mut Function) {
        for stmt in &mut function.block.stmts {
            self.expr_visitor.visit_stmt(stmt);
        }
        self.res = self
            .expr_visitor
            .local_symbol_table
            .get_expr_by_name(self.checksum_name)
            .map(|eval_expr| match eval_expr {
                EvalExpr::Literal(LitExpr::Int(u128, LitExprTy::Unsigned(UIntTy::U128))) => u128,
                _ => panic!(),
            });
    }
}
