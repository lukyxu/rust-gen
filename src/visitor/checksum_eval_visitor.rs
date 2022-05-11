use crate::ast::eval_expr::EvalExpr;
use crate::ast::expr::{LitExpr, LitExprTy};
use crate::ast::function::Function;

use crate::ast::ty::UIntTy;
use crate::visitor::base_visitor::Visitor;
use crate::visitor::expr_visitor::ExprVisitor;

pub struct ChecksumEvalVisitor {
    expr_visitor: ExprVisitor,
    checksum_name: &'static str,
    pub res: Option<u128>,
}

impl Default for ChecksumEvalVisitor {
    fn default() -> ChecksumEvalVisitor {
        ChecksumEvalVisitor {
            expr_visitor: ExprVisitor::default(),
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
            .symbol_table
            .get_expr_by_name(self.checksum_name)
            .and_then(|eval_expr| {
                if let EvalExpr::Literal(LitExpr::Int(u128, LitExprTy::Unsigned(UIntTy::U128))) =
                    eval_expr
                {
                    Some(u128)
                } else {
                    None
                }
            });
    }
}
