use crate::ast::expr::{
    ArrayExpr, AssignExpr, BinaryExpr, CastExpr, EvalExpr, Expr, IdentExpr, IfExpr, LitExpr,
    TupleExpr, UnaryExpr,
};
use crate::ast::stmt::{DeclLocalStmt, InitLocalStmt, SemiStmt};
use crate::ast::ty::Ty;
use crate::visitor::base_visitor;
use crate::visitor::base_visitor::Visitor;
use std::collections::hash_map::Iter;
use std::collections::HashMap;

#[derive(Clone)]
pub struct ExprVisitor {
    expr: Option<EvalExpr>,
    deadcode_mode: bool,
    full_symbol_table: ExprSymbolTable,
    pub local_symbol_table: ExprSymbolTable,
    prev_local_symbol_table: Vec<ExprSymbolTable>,
    max_attempt_fix: usize,
}

impl ExprVisitor {
    pub fn new() -> ExprVisitor {
        ExprVisitor {
            expr: None,
            deadcode_mode: false,
            full_symbol_table: ExprSymbolTable::default(),
            local_symbol_table: ExprSymbolTable::default(),
            prev_local_symbol_table: vec![],
            max_attempt_fix: 1,
        }
    }

    fn safe_expr_visit(&mut self, expr: &mut Expr) -> EvalExpr {
        self.expr = None;
        self.visit_expr(expr);
        self.expr.clone().unwrap()
    }

    fn add_expr(&mut self, key: &str, value: &EvalExpr, ty: &Ty) {
        if !self.deadcode_mode {
            self.full_symbol_table
                .add_expr(key, value.clone(), ty.clone());
        }
        self.local_symbol_table
            .add_expr(key, value.clone(), ty.clone());
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
        self.local_symbol_table = self.prev_local_symbol_table.pop().unwrap();
    }

    // TODO: Implement local decl stmt
    fn visit_local_decl_stmt(&mut self, _stmt: &mut DeclLocalStmt) {
        self.expr = Some(EvalExpr::unit_expr());
    }

    fn visit_local_init_stmt(&mut self, stmt: &mut InitLocalStmt) {
        let res_expr = self.safe_expr_visit(&mut stmt.rhs);
        self.add_expr(&stmt.name, &res_expr, &stmt.ty);
        self.expr = Some(EvalExpr::unit_expr());
    }

    fn visit_semi_stmt(&mut self, stmt: &mut SemiStmt) {
        self.visit_expr(&mut stmt.expr);
        self.expr = Some(EvalExpr::unit_expr());
    }

    fn visit_expr(&mut self, expr: &mut Expr) {
        if let Expr::Unary(_) = expr {
            // visit_unary_expr can modify the expr to some other expr variant
            self.visit_unary_expr(expr);
        } else {
            base_visitor::walk_expr(self, expr);
        }
    }

    fn visit_literal_expr(&mut self, expr: &mut LitExpr) {
        self.expr = Some(EvalExpr::Literal(expr.clone()));
    }

    fn visit_binary_expr(&mut self, expr: &mut BinaryExpr) {
        let mut lhs = self.safe_expr_visit(&mut expr.lhs);
        if expr.op.short_circuit_rhs(&lhs) {
            let prev_deadcode_mode = self.deadcode_mode;
            self.deadcode_mode = true;
            self.safe_expr_visit(&mut expr.rhs);
            self.expr = Some(lhs);
            self.deadcode_mode = prev_deadcode_mode;
            return;
        }
        let mut rhs = self.safe_expr_visit(&mut expr.rhs);
        let mut res = expr.op.apply(&lhs, &rhs);
        for _ in 0..=self.max_attempt_fix {
            if let Err(err) = &res {
                expr.fix(*err, &mut lhs, &mut rhs);
                res = expr.op.apply(&lhs, &rhs);
            } else {
                break;
            }
        }
        self.expr = Some(res.unwrap());
    }
    fn visit_unary_expr(&mut self, _expr: &mut UnaryExpr) {
        unreachable!()
    }

    fn visit_cast_expr(&mut self, expr: &mut CastExpr) {
        self.expr = Some(self.safe_expr_visit(&mut expr.expr).cast(&expr.ty));
    }

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
            assert!(self.deadcode_mode || !matches!(expr, EvalExpr::Unknown));
        } else {
            // assert!(self.full_symbol_table.get_expr_by_name(&expr.name).is_some());
            assert!(self.deadcode_mode);
            // Not in the local symbol table but in full symbol table
            self.expr = Some(EvalExpr::Unknown);
        }
    }

    fn visit_tuple_expr(&mut self, expr: &mut TupleExpr) {
        let mut res: Vec<EvalExpr> = vec![];
        let mut return_none = false;
        for inner_expr in &mut expr.tuple {
            let res_expr = self.safe_expr_visit(inner_expr);
            if let EvalExpr::Unknown = res_expr {
                return_none = true;
            } else {
                res.push(res_expr);
            }
        }
        let res_expr: EvalExpr = if return_none {
            EvalExpr::Unknown
        } else {
            EvalExpr::Tuple(res)
        };
        self.expr = Some(res_expr);
    }

    fn visit_array_expr(&mut self, expr: &mut ArrayExpr) {
        let mut res: Vec<EvalExpr> = vec![];
        let mut return_none = false;
        for inner_expr in &mut expr.array {
            let res_expr = self.safe_expr_visit(inner_expr);
            if let EvalExpr::Unknown = res_expr {
                return_none = true;
            } else {
                res.push(res_expr);
            }
        }
        let res_expr: EvalExpr = if return_none {
            EvalExpr::Unknown
        } else {
            EvalExpr::Array(res)
        };
        self.expr = Some(res_expr);
    }

    fn visit_assign_expr(&mut self, expr: &mut AssignExpr) {
        let res_expr = self.safe_expr_visit(&mut expr.rhs);
        let _sym_table = self.symbol_table();
        self.add_expr(
            &expr.name,
            &res_expr,
            &self.full_symbol_table.get_ty_by_name(&expr.name).unwrap(),
        );

        self.expr = Some(EvalExpr::unit_expr());
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
            self.expr = Some(res.unwrap());
        } else {
            panic!();
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct ExprSymbolTable {
    expr_mapping: HashMap<String, EvalExpr>,
    ty_mapping: HashMap<String, Ty>,
}

impl<'a> IntoIterator for &'a ExprSymbolTable {
    type Item = (&'a String, &'a Ty);
    type IntoIter = Iter<'a, String, Ty>;

    fn into_iter(self) -> Self::IntoIter {
        (&self.ty_mapping).iter()
    }
}

impl ExprSymbolTable {
    pub fn get_expr_by_name(&self, name: &str) -> Option<EvalExpr> {
        Some(self.expr_mapping.get(name)?.clone())
    }

    pub fn get_ty_by_name(&self, name: &str) -> Option<Ty> {
        Some(self.ty_mapping.get(name)?.clone())
    }

    pub fn add_expr(&mut self, key: &str, value: EvalExpr, ty: Ty) {
        self.expr_mapping.insert(key.to_owned(), value);
        self.ty_mapping.insert(key.to_owned(), ty);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::ast::expr::{BinaryOp, BlockExpr, LitExprTy, UnaryOp};
    use crate::ast::function::Function;
    use crate::ast::stmt::{LocalStmt, Stmt};
    use crate::ast::ty::IntTy;
    use crate::visitor::emit_visitor::EmitVisitor;

    #[test]
    fn unary_expr_ok_not() {
        for b in [true, false] {
            let mut expr = Expr::Unary(UnaryExpr {
                expr: Box::new(Expr::bool(b)),
                op: UnaryOp::Not,
            });
            let expected_expr = expr.clone();
            let mut visitor = ExprVisitor::new();
            visitor.visit_unary_expr(&mut expr);
            // Assert that the tree is the same
            assert_eq!(expr, expected_expr);
            let eval_expr = visitor.expr;
            let expected_eval_expr = Some(EvalExpr::bool(!b));
            // Assert that the evaluated value is correct
            assert_eq!(eval_expr, expected_eval_expr);
        }
    }

    #[test]
    fn unary_expr_ok_neg() {
        let mut expr = Expr::Unary(UnaryExpr {
            expr: Box::new(Expr::i8(32)),
            op: UnaryOp::Neg,
        });
        let expected_expr = expr.clone();
        let mut visitor = ExprVisitor::new();
        visitor.visit_unary_expr(&mut expr);
        assert_eq!(expr, expected_expr);
        let eval_expr = visitor.expr;
        let expected_eval_expr = Some(EvalExpr::i8(-32));
        assert_eq!(eval_expr, expected_eval_expr);
    }

    #[test]
    fn unary_expr_fix_overflow_neg() {
        let mut expr = Expr::Unary(UnaryExpr {
            expr: Box::new(Expr::i8(i8::MIN)),
            op: UnaryOp::Neg,
        });
        let expected_expr = Expr::i8(i8::MIN);
        let mut visitor = ExprVisitor::new();
        visitor.visit_unary_expr(&mut expr);
        assert_eq!(expr, expected_expr);
        let eval_expr = visitor.expr;
        let expected_eval_expr = Some(EvalExpr::i8(i8::MIN));
        assert_eq!(eval_expr, expected_eval_expr);
    }

    #[test]
    fn binary_expr_signed_fix() {
        for op in [
            BinaryOp::Add,
            BinaryOp::Sub,
            BinaryOp::Mul,
            BinaryOp::Div,
            BinaryOp::Rem,
        ] {
            for i in i8::MIN..=i8::MAX {
                for j in i8::MIN..=i8::MAX {
                    let mut expr = BinaryExpr {
                        lhs: Box::new(Expr::i8(i)),
                        rhs: Box::new(Expr::i8(j)),
                        op,
                    };
                    let mut visitor = ExprVisitor::new();
                    visitor.visit_binary_expr(&mut expr);
                }
            }
        }
    }

    #[test]
    fn binary_expr_unsigned_fix() {
        for op in [
            BinaryOp::Add,
            BinaryOp::Sub,
            BinaryOp::Mul,
            BinaryOp::Div,
            BinaryOp::Rem,
        ] {
            for i in u8::MIN..=u8::MAX {
                for j in u8::MIN..=u8::MAX {
                    let mut expr = BinaryExpr {
                        lhs: Box::new(Expr::u8(i)),
                        rhs: Box::new(Expr::u8(j)),
                        op,
                    };
                    let mut visitor = ExprVisitor::new();
                    visitor.visit_binary_expr(&mut expr);
                }
            }
        }
    }

    #[test]
    fn assign_scope_test() {
        let mut func = Function {
            name: "main".to_owned(),
            block: BlockExpr {
                stmts: vec![
                    Stmt::Local(LocalStmt::Init(InitLocalStmt {
                        name: "var_0".to_owned(),
                        ty: Ty::Int(IntTy::I8),
                        rhs: Expr::Literal(LitExpr::Int(127, LitExprTy::Signed(IntTy::I8))),
                        mutable: true,
                    })),
                    Stmt::Semi(SemiStmt {
                        expr: Expr::Block(BlockExpr {
                            stmts: vec![
                                Stmt::Semi(SemiStmt {
                                    expr: Expr::Assign(AssignExpr {
                                        name: "var_0".to_owned(),
                                        rhs: Box::new(Expr::Literal(LitExpr::Int(
                                            127,
                                            LitExprTy::Signed(IntTy::I8),
                                        ))),
                                    }),
                                })
                            ]
                        })
                    }),
                    Stmt::Semi(SemiStmt {
                        expr: Expr::Assign(AssignExpr {
                            name: "var_0".to_owned(),
                            rhs: Box::new(Expr::Binary(BinaryExpr {
                                lhs: Box::new(Expr::Ident(IdentExpr {
                                    name: "var_0".to_owned(),
                                    ty: Ty::Int(IntTy::I8),
                                })),
                                rhs: Box::new(Expr::Literal(LitExpr::Int(
                                    1,
                                    LitExprTy::Signed(IntTy::I8),
                                ))),
                                op: BinaryOp::Add,
                            })),
                        }),
                    }),
                ],
            },
        };
        let mut expr_visitor = ExprVisitor::new();
        expr_visitor.visit_function(&mut func);
        let mut emit_visitor = EmitVisitor::default();
        emit_visitor.visit_function(&mut func);
        println!("{}", emit_visitor.output())
    }
}
