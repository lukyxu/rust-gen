use crate::ast::expr::LitExprTy::Unsigned;
use crate::ast::expr::{ArrayExpr, AssignExpr, BinaryExpr, BinaryOp, BlockExpr, CastExpr, Expr, IdentExpr, IfExpr, LitExpr, TupleExpr, UnaryExpr, UnaryOp};
use crate::ast::function::Function;
use crate::ast::stmt::{CustomStmt, DeclLocalStmt, ExprStmt, InitLocalStmt, LocalStmt, SemiStmt, Stmt};
use crate::ast::ty::{Ty, UIntTy};
use crate::visitor::base_visitor::Visitor;
use crate::visitor::expr_visitor::ExprVisitor;

pub struct ChecksumGenVisitor {
    expr_visitor: ExprVisitor,
    checksum_name: &'static str,
}

impl ChecksumGenVisitor {
    pub fn new() -> ChecksumGenVisitor {
        ChecksumGenVisitor {
            expr_visitor: ExprVisitor::new(),
            checksum_name: "checksum",
        }
    }
}

impl Visitor for ChecksumGenVisitor {
    fn visit_function(&mut self, function: &mut Function) {
        function.block.stmts.insert(
            0,
            Stmt::Local(LocalStmt::Init(InitLocalStmt {
                name: self.checksum_name.to_owned(),
                ty: Ty::UInt(UIntTy::U128),
                rhs: Expr::Literal(LitExpr::Int(0, Unsigned(UIntTy::U128))),
                mutable: true,
            })),
        );
        function.block.stmts.push(Stmt::Custom(CustomStmt {
            stmt: format!("println!(\"{{}}\", {})", self.checksum_name),
        }));
        self.visit_block_expr(&mut function.block);
    }

    fn visit_block_expr(&mut self, expr: &mut BlockExpr) {
        self.expr_visitor.enter_scope();
        for stmt in (&mut expr.stmts).split_last_mut().unwrap().1 {
            self.expr_visitor.visit_stmt(stmt);
            self.visit_stmt(stmt);
        }
        for (name, ty) in &self.expr_visitor.local_symbol_table {
            if name == self.checksum_name {
                continue;
            }
            let cast_expr = match ty {
                Ty::Int(_) | Ty::UInt(_) => Expr::Cast(CastExpr {
                    expr: {
                        Box::new(Expr::Ident(IdentExpr {
                            name: name.clone(),
                            ty: ty.clone(),
                        }))
                    },
                    ty: Ty::UInt(UIntTy::U128),
                }),
                // TODO: Hash other types too
                _ => continue,
            };
            let stmt = Stmt::Semi(SemiStmt {
                expr: Expr::Assign(AssignExpr {
                    name: self.checksum_name.to_owned(),
                    rhs: Box::new(Expr::Binary(BinaryExpr {
                        lhs: Box::new(Expr::Ident(IdentExpr {
                            name: self.checksum_name.to_owned(),
                            ty: Ty::UInt(UIntTy::U128),
                        })),
                        rhs: Box::new(cast_expr),
                        op: BinaryOp::Add,
                    })),
                }),
            });
            expr.stmts.insert(expr.stmts.len() - 1, stmt);
        }
        self.visit_stmt((&mut expr.stmts).last_mut().unwrap());
        self.expr_visitor.exit_scope();
    }

    // fn visit_if_expr(&mut self, expr: &mut IfExpr) {
    //     let true_expr_visitor = self.expr_visitor.clone();
    //     let false_expr_visitor = self.expr_visitor.clone();
    //
    // }
}
