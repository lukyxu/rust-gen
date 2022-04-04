use crate::ast::expr::{
    BinaryExpr, BinaryOp, BlockExpr, CastExpr, Expr, IdentExpr, IfExpr, LitExpr, UnaryExpr, UnaryOp,
};
use crate::ast::function::Function;
use crate::ast::stmt::{DeclLocalStmt, ExprStmt, InitLocalStmt, LocalStmt, SemiStmt, Stmt};
use crate::ast::ty::Ty;

pub trait Visitor: Sized {
    fn enter_scope(&mut self) {}
    fn exit_scope(&mut self) {}

    fn visit_function(&mut self, function: &mut Function) {
        walk_function(self, function);
    }
    fn visit_name(&mut self, _name: &String) {}
    fn visit_type(&mut self, _ty: &Ty) {}

    // Statements
    fn visit_stmt(&mut self, stmt: &mut Stmt) {
        walk_stmt(self, stmt);
    }
    fn visit_local_decl_stmt(&mut self, stmt: &mut DeclLocalStmt) {
        walk_decl_local_stmt(self, stmt)
    }
    fn visit_local_init_stmt(&mut self, stmt: &mut InitLocalStmt) {
        walk_init_local_stmt(self, stmt)
    }
    fn visit_expr_stmt(&mut self, stmt: &mut ExprStmt) {
        walk_expr_stmt(self, stmt)
    }
    fn visit_semi_stmt(&mut self, stmt: &mut SemiStmt) {
        walk_semi_stmt(self, stmt)
    }

    // Expressions
    fn visit_expr(&mut self, expr: &mut Expr) {
        walk_expr(self, expr)
    }
    fn visit_literal_expr(&mut self, _expr: &mut LitExpr) {}
    fn visit_binary_expr(&mut self, expr: &mut BinaryExpr) {
        walk_binary_expr(self, expr)
    }
    fn visit_unary_expr(&mut self, expr: &mut UnaryExpr) {
        walk_unary_expr(self, expr)
    }
    fn visit_cast_expr(&mut self, expr: &mut CastExpr) {
        walk_cast_expr(self, expr)
    }
    fn visit_if_expr(&mut self, expr: &mut IfExpr) {
        walk_if_expr(self, expr)
    }
    fn visit_block_expr(&mut self, expr: &mut BlockExpr) {
        walk_block_expr(self, expr)
    }
    fn visit_ident_expr(&mut self, expr: &mut IdentExpr) {
        walk_ident_expr(self, expr)
    }

    // Operations
    fn visit_unary_op(&mut self, _op: &mut UnaryOp) {}
    fn visit_binary_op(&mut self, _op: &mut BinaryOp) {}
}

fn walk_function<V: Visitor>(visitor: &mut V, function: &mut Function) {
    visitor.visit_block_expr(&mut function.block)
}

fn walk_stmt<V: Visitor>(visitor: &mut V, stmt: &mut Stmt) {
    match stmt {
        Stmt::Local(LocalStmt::Decl(local_decl_stmt)) => {
            visitor.visit_local_decl_stmt(local_decl_stmt)
        }
        Stmt::Local(LocalStmt::Init(local_init_stmt)) => {
            visitor.visit_local_init_stmt(local_init_stmt)
        }
        Stmt::Expr(expr_stmt) => visitor.visit_expr_stmt(expr_stmt),
        Stmt::Semi(semi_stmt) => visitor.visit_semi_stmt(semi_stmt),
    }
}

fn walk_decl_local_stmt<V: Visitor>(visitor: &mut V, DeclLocalStmt { name, ty }: &DeclLocalStmt) {
    visitor.visit_name(name);
    visitor.visit_type(ty)
}

fn walk_init_local_stmt<V: Visitor>(
    visitor: &mut V,
    InitLocalStmt { name, ty, rhs }: &mut InitLocalStmt,
) {
    visitor.visit_name(name);
    visitor.visit_type(ty);
    visitor.visit_expr(rhs)
}

fn walk_expr_stmt<V: Visitor>(visitor: &mut V, ExprStmt { expr }: &mut ExprStmt) {
    visitor.visit_expr(expr)
}

fn walk_semi_stmt<V: Visitor>(visitor: &mut V, SemiStmt { expr }: &mut SemiStmt) {
    visitor.visit_expr(expr)
}

fn walk_expr<V: Visitor>(visitor: &mut V, expr: &mut Expr) {
    match expr {
        Expr::Literal(literal_expr) => visitor.visit_literal_expr(literal_expr),
        Expr::Binary(binary_expr) => visitor.visit_binary_expr(binary_expr),
        Expr::Unary(unary_expr) => visitor.visit_unary_expr(unary_expr),
        Expr::Cast(cast_expr) => visitor.visit_cast_expr(cast_expr),
        Expr::If(if_expr) => visitor.visit_if_expr(if_expr),
        Expr::Block(block_expr) => visitor.visit_block_expr(block_expr),
        Expr::Ident(ident_expr) => visitor.visit_ident_expr(ident_expr),
    }
}

fn walk_binary_expr<V: Visitor>(visitor: &mut V, BinaryExpr { lhs, rhs, op }: &mut BinaryExpr) {
    visitor.visit_expr(lhs);
    visitor.visit_expr(rhs);
    visitor.visit_binary_op(op)
}

fn walk_unary_expr<V: Visitor>(visitor: &mut V, UnaryExpr { expr, op }: &mut UnaryExpr) {
    visitor.visit_expr(expr);
    visitor.visit_unary_op(op)
}

fn walk_cast_expr<V: Visitor>(visitor: &mut V, CastExpr { expr, ty }: &mut CastExpr) {
    visitor.visit_expr(expr);
    visitor.visit_type(ty)
}

fn walk_if_expr<V: Visitor>(
    visitor: &mut V,
    IfExpr {
        condition,
        then,
        otherwise,
    }: &mut IfExpr,
) {
    visitor.visit_expr(condition);
    visitor.enter_scope();
    visitor.visit_stmt(then);
    visitor.exit_scope();
    if let Some(x) = otherwise {
        visitor.enter_scope();
        visitor.visit_stmt(x);
        visitor.exit_scope();
    }
}

fn walk_block_expr<V: Visitor>(visitor: &mut V, BlockExpr { stmts }: &mut BlockExpr) {
    visitor.enter_scope();
    for stmt in stmts {
        visitor.visit_stmt(stmt)
    }
    visitor.exit_scope();
}

fn walk_ident_expr<V: Visitor>(visitor: &mut V, IdentExpr { name, ty }: &IdentExpr) {
    visitor.visit_type(ty);
    visitor.visit_name(name)
}
