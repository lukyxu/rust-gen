use crate::ast::expr::{
    BinaryExpr, BinaryOp, BlockExpr, CastExpr, Expr, IdentExpr, IfExpr, LitExpr, LitExprTy,
    UnaryExpr, UnaryOp,
};
use crate::ast::function::Function;
use crate::ast::stmt::{DeclLocalStmt, ExprStmt, InitLocalStmt, SemiStmt, Stmt};
use crate::ast::ty::{IntTy, Ty, UIntTy};
use crate::visitor::visitor::Visitor;
use either::Either;

pub struct EmitVisitor {
    output: String,
    curr_indent: usize,
    indentation: usize,
}

impl Default for EmitVisitor {
    fn default() -> Self {
        EmitVisitor {
            output: String::new(),
            curr_indent: 0,
            indentation: 4,
        }
    }
}

impl EmitVisitor {
    pub fn output(&self) -> String {
        if self.output.is_empty() {
            panic!("Run visit before accessing emit visitor output")
        }
        return self.output.clone();
    }
}

impl Visitor for EmitVisitor {
    fn visit_function(&mut self, function: &Function) {
        self.output.push_str(&format!("fn {}() ", function.name));
        self.visit_expr(&function.block)
    }
    fn visit_type(&mut self, ty: &Ty) {
        self.output.push_str(&ty.to_string())
    }

    fn visit_local_init_stmt(&mut self, stmt: &InitLocalStmt) {
        self.output.push_str(&format!(
            "{}let {}: ",
            " ".repeat(self.curr_indent),
            stmt.name
        ));
        self.visit_type(&stmt.ty);
        self.output.push_str(" = ");
        self.visit_expr(&stmt.rhs);
        self.output.push_str(";")
    }

    fn visit_semi_stmt(&mut self, stmt: &SemiStmt) {
        self.output.push_str(&" ".repeat(self.curr_indent));
        self.visit_expr(&stmt.expr);
        self.output.push_str(";");
    }

    fn visit_expr_stmt(&mut self, stmt: &ExprStmt) {
        self.output.push_str(&" ".repeat(self.curr_indent));
        self.visit_expr(&stmt.expr);
    }

    fn visit_literal_expr(&mut self, expr: &LitExpr) {
        let expr = match expr {
            LitExpr::Str(str) => str.to_string(),
            LitExpr::Byte(byte) => byte.to_string(),
            LitExpr::Char(char) => char.to_string(),
            // TODO: Tidy this logic up
            LitExpr::Int(u128, int_type) => {
                let to_emit: String = match int_type {
                    LitExprTy::Signed(t) => {
                        let int_str = match t {
                            IntTy::ISize => (*u128 as isize).to_string(),
                            IntTy::I8 => (*u128 as i8).to_string(),
                            IntTy::I16 => (*u128 as i16).to_string(),
                            IntTy::I32 => (*u128 as i32).to_string(),
                            IntTy::I64 => (*u128 as i64).to_string(),
                            IntTy::I128 => (*u128 as i128).to_string(),
                        };
                        format!("{}_{}", int_str, Ty::Int(*t).to_string()).to_string()
                    }
                    LitExprTy::Unsigned(t) => {
                        let uint_str = match t {
                            UIntTy::USize => (*u128 as usize).to_string(),
                            UIntTy::U8 => (*u128 as u8).to_string(),
                            UIntTy::U16 => (*u128 as u16).to_string(),
                            UIntTy::U32 => (*u128 as u32).to_string(),
                            UIntTy::U64 => (*u128 as u64).to_string(),
                            UIntTy::U128 => u128.to_string(),
                        };
                        format!("{}_{}", uint_str, Ty::UInt(*t).to_string()).to_string()
                    }
                    // Unsuffixed defaults to i32
                    LitExprTy::Unsuffixed => (*u128 as i32).to_string(),
                };
                to_emit
            }
            LitExpr::Float(f_str, float_type) => f_str.to_string(),
            LitExpr::Bool(bool) => bool.to_string(),
        };
        self.output.push_str(&expr)
    }

    fn visit_block_expr(&mut self, expr: &BlockExpr) {
        self.output.push_str("{\n");
        self.curr_indent += self.indentation;
        for stmt in &expr.stmts {
            // self.output
            //     .push_str(&format!("{}", " ".repeat(self.curr_indent)));
            self.visit_stmt(stmt);
            self.output.push_str("\n");
        }
        self.curr_indent -= self.indentation;
        self.output.push_str("}");
    }

    fn visit_if_expr(&mut self, expr: &IfExpr) {
        self.output.push_str("if ");
        self.visit_expr(&expr.condition);
        self.output.push_str(" {\n");
        self.curr_indent += self.indentation;
        self.visit_stmt(&expr.then);
        self.curr_indent -= self.indentation;
        self.output
            .push_str(&format!("\n{}}}", " ".repeat(self.curr_indent)));
        if let Some(otherwise) = &expr.otherwise {
            self.output.push_str(&format!(" else {{\n"));
            self.curr_indent += self.indentation;
            self.visit_stmt(otherwise);
            self.curr_indent -= self.indentation;
            self.output
                .push_str(&format!("\n{}}}", " ".repeat(self.curr_indent)));
        }
    }

    fn visit_binary_expr(&mut self, expr: &BinaryExpr) {
        self.output.push_str("(");
        self.visit_expr(&expr.lhs);
        self.output.push_str(" ");
        self.visit_binary_op(&expr.op);
        self.output.push_str(" ");
        self.visit_expr(&expr.rhs);
        self.output.push_str(")");
    }

    fn visit_binary_op(&mut self, op: &BinaryOp) {
        let op = match op {
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::And => "&&",
            BinaryOp::Or => "||",
        };
        self.output.push_str(&op)
    }

    fn visit_ident_expr(&mut self, expr: &IdentExpr) {
        self.output.push_str(&expr.name)
    }
}
