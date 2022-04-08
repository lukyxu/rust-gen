use crate::ast::expr::{
    AssignExpr, BinaryExpr, BinaryOp, BlockExpr, IdentExpr, IfExpr, LitExpr, LitExprTy, TupleExpr,
};
use crate::ast::function::Function;
use crate::ast::stmt::{ExprStmt, InitLocalStmt, SemiStmt};
use crate::ast::ty::{IntTy, Ty, UIntTy};
use crate::visitor::visitor::Visitor;

pub struct EmitVisitor {
    output: String,
    curr_indent: usize,
    indentation: usize,
    expression_mode: bool,
}

impl Default for EmitVisitor {
    fn default() -> Self {
        EmitVisitor {
            output: String::new(),
            curr_indent: 0,
            indentation: 4,
            expression_mode: false,
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
    fn enter_scope(&mut self) {
        self.curr_indent += self.indentation;
    }
    fn exit_scope(&mut self) {
        self.curr_indent -= self.indentation;
    }

    fn visit_function(&mut self, function: &mut Function) {
        self.output.push_str(&format!("fn {}() ", function.name));
        self.visit_block_expr(&mut function.block)
    }

    fn visit_type(&mut self, ty: &Ty) {
        self.output.push_str(&ty.to_string())
    }

    fn visit_local_init_stmt(&mut self, stmt: &mut InitLocalStmt) {
        self.output.push_str(&format!(
            "{}let {}{}: ",
            " ".repeat(self.curr_indent),
            if stmt.mutable { "mut " } else { "" },
            stmt.name
        ));
        self.visit_type(&stmt.ty);
        self.output.push_str(" = ");
        self.visit_expr(&mut stmt.rhs);
        self.output.push_str(";")
    }

    fn visit_expr_stmt(&mut self, stmt: &mut ExprStmt) {
        self.output.push_str(&" ".repeat(self.curr_indent));
        self.visit_expr(&mut stmt.expr);
    }

    fn visit_semi_stmt(&mut self, stmt: &mut SemiStmt) {
        self.output.push_str(&" ".repeat(self.curr_indent));
        self.visit_expr(&mut stmt.expr);
        self.output.push_str(";");
    }

    fn visit_literal_expr(&mut self, expr: &mut LitExpr) {
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
            LitExpr::Float(f_str, _float_type) => f_str.to_string(),
            LitExpr::Bool(bool) => bool.to_string(),
            LitExpr::Tuple(_tuple) => panic!(),
        };
        self.output.push_str(&expr)
    }

    fn visit_binary_expr(&mut self, expr: &mut BinaryExpr) {
        let prev_expression = self.expression_mode;
        self.expression_mode = true;
        self.output.push_str("(");
        self.visit_expr(&mut expr.lhs);
        self.output.push_str(" ");
        self.visit_binary_op(&mut expr.op);
        self.output.push_str(" ");
        self.visit_expr(&mut expr.rhs);
        self.output.push_str(")");
        self.expression_mode = prev_expression;
    }

    // Have two of these, one for if_expression
    fn visit_if_expr(&mut self, expr: &mut IfExpr) {
        self.output.push_str("if ");
        self.visit_expr(&mut expr.condition);
        self.output.push_str(" ");
        self.visit_block_expr(&mut expr.then);
        // self.output
        //     .push_str(&format!("\n{}}}", " ".repeat(self.curr_indent)));
        if let Some(otherwise) = &mut expr.otherwise {
            self.output.push_str(&format!(" else "));
            self.visit_block_expr(otherwise);
            // if !self.expression_mode {
            //     self.output.push_str("\n")
            // }
            // self.output
            //     .push_str(&format!("\n{}}}", " ".repeat(self.curr_indent)));
        }
    }

    fn visit_block_expr(&mut self, expr: &mut BlockExpr) {
        self.output.push_str("{\n");
        self.enter_scope();
        for stmt in expr.stmts.iter_mut() {
            // self.output
            //     .push_str(&format!("{}", " ".repeat(self.curr_indent)));
            self.visit_stmt(stmt);
            self.output.push_str("\n");
        }
        self.exit_scope();
        self.output
            .push_str(&format!("{}}}", " ".repeat(self.curr_indent)));
    }

    fn visit_ident_expr(&mut self, expr: &mut IdentExpr) {
        self.visit_name(&expr.name)
    }

    fn visit_binary_op(&mut self, op: &mut BinaryOp) {
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

    fn visit_tuple_expr(&mut self, expr: &mut TupleExpr) {
        self.output.push_str("(");
        for (i, expr) in (&mut expr.tuple).iter_mut().enumerate() {
            if i != 0 {
                self.output.push_str(", ")
            }
            self.visit_expr(expr)
        }
        self.output.push_str(")")
    }

    fn visit_assign_expr(&mut self, expr: &mut AssignExpr) {
        self.visit_name(&expr.name);
        self.output.push_str(" = ");
        self.visit_expr(&mut expr.rhs);
    }

    fn visit_name(&mut self, name: &String) {
        self.output.push_str(name)
    }
}
