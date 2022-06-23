use crate::ast::expr::{ArrayExpr, AssignExpr, BinaryExpr, BlockExpr, CastExpr, Expr, Field, FieldExpr, FieldStructExpr, FunctionCallExpr, IdentExpr, IfExpr, IndexExpr, LitExpr, LitIntExpr, LitIntTy, Member, PlaceExpr, ReferenceExpr, StructExpr, TupleExpr, TupleStructExpr, UnaryExpr};
use crate::ast::file::RustFile;
use crate::ast::function::Function;
use crate::ast::item::{FunctionItem, Item, StructItem};
use crate::ast::op::{BinaryOp, UnaryOp};
use crate::ast::stmt::{CustomStmt, DeclLocalStmt, ExprStmt, InitLocalStmt, SemiStmt, Stmt};
use crate::ast::ty::{IntTy, Lifetime, StructTy, Ty, UIntTy};
use crate::visitor::base_visitor::Visitor;

/// Visitor used to print a given ast.
/// Output can be parsed through rustfmt to reformat it to a given style.
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
        assert!(
            !self.output.is_empty(),
            "Run visit before accessing emit visitor output"
        );
        self.output.clone()
    }
}

impl Visitor for EmitVisitor {
    fn enter_scope(&mut self) {
        self.curr_indent += self.indentation;
    }
    fn exit_scope(&mut self) {
        self.curr_indent -= self.indentation;
    }

    fn visit_name(&mut self, name: &str) {
        self.output.push_str(name);
    }

    fn visit_type(&mut self, ty: &Ty) {
        self.output.push_str(&ty.to_string());
    }

    fn visit_file(&mut self, file: &mut RustFile) {
        for (i, item) in (&mut file.items).iter_mut().enumerate() {
            if i > 0 {
                self.output.push_str("\n\n");
            }
            self.visit_item(item);
        }
    }

    fn visit_function(&mut self, function: &mut Function) {
        self.output.push_str(&format!("fn {}() ", function.name));
        if !function.return_ty.is_unit() {
            self.output.push_str(&format!("-> {} ", function.return_ty.to_string()));
        }
        self.visit_block_expr(&mut function.block);
    }

    fn visit_struct_item(&mut self, item: &mut StructItem) {
        // TODO: Remove this
        let mut derives = vec!["PartialEq"];
        if item.struct_ty.is_copy() {
            derives.push("Copy");
        }
        if item.struct_ty.is_clone() {
            derives.push("Clone");
        }
        self.output
            .push_str(&format!("#[derive({})]\n", derives.join(", ")));
        let lifetimes = (!item.struct_ty.lifetimes().is_empty())
            .then(|| {
                item.struct_ty
                    .lifetimes()
                    .iter()
                    .map(Lifetime::to_string)
                    .collect::<Vec<String>>()
                    .join(", ")
            })
            .map(|lifetimes| format!("<{}>", lifetimes))
            .unwrap_or_default();
        match &item.struct_ty {
            StructTy::Field(field_struct) => self.output.push_str(&format!(
                "struct {}{} {{\n{}\n}} ",
                field_struct.name,
                lifetimes,
                field_struct
                    .fields
                    .iter()
                    .map(|f| format!(
                        "{}{},",
                        " ".repeat(self.curr_indent + self.indentation),
                        f.to_string()
                    ))
                    .collect::<Vec<String>>()
                    .join("\n")
            )),
            StructTy::Tuple(tuple_struct) => {
                self.output.push_str(&format!(
                    "struct {}{}{};",
                    tuple_struct.name,
                    lifetimes,
                    tuple_struct.fields.to_string()
                ));
            }
        }
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
        self.output.push(';');
    }

    fn visit_expr_stmt(&mut self, stmt: &mut ExprStmt) {
        self.output.push_str(&" ".repeat(self.curr_indent));
        self.visit_expr(&mut stmt.expr);
    }

    fn visit_semi_stmt(&mut self, stmt: &mut SemiStmt) {
        self.output.push_str(&" ".repeat(self.curr_indent));
        self.visit_expr(&mut stmt.expr);
        self.output.push(';');
    }

    fn visit_custom_stmt(&mut self, stmt: &mut CustomStmt) {
        self.output.push_str(&" ".repeat(self.curr_indent));
        match stmt {
            CustomStmt::Println(stmt) => self.output.push_str(
                format!("println!(\"{}\", {})", stmt.format, stmt.args.join(", ")).as_str(),
            ),
            CustomStmt::Assert(stmt) => {
                let lhs = &mut stmt.lhs_expr;
                let rhs = &mut match &stmt.rhs_expr {
                    None => lhs.clone(),
                    Some(eval_expr) => eval_expr.clone().into(),
                };
                self.output.push_str("assert_eq!(");
                self.visit_expr(lhs);
                self.output.push_str(", ");
                self.visit_expr(rhs);
                self.output.push_str(");");
            }
        };
    }

    fn visit_literal_expr(&mut self, expr: &mut LitExpr) {
        let expr = match expr {
            LitExpr::Str(str) => (*str).to_string(),
            LitExpr::Byte(byte) => byte.to_string(),
            LitExpr::Char(char) => char.to_string(),
            LitExpr::Int(LitIntExpr {
                value: u128,
                ty: int_type,
            }) => match int_type {
                LitIntTy::Signed(t) => {
                    let int_str = match t {
                        IntTy::ISize => (*u128 as isize).to_string(),
                        IntTy::I8 => (*u128 as i8).to_string(),
                        IntTy::I16 => (*u128 as i16).to_string(),
                        IntTy::I32 => (*u128 as i32).to_string(),
                        IntTy::I64 => (*u128 as i64).to_string(),
                        IntTy::I128 => (*u128 as i128).to_string(),
                    };
                    format!("{}_{}", int_str, (*t).to_string())
                }
                LitIntTy::Unsigned(t) => {
                    let uint_str = match t {
                        UIntTy::USize => (*u128 as usize).to_string(),
                        UIntTy::U8 => (*u128 as u8).to_string(),
                        UIntTy::U16 => (*u128 as u16).to_string(),
                        UIntTy::U32 => (*u128 as u32).to_string(),
                        UIntTy::U64 => (*u128 as u64).to_string(),
                        UIntTy::U128 => u128.to_string(),
                    };
                    format!("{}_{}", uint_str, (*t).to_string())
                }
            },
            LitExpr::Float(f_str, _float_type) => (*f_str).to_string(),
            LitExpr::Bool(bool) => bool.to_string(),
        };
        self.output.push_str(&expr);
    }

    fn visit_binary_expr(&mut self, expr: &mut BinaryExpr) {
        self.output.push('(');
        if expr.op.is_function_call() {
            self.output.push('(');
            self.visit_expr(&mut expr.lhs);
            self.output.push_str(").");
            self.visit_binary_op(&mut expr.op);
            self.output.push('(');
            self.visit_expr(&mut expr.rhs);
            self.output.push(')');
        } else {
            self.visit_expr(&mut expr.lhs);
            self.output.push(' ');
            self.visit_binary_op(&mut expr.op);
            self.output.push(' ');
            self.visit_expr(&mut expr.rhs);
        }
        self.output.push(')');
    }

    fn visit_unary_expr(&mut self, expr: &mut UnaryExpr) {
        self.visit_unary_op(&mut expr.op);
        self.output.push('(');
        self.visit_expr(&mut expr.expr);
        self.output.push(')');
    }

    fn visit_cast_expr(&mut self, expr: &mut CastExpr) {
        self.output.push('(');
        self.visit_expr(&mut expr.expr);
        self.output.push_str(" as ");
        self.visit_type(&expr.ty);
        self.output.push(')');
    }

    // Have two of these, one for if_expression
    fn visit_if_expr(&mut self, expr: &mut IfExpr) {
        self.output.push_str("if ");
        self.visit_expr(&mut expr.condition);
        self.output.push(' ');
        self.visit_block_expr(&mut expr.then);
        // self.output
        //     .push_str(&format!("\n{}}}", " ".repeat(self.curr_indent)));
        if let Some(otherwise) = &mut expr.otherwise {
            self.output.push_str(" else ");
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
        for stmt in &mut expr.stmts {
            // self.output
            //     .push_str(&format!("{}", " ".repeat(self.curr_indent)));
            self.visit_stmt(stmt);
            self.output.push('\n');
        }
        self.exit_scope();
        self.output
            .push_str(&format!("{}}}", " ".repeat(self.curr_indent)));
    }

    fn visit_ident_expr(&mut self, expr: &mut IdentExpr) {
        self.visit_name(&expr.name);
    }

    fn visit_tuple_expr(&mut self, expr: &mut TupleExpr) {
        self.output.push('(');
        for (i, expr) in (&mut expr.tuple).iter_mut().enumerate() {
            if i != 0 {
                self.output.push_str(", ");
            }
            self.visit_expr(expr);
        }
        if expr.tuple.len() == 1 {
            self.output.push(',');
        }
        self.output.push(')');
    }

    fn visit_assign_expr(&mut self, expr: &mut AssignExpr) {
        match &mut expr.place {
            PlaceExpr::Field(expr) => self.visit_field_expr(expr),
            PlaceExpr::Index(expr) => self.visit_index_expr(expr),
            PlaceExpr::Ident(expr) => self.visit_ident_expr(expr),
        }
        self.output.push_str(" = ");
        self.visit_expr(&mut expr.rhs);
    }

    fn visit_field_expr(&mut self, expr: &mut FieldExpr) {
        self.output.push('(');
        self.visit_expr(&mut expr.base);
        self.output.push('.');
        match &expr.member {
            Member::Named(name) => self.visit_name(name),
            Member::Unnamed(usize) => self.output.push_str(&usize.to_string()),
        }
        self.output.push(')');
    }

    fn visit_array_expr(&mut self, expr: &mut ArrayExpr) {
        self.output.push('[');
        for (i, expr) in (&mut expr.array).iter_mut().enumerate() {
            if i != 0 {
                self.output.push_str(", ");
            }
            self.visit_expr(expr);
        }
        self.output.push(']');
    }

    fn visit_index_expr(&mut self, expr: &mut IndexExpr) {
        self.output.push('(');
        self.visit_expr(&mut expr.base);
        self.output.push('[');
        self.visit_expr(&mut expr.index);
        self.output.push(']');
        self.output.push(')');
    }

    fn visit_field_struct_expr(&mut self, expr: &mut FieldStructExpr) {
        self.output.push_str(&format!("{} {{ ", expr.struct_name));
        for (i, field) in expr.fields.iter_mut().enumerate() {
            if i > 0 {
                self.output.push_str(", ");
            }
            self.visit_field(field);
        }
        self.output.push_str(" }");
    }

    fn visit_tuple_struct_expr(&mut self, expr: &mut TupleStructExpr) {
        self.output.push_str(&expr.struct_name);
        self.visit_tuple_expr(&mut expr.fields);
    }

    fn visit_reference_expr(&mut self, expr: &mut ReferenceExpr) {
        self.output.push_str(&format!(
            "&{}(",
            expr.mutability.then(|| "mut ").unwrap_or_default()
        ));
        self.visit_expr(&mut expr.expr);
        self.output.push(')');
    }

    fn visit_function_call_expr(&mut self, expr: &mut FunctionCallExpr) {
        self.output.push_str(&format!("{}()",expr.name))
    }

    fn visit_field(&mut self, field: &mut Field) {
        self.output.push_str(&format!("{}: ", &field.name));
        self.visit_expr(&mut field.expr);
    }

    fn visit_unary_op(&mut self, op: &mut UnaryOp) {
        self.output.push_str(&op.to_string());
    }
    fn visit_binary_op(&mut self, op: &mut BinaryOp) {
        self.output.push_str(&op.to_string());
    }
}
