extern crate core;

use crate::ast::function::Function;
use crate::context::Context;
use crate::visitor::emit_visitor::EmitVisitor;
use crate::visitor::expr_visitor::ExprVisitor;
use crate::visitor::visitor::Visitor;
use rand::rngs::ThreadRng;

mod ast;
mod context;
mod policy;
mod visitor;

fn main() {
    // TODO: Parse configuation files as generation policies
    let mut ctx = Context::default();
    let mut main = Function::create_main_fn(&mut ctx);
    print_output(&mut main);
    let mut expr_visitor = ExprVisitor::default();
    expr_visitor.visit_function(&mut main);
    print_output(&mut main)
}

fn print_output(main: &mut Function) {
    let mut emit_visitor = EmitVisitor::default();
    emit_visitor.visit_function(main);
    println!("{}", emit_visitor.output());
}
