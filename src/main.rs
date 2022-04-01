extern crate core;

use crate::ast::function::Function;
use crate::context::Context;
use crate::visitor::emit_visitor::EmitVisitor;
use crate::visitor::visitor::Visitor;

mod ast;
mod context;
mod policy;
mod visitor;

fn main() {
    // TODO: Parse configuation files as generation policies
    let mut ctx = Context::default();
    let main = Function::create_main_fn(&mut ctx);
    let mut emit_visitor = EmitVisitor::default();
    emit_visitor.visit_function(&main);
    println!("{}", emit_visitor.output());
}
