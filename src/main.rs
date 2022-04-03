extern crate core;

use crate::ast::function::Function;
use crate::context::Context;
use crate::visitor::emit_visitor::EmitVisitor;
use crate::visitor::expr_visitor::ExprVisitor;
use crate::visitor::visitor::Visitor;
use clap::Parser;

mod ast;
mod context;
mod policy;
mod visitor;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    #[clap(short, long)]
    seed: Option<u64>
}

fn main() {
    let args: Args = Args::parse();
    // TODO: Parse configuation files as generation policies
    run_generator(args.seed)
}

fn run_generator(seed: Option<u64>) {
    let mut ctx = Context::new(seed);
    let mut main = Function::create_main_fn(&mut ctx);
    // print_output(&mut main);
    // Make program compilable
    let mut expr_visitor = ExprVisitor::default();
    // Make sure there is no runtime errors

    // Print program
    expr_visitor.visit_function(&mut main);
    print_output(&mut main)
}

fn print_output(main: &mut Function) {
    let mut emit_visitor = EmitVisitor::default();
    emit_visitor.visit_function(main);
    println!("{}", emit_visitor.output());
}

#[test]
fn test() {
    for i in 1..100 {
        run_generator(Some(i))
    }
}