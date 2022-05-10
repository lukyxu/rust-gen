use crate::ast::function::Function;
use crate::context::Context;
use crate::policy::Policy;
use crate::statistics::Statistics;
use crate::visitor::base_visitor::Visitor;
use crate::visitor::checksum_eval_visitor::ChecksumEvalVisitor;
use crate::visitor::checksum_gen_visitor::ChecksumGenVisitor;
use crate::visitor::emit_visitor::EmitVisitor;
use crate::visitor::expr_visitor::ExprVisitor;

pub struct GeneratorOutput {
    pub program: String,
    pub statistics: Statistics,
    pub expected_checksum: u128,
}

pub fn run_generator(seed: Option<u64>, policy: &Policy) -> GeneratorOutput {
    let add_checksum = true;
    let mut ctx = Context::with_policy(seed, &policy);
    let mut main = Function::create_main_fn(&mut ctx);
    let mut expr_visitor = ExprVisitor::new();
    expr_visitor.visit_function(&mut main);
    // Make program compilable
    let mut checksum_gen_visitor = ChecksumGenVisitor::new(add_checksum);
    checksum_gen_visitor.visit_function(&mut main);
    let mut checksum_eval_visitor = ChecksumEvalVisitor::new();
    checksum_eval_visitor.visit_function(&mut main);
    let mut emit_visitor = EmitVisitor::default();
    emit_visitor.visit_function(&mut main);
    GeneratorOutput {
        program: emit_visitor.output(),
        statistics: std::mem::take(&mut ctx.statistics),
        expected_checksum: checksum_eval_visitor.res.unwrap(),
    }
}

fn _print_program(main: &mut Function) {
    let mut emit_visitor = EmitVisitor::default();
    emit_visitor.visit_function(main);
    println!("{}", emit_visitor.output())
}

#[test]
fn generator_bench() {
    for i in 0..100 {
        run_generator(Some(i), &Policy::default());
    }
}
