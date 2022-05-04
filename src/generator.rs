use crate::ast::function::Function;
use crate::context::Context;
use crate::policy::Policy;
use crate::visitor::base_visitor::Visitor;
use crate::visitor::checksum_eval_visitor::ChecksumEvalVisitor;
use crate::visitor::checksum_gen_visitor::ChecksumGenVisitor;
use crate::visitor::emit_visitor::EmitVisitor;
use crate::visitor::expr_visitor::ExprVisitor;

pub struct GeneratorOutput {
    pub program: String,
    pub expected_checksum: u128,
}

// TODO: Take in reference to policy
pub fn run_generator(seed: Option<u64>, policy: Policy) -> GeneratorOutput {
    let mut ctx = Context::with_policy(seed, policy);
    let mut main = Function::create_main_fn(&mut ctx);
    // Make program compilable
    // _print_program(&mut main);
    let mut expr_visitor = ExprVisitor::new();
    // Fix runtime errors
    expr_visitor.visit_function(&mut main);
    let mut checksum_gen_visitor = ChecksumGenVisitor::new();
    checksum_gen_visitor.visit_function(&mut main);
    // _print_program(&mut main);
    let mut checksum_eval_visitor = ChecksumEvalVisitor::new();
    checksum_eval_visitor.visit_function(&mut main);
    let mut emit_visitor = EmitVisitor::default();
    emit_visitor.visit_function(&mut main);
    GeneratorOutput {
        program: emit_visitor.output(),
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
        run_generator(Some(i), Policy::default());
    }
}
