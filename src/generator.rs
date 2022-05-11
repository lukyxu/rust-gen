use crate::ast::file::RustFile;
use crate::ast::function::Function;
use crate::context::Context;
use crate::policy::Policy;
use crate::statistics::FullStatistics;
use crate::visitor::base_visitor::Visitor;
use crate::visitor::checksum_eval_visitor::ChecksumEvalVisitor;
use crate::visitor::checksum_gen_visitor::ChecksumGenVisitor;
use crate::visitor::emit_visitor::EmitVisitor;
use crate::visitor::expr_visitor::ExprVisitor;

pub struct GeneratorOutput {
    pub program: String,
    pub statistics: FullStatistics,
    pub expected_checksum: u128,
}

pub fn run_generator(seed: Option<u64>, policy: &Policy) -> GeneratorOutput {
    let add_checksum = true;
    let mut ctx = Context::with_policy(seed, policy);
    let mut file = RustFile::generate_file(&mut ctx).expect("Cannot create main function");
    let mut expr_visitor = ExprVisitor::default();
    expr_visitor.visit_file(&mut file);
    // Make program compilable
    let mut checksum_gen_visitor = ChecksumGenVisitor::new(add_checksum);
    checksum_gen_visitor.visit_file(&mut file);
    let mut checksum_eval_visitor = ChecksumEvalVisitor::default();
    checksum_eval_visitor.visit_file(&mut file);
    let mut emit_visitor = EmitVisitor::default();
    emit_visitor.visit_file(&mut file);
    GeneratorOutput {
        program: emit_visitor.output(),
        statistics: std::mem::take(&mut ctx.statistics.into()),
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
