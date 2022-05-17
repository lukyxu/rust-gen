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
use std::error::Error;
use std::fmt::{Display, Formatter};

pub struct GeneratorOutput {
    pub program: String,
    pub statistics: FullStatistics,
    pub expected_checksum: u128,
}

#[derive(Debug)]
pub struct GeneratorError {
    pub statistics: FullStatistics,
    pub error_message: String,
}

impl Error for GeneratorError {}

impl Display for GeneratorError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", self.error_message)
    }
}

pub fn run_generator(
    seed: Option<u64>,
    policy: &Policy,
) -> Result<GeneratorOutput, GeneratorError> {
    let add_checksum = true;
    let mut ctx = Context::with_policy(seed, policy);
    let mut file = RustFile::generate_file(&mut ctx).ok_or(GeneratorError {
        statistics: ctx.statistics.clone().into(),
        error_message: "Unable to generate rust file".to_string(),
    })?;
    let mut expr_visitor = ExprVisitor::default();
    expr_visitor.visit_file(&mut file);
    // Make program compilable
    let mut checksum_gen_visitor = ChecksumGenVisitor::new(add_checksum);
    checksum_gen_visitor.visit_file(&mut file);
    let mut checksum_eval_visitor = ChecksumEvalVisitor::default();
    checksum_eval_visitor.visit_file(&mut file);
    let mut emit_visitor = EmitVisitor::default();
    emit_visitor.visit_file(&mut file);
    Ok(GeneratorOutput {
        program: emit_visitor.output(),
        statistics: std::mem::take(&mut ctx.statistics.into()),
        expected_checksum: checksum_eval_visitor.res.unwrap(),
    })
}

fn _print_program(file: &mut RustFile) {
    let mut emit_visitor = EmitVisitor::default();
    emit_visitor.visit_file(file);
    println!("{}", emit_visitor.output());
}

#[test]
fn generator_bench() {
    for i in 0..10 {
        run_generator(Some(i), &Policy::default()).unwrap();
    }
}
