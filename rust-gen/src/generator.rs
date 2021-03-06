//! Program generator.

use crate::ast::file::RustFile;
use crate::context::Context;
use crate::policy::Policy;
use crate::visitor::base_visitor::Visitor;
use crate::visitor::checksum_eval_visitor::ChecksumEvalVisitor;

use crate::statistics::generation::{FullGenerationStatistics, GenerationStatistics};
use crate::statistics::program::FullProgramStatistics;
use crate::visitor::assert_gen_visitor::AssertGenVisitor;
use crate::visitor::checksum_gen_visitor::ChecksumGenVisitor;
use crate::visitor::emit_visitor::EmitVisitor;
use crate::visitor::expr_visitor::ExprVisitor;
use crate::visitor::statistics_visitor::StatisticsVisitor;
use std::error::Error;
use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub struct GeneratorOutput {
    pub program: String,
    pub generation_statistics: FullGenerationStatistics,
    pub program_statistics: Option<FullProgramStatistics>,
    pub expected_checksum: Option<u128>,
}

#[derive(Debug, Clone)]
/// Error in generation.
pub struct GeneratorError {
    pub statistics: Box<GenerationStatistics>,
    pub error_message: String,
}

impl Error for GeneratorError {}

impl Display for GeneratorError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", self.error_message)
    }
}

pub type GeneratorResult = Result<GeneratorOutput, GeneratorError>;

pub fn run_generator(
    seed: Option<u64>,
    policy: &Policy,
    add_checksum: bool,
    add_assertions: bool,
) -> GeneratorResult {
    let mut ctx = Context::with_policy(seed, policy);
    let mut file = RustFile::generate_file(&mut ctx).ok_or(GeneratorError {
        statistics: Box::new(ctx.statistics.clone().into()),
        error_message: "Unable to generate rust file".to_string(),
    })?;
    let mut expr_visitor = ExprVisitor::default();
    expr_visitor.visit_file(&mut file);
    // _print_program(&mut file);
    let mut statistic_visitor = StatisticsVisitor::default();
    statistic_visitor.visit_file(&mut file);
    let mut checksum_gen_visitor = ChecksumGenVisitor::new(true, add_checksum, false);
    checksum_gen_visitor.visit_file(&mut file);
    let mut assert_gen_visitor = AssertGenVisitor::new(false, add_assertions, true);
    assert_gen_visitor.visit_file(&mut file);
    // _print_program(&mut file);
    let mut checksum_eval_visitor = ChecksumEvalVisitor::default();
    checksum_eval_visitor.visit_file(&mut file);
    let mut emit_visitor = EmitVisitor::default();
    emit_visitor.visit_file(&mut file);

    Ok(GeneratorOutput {
        program: emit_visitor.output(),
        generation_statistics: std::mem::take(&mut ctx.statistics.into()),
        program_statistics: Some(std::mem::take(&mut statistic_visitor.statistics))
            .map(|stats| stats.into()),
        expected_checksum: if add_checksum {
            checksum_eval_visitor.res
        } else {
            None
        },
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
        run_generator(Some(i), &Policy::default(), true, true).unwrap();
    }
}
