//! Runner errors.

use crate::generator::GeneratorError;
use crate::runtime::config::{OptLevel, RustVersion};
use crate::runtime::run::{RunOutput, SubRunOutput};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::path::PathBuf;
use std::process::Output;
use std::time::Duration;

#[derive(Debug, Clone)]
/// Errors related with the process of randomized differential testing.
pub enum RunnerError {
    /// Error in generation.
    GeneratorFailure(GeneratorError, RunOutput),
    /// Generation exceeded timeout.
    GeneratorTimeout(GeneratorTimeoutError, RunOutput),
    /// Error in compilation.
    CompilationFailure(Vec<CompilationError>, RunOutput),
    /// Compilation exceeded timeout.
    CompilationTimeout(Vec<CompilationTimeoutError>, RunOutput),
    /// Error in running executable.
    RunFailure(Vec<RunError>, RunOutput),
    /// Running executable exceeded timeout.
    RunTimeout(Vec<RunTimeoutError>, RunOutput),
    /// Sub-runs contained differing checksums.
    DifferingChecksum(DifferingChecksumError, RunOutput),
    /// Sub-runs contains checksums which are different from the expected checksum.
    UnexpectedChecksum(UnexpectedChecksumError, RunOutput),
    /// Error in running rustfmt.
    RustFmtFailure(RustFmtError, RunOutput),
    /// Rustfmt exceeded timeout.
    RustFmtTimeout(RustFmtTimeoutError, RunOutput),
}

impl RunnerError {
    pub fn error_kind(&self) -> &'static str {
        match self {
            RunnerError::GeneratorFailure(_, _) => "generator_failure",
            RunnerError::GeneratorTimeout(_, _) => "generator_timeout",
            RunnerError::CompilationFailure(_, _) => "compilation_failure",
            RunnerError::CompilationTimeout(_, _) => "compilation_timeout",
            RunnerError::RunFailure(_, _) => "run_failure",
            RunnerError::RunTimeout(_, _) => "run_timeout",
            RunnerError::DifferingChecksum(_, _) => "differing_checksum",
            RunnerError::UnexpectedChecksum(_, _) => "unexpected_checksum",
            RunnerError::RustFmtFailure(_, _) => "rustfmt_failure",
            RunnerError::RustFmtTimeout(_, _) => "rustfmt_timeout",
        }
    }

    pub fn run_output(&self) -> &RunOutput {
        match self {
            RunnerError::GeneratorFailure(_, run_output)
            | RunnerError::GeneratorTimeout(_, run_output)
            | RunnerError::CompilationFailure(_, run_output)
            | RunnerError::CompilationTimeout(_, run_output)
            | RunnerError::RunFailure(_, run_output)
            | RunnerError::RunTimeout(_, run_output)
            | RunnerError::DifferingChecksum(_, run_output)
            | RunnerError::UnexpectedChecksum(_, run_output)
            | RunnerError::RustFmtFailure(_, run_output)
            | RunnerError::RustFmtTimeout(_, run_output) => run_output,
        }
    }

    pub fn files(&self) -> Vec<PathBuf> {
        self.run_output().files.clone()
    }
}

impl Error for RunnerError {}

fn display_fmt_array<T: Error>(errors: &Vec<T>, f: &mut Formatter<'_>) -> std::fmt::Result {
    for (i, err) in errors.iter().enumerate() {
        if i == errors.len() - 1 {
            return Display::fmt(err, f);
        }
        Display::fmt(err, f)?
    }
    panic!()
}

impl Display for RunnerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RunnerError::GeneratorTimeout(err, _) => Display::fmt(err, f),
            RunnerError::GeneratorFailure(err, _) => Display::fmt(err, f),
            RunnerError::CompilationFailure(errors, _) => display_fmt_array(errors, f),
            RunnerError::CompilationTimeout(errors, _) => display_fmt_array(errors, f),
            RunnerError::RunFailure(errors, _) => display_fmt_array(errors, f),
            RunnerError::RunTimeout(errors, _) => display_fmt_array(errors, f),
            RunnerError::DifferingChecksum(err, _) => Display::fmt(err, f),
            RunnerError::UnexpectedChecksum(err, _) => Display::fmt(err, f),
            RunnerError::RustFmtFailure(err, _) => Display::fmt(err, f),
            RunnerError::RustFmtTimeout(err, _) => Display::fmt(err, f),
        }
    }
}

impl Error for GeneratorTimeoutError {}

#[derive(Debug, Clone)]
/// Generation exceeded timeout.
pub struct GeneratorTimeoutError {
    pub duration: Duration,
}

impl GeneratorTimeoutError {
    pub fn new(duration: Duration) -> GeneratorTimeoutError {
        GeneratorTimeoutError { duration }
    }
}

impl Display for GeneratorTimeoutError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "Generator timeout. Timeout of {} seconds exceeded.",
            self.duration.as_secs()
        )
    }
}

#[derive(Debug, Clone)]
/// Error in compilation.
pub struct CompilationError {
    pub rust_file_path: PathBuf,
    pub opt: OptLevel,
    pub version: RustVersion,
    pub status_code: i32,
    pub std_err: String,
}

impl CompilationError {
    pub fn new(
        rust_file_path: PathBuf,
        output: &Output,
        opt: OptLevel,
        version: RustVersion,
    ) -> CompilationError {
        return CompilationError {
            rust_file_path: rust_file_path.to_owned(),
            opt,
            version,
            status_code: output.status.code().unwrap_or(-1),
            std_err: String::from_utf8_lossy(output.stderr.as_ref())
                .parse()
                .unwrap(),
        };
    }
}

impl Error for CompilationError {}

impl Display for CompilationError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "Failed to compile {} with optimization {:?} and Rust version {:?}",
            self.rust_file_path.to_str().unwrap(),
            self.opt,
            self.version
        )?;
        writeln!(f, "Status code {}", self.status_code)?;
        writeln!(f, "Standard error")?;
        writeln!(f, "{}", self.std_err)
    }
}

impl Error for CompilationTimeoutError {}

#[derive(Debug, Clone)]
/// Compilation exceeded timeout.
pub struct CompilationTimeoutError {
    pub opt: OptLevel,
    pub version: RustVersion,
    pub duration: Duration,
}

impl CompilationTimeoutError {
    pub fn new(opt: OptLevel, version: RustVersion, duration: Duration) -> CompilationTimeoutError {
        CompilationTimeoutError {
            opt,
            version,
            duration,
        }
    }
}

impl Display for CompilationTimeoutError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "Compilation timeout for optimization {:?} and Rust version {:?}. Timeout of {} seconds exceeded.",
            self.opt,
            self.version,
            self.duration.as_secs()
        )
    }
}

#[derive(Debug, Clone)]
/// Error in running executable.
pub struct RunError {
    pub rust_file_path: PathBuf,
    pub bin_file_path: PathBuf,
    pub opt: OptLevel,
    pub version: RustVersion,
    pub status_code: i32,
    pub std_err: String,
}

impl RunError {
    pub fn new(rust_file_path: PathBuf, bin_file_path: PathBuf, opt: OptLevel, version: RustVersion, output: &Output) -> RunError {
        return RunError {
            rust_file_path: rust_file_path.to_owned(),
            bin_file_path: bin_file_path.to_owned(),
            opt,
            version,
            status_code: output.status.code().unwrap_or(-1),
            std_err: String::from_utf8_lossy(output.stderr.as_ref())
                .parse()
                .unwrap(),
        };
    }
}

impl Error for RunError {}

impl Display for RunError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Failed to run {} for optimization {:?} and Rust version {:?}", self.rust_file_path.to_str().unwrap(), self.opt, self.version)?;
        writeln!(f, "Status code {}", self.status_code)?;
        writeln!(f, "Standard error")?;
        writeln!(f, "{}", self.std_err)
    }
}

impl Error for RunTimeoutError {}

#[derive(Debug, Clone)]
/// Running executable exceeded timeout.
pub struct RunTimeoutError {
    pub opt: OptLevel,
    pub version: RustVersion,
    pub duration: Duration,
}

impl RunTimeoutError {
    pub fn new(opt: OptLevel, version: RustVersion, duration: Duration) -> RunTimeoutError {
        RunTimeoutError {
            opt,
            version,
            duration,
        }
    }
}

impl Display for RunTimeoutError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "Run timeout for optimization {:?} and Rust version {:?}. Timeout of {} seconds exceeded.",
            self.opt,
            self.version,
            self.duration.as_secs()
        )
    }
}

#[derive(Debug, Clone)]
/// Sub-runs contained differing checksums.
pub struct DifferingChecksumError {
    pub checksums: Vec<SubRunOutput>,
}

impl Error for DifferingChecksumError {}

impl Display for DifferingChecksumError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Differing checksum results between runs")?;
        writeln!(f, "{:#?}", self.checksums)
    }
}

#[derive(Debug, Clone)]
/// Sub-runs contains checksums which are different from the expected checksum.
pub struct UnexpectedChecksumError {
    pub expected_checksum: u128,
    pub checksums: Vec<SubRunOutput>,
}

impl Error for UnexpectedChecksumError {}

impl Display for UnexpectedChecksumError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Miscalculated checksum")?;
        writeln!(f, "Expected checksum {}", self.expected_checksum)?;
        writeln!(f, "Actual checksums {:#?}", self.checksums)
    }
}

#[derive(Debug, Clone)]
/// Error in running rustfmt.
pub struct RustFmtError {
    pub status_code: i32,
    pub std_err: String,
}

impl RustFmtError {
    pub fn new(output: Output) -> RustFmtError {
        return RustFmtError {
            status_code: output.status.code().unwrap_or(-1),
            std_err: String::from_utf8_lossy(output.stderr.as_ref())
                .parse()
                .unwrap(),
        };
    }
}

impl Error for RustFmtError {}

impl Display for RustFmtError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Failed to run rustfmt")?;
        writeln!(f, "Status code {}", self.status_code)?;
        writeln!(f, "Standard error")?;
        writeln!(f, "{}", self.std_err)
    }
}

impl Error for RustFmtTimeoutError {}

#[derive(Debug, Clone)]
/// Rustfmt exceeded timeout.
pub struct RustFmtTimeoutError {
    pub duration: Duration,
}

impl RustFmtTimeoutError {
    pub fn new(duration: Duration) -> RustFmtTimeoutError {
        RustFmtTimeoutError { duration }
    }
}

impl Display for RustFmtTimeoutError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "Rustfmt timeout. Timeout of {} seconds exceeded.",
            self.duration.as_secs()
        )
    }
}
