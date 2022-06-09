use crate::generator::GeneratorError;
use crate::runtime::config::{OptLevel, RustVersion};
use crate::runtime::run::{RunOutput, SubRunOutput};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::path::PathBuf;
use std::process::Output;
use std::time::Duration;

#[derive(Debug, Clone)]
pub enum RunnerError {
    GeneratorTimeout(GeneratorTimeoutError, RunOutput),
    Generator(GeneratorError, RunOutput),
    Compilation(Vec<CompilationError>, RunOutput),
    CompilationTimeout(Vec<CompilationTimeoutError>, RunOutput),
    Run(Vec<RunError>, RunOutput),
    RunTimeout(Vec<RunTimeoutError>, RunOutput),
    DifferingChecksum(DifferingChecksumError, RunOutput),
    UnexpectedChecksum(UnexpectedChecksumError, RunOutput),
    RustFmt(RustFmtError, RunOutput),
    RustFmtTimeout(RustFmtTimeoutError, RunOutput),
}

impl RunnerError {
    pub fn folder_name(&self) -> &'static str {
        match self {
            RunnerError::GeneratorTimeout(_, _) => "generator_timeout",
            RunnerError::Generator(_, _) => "generator_error",
            RunnerError::Compilation(_, _) => "compilation_error",
            RunnerError::CompilationTimeout(_, _) => "compilation_timeout_error",
            RunnerError::Run(_, _) => "run_error",
            RunnerError::RunTimeout(_, _) => "run_timeout_error",
            RunnerError::DifferingChecksum(_, _) => "differing_checksum_error",
            RunnerError::UnexpectedChecksum(_, _) => "unexpected_checksum_error",
            RunnerError::RustFmt(_, _) => "rustfmt_error",
            RunnerError::RustFmtTimeout(_, _) => "rustfmt_timeout_error",
        }
    }

    pub fn run_output(&self) -> &RunOutput {
        match self {
            RunnerError::GeneratorTimeout(_, run_output)
            | RunnerError::Generator(_, run_output)
            | RunnerError::Compilation(_, run_output)
            | RunnerError::CompilationTimeout(_, run_output)
            | RunnerError::Run(_, run_output)
            | RunnerError::RunTimeout(_, run_output)
            | RunnerError::DifferingChecksum(_, run_output)
            | RunnerError::UnexpectedChecksum(_, run_output)
            | RunnerError::RustFmt(_, run_output)
            | RunnerError::RustFmtTimeout(_, run_output) => run_output
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
            RunnerError::Generator(err, _) => Display::fmt(err, f),
            RunnerError::Compilation(errors, _) => display_fmt_array(errors, f),
            RunnerError::CompilationTimeout(errors, _) => display_fmt_array(errors, f),
            RunnerError::Run(errors, _) => display_fmt_array(errors, f),
            RunnerError::RunTimeout(errors, _) => display_fmt_array(errors, f),
            RunnerError::DifferingChecksum(err, _) => Display::fmt(err, f),
            RunnerError::UnexpectedChecksum(err, _) => Display::fmt(err, f),
            RunnerError::RustFmt(err, _) => Display::fmt(err, f),
            RunnerError::RustFmtTimeout(err, _) => Display::fmt(err, f),
        }
    }
}

impl Error for GeneratorTimeoutError {}

#[derive(Debug, Clone)]
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

    pub fn files(&self) -> Vec<PathBuf> {
        return vec![self.rust_file_path.clone()];
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
pub struct CompilationTimeoutError {
    pub opt: OptLevel,
    pub version: RustVersion,
    pub duration: Duration,
}

impl CompilationTimeoutError {
    pub fn new(opt: OptLevel, version: RustVersion, duration: Duration) -> CompilationTimeoutError {
        CompilationTimeoutError { opt, version,  duration }
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
pub struct RunError {
    pub rust_file_path: PathBuf,
    pub bin_file_path: PathBuf,
    pub status_code: i32,
    pub std_err: String,
}

impl RunError {
    pub fn new(rust_file_path: PathBuf, bin_file_path: PathBuf, output: &Output) -> RunError {
        return RunError {
            rust_file_path: rust_file_path.to_owned(),
            bin_file_path: bin_file_path.to_owned(),
            status_code: output.status.code().unwrap_or(-1),
            std_err: String::from_utf8_lossy(output.stderr.as_ref())
                .parse()
                .unwrap(),
        };
    }

    pub fn files(&self) -> Vec<PathBuf> {
        return vec![self.rust_file_path.clone(), self.bin_file_path.clone()];
    }
}

impl Error for RunError {}

impl Display for RunError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Failed to run {}", self.rust_file_path.to_str().unwrap())?;
        writeln!(f, "Status code {}", self.status_code)?;
        writeln!(f, "Standard error")?;
        writeln!(f, "{}", self.std_err)
    }
}

impl Error for RunTimeoutError {}

#[derive(Debug, Clone)]
pub struct RunTimeoutError {
    pub opt: OptLevel,
    pub version: RustVersion,
    pub duration: Duration,
}

impl RunTimeoutError {
    pub fn new(opt: OptLevel, version: RustVersion, duration: Duration) -> RunTimeoutError {
        RunTimeoutError { opt, version,  duration }
    }
}

impl Display for RunTimeoutError {
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
pub struct RustFmtError {
    pub run_output: RunOutput,
    pub status_code: i32,
    pub std_err: String,
}

impl RustFmtError {
    pub fn new(output: Output, run_output: RunOutput) -> RustFmtError {
        return RustFmtError {
            run_output,
            status_code: output.status.code().unwrap_or(-1),
            std_err: String::from_utf8_lossy(output.stderr.as_ref())
                .parse()
                .unwrap(),
        };
    }

    pub fn files(&self) -> Vec<PathBuf> {
        self.run_output.files.clone()
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
            "Compilation timeout. Timeout of {} seconds exceeded.",
            self.duration.as_secs()
        )
    }
}
