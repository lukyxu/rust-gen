use crate::generator::GeneratorError;
use crate::runtime::run::ChecksumMapping;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::path::PathBuf;
use std::process::Output;
use std::time::Duration;

#[derive(Debug)]
pub enum RunnerError {
    GeneratorTimeout(GeneratorTimeoutError),
    Generator(GeneratorError),
    Compilation(CompilationError),
    Run(RunError),
    DifferingChecksum(DifferingChecksumError),
    UnexpectedChecksum(UnexpectedChecksumError),
    RustFmt(RustFmtError),
}

impl RunnerError {
    pub fn folder_name(&self) -> &'static str {
        match self {
            RunnerError::GeneratorTimeout(_) => "generator_timeout",
            RunnerError::Generator(_) => "generator_error",
            RunnerError::Compilation(_) => "compilation_error",
            RunnerError::Run(_) => "run_error",
            RunnerError::DifferingChecksum(_) => "differing_checksum_error",
            RunnerError::UnexpectedChecksum(_) => "unexpected_checksum_error",
            RunnerError::RustFmt(_) => "rustfmt_error",
        }
    }

    pub fn files(&self) -> Vec<PathBuf> {
        match self {
            RunnerError::GeneratorTimeout(_err) => vec![],
            RunnerError::Generator(_err) => vec![],
            RunnerError::Compilation(err) => err.files(),
            RunnerError::Run(err) => err.files(),
            RunnerError::DifferingChecksum(err) => err.files(),
            RunnerError::UnexpectedChecksum(err) => err.files(),
            RunnerError::RustFmt(err) => err.files(),
        }
    }
}

impl Error for RunnerError {}

impl Display for RunnerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RunnerError::GeneratorTimeout(err) => Display::fmt(err, f),
            RunnerError::Generator(err) => Display::fmt(err, f),
            RunnerError::Compilation(err) => Display::fmt(err, f),
            RunnerError::Run(err) => Display::fmt(err, f),
            RunnerError::DifferingChecksum(err) => Display::fmt(err, f),
            RunnerError::UnexpectedChecksum(err) => Display::fmt(err, f),
            RunnerError::RustFmt(err) => Display::fmt(err, f),
        }
    }
}

impl Error for GeneratorTimeoutError {}

#[derive(Debug)]
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

impl From<GeneratorTimeoutError> for RunnerError {
    fn from(err: GeneratorTimeoutError) -> RunnerError {
        RunnerError::GeneratorTimeout(err)
    }
}

#[derive(Debug)]
pub struct CompilationError {
    pub rust_file_path: PathBuf,
    pub status_code: i32,
    pub std_err: String,
}

impl CompilationError {
    pub fn new(rust_file_path: PathBuf, output: &Output) -> CompilationError {
        return CompilationError {
            rust_file_path: rust_file_path.to_owned(),
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
            "Failed to compile {}",
            self.rust_file_path.to_str().unwrap()
        )?;
        writeln!(f, "Status code {}", self.status_code)?;
        writeln!(f, "Standard error")?;
        writeln!(f, "{}", self.std_err)
    }
}

impl From<CompilationError> for RunnerError {
    fn from(err: CompilationError) -> RunnerError {
        RunnerError::Compilation(err)
    }
}

#[derive(Debug)]
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

impl From<RunError> for RunnerError {
    fn from(err: RunError) -> RunnerError {
        RunnerError::Run(err)
    }
}

#[derive(Debug)]
pub struct DifferingChecksumError {
    pub files: Vec<PathBuf>,
    pub checksums: ChecksumMapping,
}

impl DifferingChecksumError {
    pub fn files(&self) -> Vec<PathBuf> {
        self.files.clone()
    }
}

impl Error for DifferingChecksumError {}

impl Display for DifferingChecksumError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Differing checksum results between runs")?;
        writeln!(f, "{:#?}", self.checksums)
    }
}

impl From<DifferingChecksumError> for RunnerError {
    fn from(err: DifferingChecksumError) -> RunnerError {
        RunnerError::DifferingChecksum(err)
    }
}

#[derive(Debug)]
pub struct UnexpectedChecksumError {
    pub files: Vec<PathBuf>,
    pub expected_checksum: u128,
    pub checksums: ChecksumMapping,
}

impl UnexpectedChecksumError {
    pub fn files(&self) -> Vec<PathBuf> {
        self.files.clone()
    }
}

impl Error for UnexpectedChecksumError {}

impl Display for UnexpectedChecksumError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Miscalculated checksum")?;
        writeln!(f, "Expected checksum {}", self.expected_checksum)?;
        writeln!(f, "Actual checksums {:#?}", self.checksums)
    }
}

impl From<UnexpectedChecksumError> for RunnerError {
    fn from(err: UnexpectedChecksumError) -> RunnerError {
        RunnerError::UnexpectedChecksum(err)
    }
}

#[derive(Debug)]
pub struct RustFmtError {
    pub files: Vec<PathBuf>,
    pub status_code: i32,
    pub std_err: String,
}

impl RustFmtError {
    pub fn new(output: Output, files: Vec<PathBuf>) -> RustFmtError {
        return RustFmtError {
            files,
            status_code: output.status.code().unwrap_or(-1),
            std_err: String::from_utf8_lossy(output.stderr.as_ref())
                .parse()
                .unwrap(),
        };
    }

    pub fn files(&self) -> Vec<PathBuf> {
        self.files.clone()
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

impl From<RustFmtError> for RunnerError {
    fn from(err: RustFmtError) -> RunnerError {
        RunnerError::RustFmt(err)
    }
}
