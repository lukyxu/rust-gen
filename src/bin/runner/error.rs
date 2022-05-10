use std::error::Error;
use std::fmt::{Display, Formatter};
use std::process::Output;

#[derive(Debug)]
pub enum RunnerError {
    Compilation(CompilationError),
    Run(RunError),
    DifferingChecksum(DifferingChecksumError),
    UnexpectedChecksum(UnexpectedChecksumError),
}

impl RunnerError {
    pub fn folder_name(&self) -> &'static str {
        match self {
            RunnerError::Compilation(_) => "compilation_error",
            RunnerError::Run(_) => "run_error",
            RunnerError::DifferingChecksum(_) => "differing_checksum_error",
            RunnerError::UnexpectedChecksum(_) => "unexpected_checksum_error",
        }
    }
}

impl Error for RunnerError {}

impl Display for RunnerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RunnerError::Compilation(err) => Display::fmt(err, f),
            RunnerError::Run(err) => Display::fmt(err, f),
            RunnerError::DifferingChecksum(err) => Display::fmt(err, f),
            RunnerError::UnexpectedChecksum(err) => Display::fmt(err, f),
        }
    }
}

#[derive(Debug)]
pub struct CompilationError {
    pub rust_file_path: String,
    pub status_code: i32,
    pub std_err: String,
}

impl CompilationError {
    pub fn new(rust_file_path: &str, output: Output) -> CompilationError {
        return CompilationError {
            rust_file_path: rust_file_path.to_owned(),
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
        writeln!(f, "Failed to compile {}", self.rust_file_path)?;
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
    pub rust_file_path: String,
    pub bin_file_path: String,
    pub status_code: i32,
    pub std_err: String,
}

impl RunError {
    pub fn new(rust_file_path: &str, bin_file_path: &str, output: Output) -> RunError {
        return RunError {
            rust_file_path: rust_file_path.to_owned(),
            bin_file_path: bin_file_path.to_owned(),
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
        writeln!(f, "Failed to run {}", self.rust_file_path)?;
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
    pub files: Vec<String>,
    pub checksums: Vec<(&'static str, u128)>,
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
    pub files: Vec<String>,
    pub expected_checksum: u128,
    pub checksums: Vec<(&'static str, u128)>,
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