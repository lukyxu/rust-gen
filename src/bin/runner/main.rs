use clap::Parser;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};

use indicatif::{ProgressBar, ProgressStyle};
use rust_gen::generator::{run_generator, GeneratorOutput};
use rust_gen::policy::Policy;
use std::fs;
use std::path::Path;
use std::process::{Command, Output};
use std::str::FromStr;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    #[clap(short, long)]
    num_runs: Option<u64>,
    // #[clap(short, long)]
    // policy: Option<String>,
    #[clap(short, long, default_value = "output")]
    output_path: String,
    #[clap(short, long)]
    store_passing_programs: bool,
}

#[derive(Debug)]
enum RunnerError {
    Compilation(CompilationError),
    Run(RunError),
    DifferingChecksum(DifferingChecksumError),
    UnexpectedChecksum(UnexpectedChecksumError),
}

impl RunnerError {
    fn folder_name(&self) -> &'static str {
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

type RunOutput = Result<Vec<String>, RunnerError>;

// TODO: Make policy take in a pointer
fn run(seed: Option<u64>, policy: Policy, base_name: &str) -> RunOutput {
    // Generate program
    let GeneratorOutput {
        program,
        expected_checksum,
    } = run_generator(seed, policy);

    // Save program
    let rust_file = base_name.to_string() + ".rs";
    fs::write(&rust_file, program).expect("Unable to write file");

    // Compile program (with multiple optimizations
    let mut runs: Vec<(&str, u128)> = vec![];
    let mut files: Vec<String> = vec![rust_file.clone()];

    for opt in ["0", "1", "2", "3", "s"] {
        let output_file = base_name.to_string() + "-" + opt.to_string().as_str();
        files.push(output_file.to_owned());
        compile_program(&rust_file, &output_file, opt)?;
        let checksum = run_program(&rust_file, &output_file)?;
        runs.push((opt, checksum));
    }

    // Compare outputs
    if !runs.iter().all(|output| output.1 == runs[0].1) {
        return Err(DifferingChecksumError {
            files,
            checksums: runs,
        }
        .into());
    }

    if !runs.iter().all(|output| output.1 == expected_checksum) {
        return Err(UnexpectedChecksumError {
            files,
            expected_checksum,
            checksums: runs,
        }
        .into());
    }

    Ok(files)
}

pub fn main() {
    let args: Args = Args::parse();
    let policy = Policy::default();
    let num_rums = args.num_runs.unwrap_or(u64::MAX);
    let output_path = args.output_path;
    let base_name = "prog";
    let progress_bar = ProgressBar::new(num_rums);
    progress_bar.set_style(ProgressStyle::default_bar()
        .template("{spinner:.green} [{elapsed_precise}] [{bar:50.cyan/blue}] Program {pos:>5}/{len:5} (ETA {eta})")
        .progress_chars("#>-"));

    if Path::exists(Path::new(&output_path)) {
        fs::remove_dir_all(&output_path).expect("Unable to remove directory");
    }

    for i in 0..num_rums {
        match &run(Some(i), policy.clone(), base_name) {
            Ok(files) => {
                if args.store_passing_programs {
                    fs::create_dir_all(format!("{}/pass/{}", output_path, i))
                        .expect("Unable to create directory");
                }
                for file in files {
                    if args.store_passing_programs {
                        fs::rename(file, format!("{}/pass/{}/{}", output_path, i, file))
                            .expect("Cannot move file")
                    } else {
                        fs::remove_file(file).expect("Unable to remove file");
                    }
                }
            }
            Err(err) => {
                println!("Failed seed {}", i);
                println!("{}", err);
                fs::create_dir_all(format!("{}/fail/{}/{}", &output_path, err.folder_name(), i))
                    .expect("Unable to create directory");
                match err {
                    RunnerError::Compilation(compilation_err) => {
                        fs::rename(
                            &compilation_err.rust_file_path,
                            format!(
                                "{}/fail/{}/{}/{}",
                                output_path,
                                err.folder_name(),
                                i,
                                &compilation_err.rust_file_path
                            ),
                        )
                        .expect("Cannot move file");
                    }
                    RunnerError::Run(run_err) => {
                        fs::rename(
                            &run_err.rust_file_path,
                            format!(
                                "{}/fail/{}/{}/{}",
                                output_path,
                                err.folder_name(),
                                i,
                                &run_err.rust_file_path
                            ),
                        )
                        .expect("Cannot move file");
                        fs::rename(
                            &run_err.bin_file_path,
                            format!(
                                "{}/fail/{}/{}/{}",
                                output_path,
                                err.folder_name(),
                                i,
                                &run_err.bin_file_path
                            ),
                        )
                        .expect("Cannot move file");
                    }
                    RunnerError::DifferingChecksum(DifferingChecksumError { files, .. })
                    | RunnerError::UnexpectedChecksum(UnexpectedChecksumError { files, .. }) => {
                        for file in files {
                            fs::rename(
                                file,
                                format!(
                                    "{}/fail/{}/{}/{}",
                                    output_path,
                                    err.folder_name(),
                                    i,
                                    file
                                ),
                            )
                            .expect("Cannot move file");
                        }
                    }
                }
            }
        }
        progress_bar.inc(1);
    }
}

#[derive(Debug)]
struct CompilationError {
    rust_file_path: String,
    status_code: i32,
    std_err: String,
}

impl CompilationError {
    fn new(rust_file_path: &str, output: Output) -> CompilationError {
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
struct RunError {
    rust_file_path: String,
    bin_file_path: String,
    status_code: i32,
    std_err: String,
}

impl RunError {
    fn new(rust_file_path: &str, bin_file_path: &str, output: Output) -> RunError {
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
struct DifferingChecksumError {
    files: Vec<String>,
    checksums: Vec<(&'static str, u128)>,
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
struct UnexpectedChecksumError {
    files: Vec<String>,
    expected_checksum: u128,
    checksums: Vec<(&'static str, u128)>,
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

fn compile_program(
    input_file: &str,
    output_file: &str,
    opt_level: &str,
) -> Result<(), CompilationError> {
    let output = Command::new("rustc")
        .args([
            "-A",
            "warnings",
            "-C",
            &format!("opt-level={}", opt_level),
            input_file,
            &format!("-o"),
            output_file,
        ])
        .output()
        .expect("failed to execute compile process");

    if !output.status.success() {
        return Err(CompilationError::new(input_file, output));
    }
    return Ok(());
}

fn run_program(rust_file: &str, executable: &str) -> Result<u128, RunError> {
    let output = Command::new(format!("./{}", &executable))
        .output()
        .expect("failed to execute run process");
    if !output.status.success() {
        return Err(RunError::new(rust_file, executable, output));
    }
    Ok(u128::from_str(
        String::from_utf8(output.stdout)
            .expect("Invalid stdout")
            .trim_end(),
    )
    .expect("Unexpected execution output"))
}
