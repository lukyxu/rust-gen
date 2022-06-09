use crate::generator::{run_generator, GeneratorOutput, GeneratorResult};
use crate::policy::Policy;
use crate::runtime::config::{OptLevel, RustVersion};
use crate::runtime::error::{
    CompilationError, CompilationTimeoutError, DifferingChecksumError, GeneratorTimeoutError,
    RunError, RunTimeoutError, RunnerError, RustFmtError, UnexpectedChecksumError,
};
use crate::statistics::FullStatistics;
use crate::utils::write_as_ron;
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::str::FromStr;
use std::sync::{mpsc, Arc};
use std::time::{Duration, Instant};
use std::{fs, thread};

#[derive(Debug, Default, Clone)]
pub struct RunOutput {
    pub files: Vec<PathBuf>,
    pub statistics: Option<FullStatistics>,
    pub expected_checksum: Option<u128>,
    pub rust_file_path: Option<PathBuf>,
    pub subruns: Vec<SubRunResult>,
    pub generation_time: Duration,
}

pub type SubRunResult = Result<SubRunOutput, SubRunError>;

#[derive(Debug, Clone)]
#[allow(unused)]
pub struct SubRunOutput {
    compiler_name: String,
    opt: OptLevel,
    version: RustVersion,
    executable_file: Option<PathBuf>,
    compilation_duration: Option<Duration>,
    run_duration: Option<Duration>,
    checksum: Option<u128>,
}

#[derive(Debug, Clone)]
pub enum SubRunError {
    CompilationFailure(CompilationError, SubRunOutput),
    CompilationTimeout(CompilationTimeoutError, SubRunOutput),
    RunFailure(RunError, SubRunOutput),
    RunTimeout(RunTimeoutError, SubRunOutput),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub enum SubRunErrorKind {
    CompilationFailure,
    CompilationTimeout,
    RunFailure,
    RunTimeout,
}

impl SubRunError {
    pub fn subrun_output(&self) -> &SubRunOutput {
        match self {
            SubRunError::CompilationFailure(_, res)
            | SubRunError::CompilationTimeout(_, res)
            | SubRunError::RunFailure(_, res)
            | SubRunError::RunTimeout(_, res) => res,
        }
    }
}

pub type RunResult = Result<RunOutput, RunnerError>;

// pub type RunErrorMapping = BTreeMap<(OptLevel, RustVersion), Option<RunError>>;
// pub type CompilationErrorMapping = BTreeMap<(OptLevel, RustVersion), Option<CompilationError>>;

pub type RunMapping = BTreeMap<(OptLevel, RustVersion), u128>;

pub struct Runner {
    pub base_name: String,
    pub tmp_dir: PathBuf,
    pub add_assertions: bool,
    pub no_compile: bool,
    pub opts: Vec<OptLevel>,
    pub versions: Vec<RustVersion>,
    pub rustfmt: bool,
    pub generate_timeout: Duration,
}

pub struct Timed<T>(Duration, Option<T>);

impl<T> Timed<T> {
    fn run_with_timeout<F>(duration: Duration, f: F) -> Timed<T>
    where
        T: Send + 'static,
        F: FnOnce() -> T,
        F: Send + 'static,
    {
        let now = Instant::now();
        let (sender, receiver) = mpsc::channel();
        thread::spawn(move || sender.send(f()));
        match receiver.recv_timeout(duration) {
            Ok(res) => {
                let time_taken = Instant::now() - now;
                if time_taken >= duration {
                    return Timed(duration, None)
                }
                Timed(time_taken, Some(res))
            },
            Err(_) => Timed(duration, None),
        }
    }
}

impl Runner {
    pub fn run(&self, seed: Option<u64>, policy: &Policy) -> RunResult {
        let mut run_output = RunOutput::default();
        let gen_output = timed_run_generator(
            Duration::from_millis(200),
            seed,
            policy,
            true,
            self.add_assertions,
        );
        run_output.generation_time = gen_output.0;
        let GeneratorOutput {
            program,
            statistics,
            expected_checksum,
        } = gen_output
            .1
            .ok_or(RunnerError::GeneratorTimeout(
                GeneratorTimeoutError::new(gen_output.0),
                run_output.clone(),
            ))?
            .map_err(|err| RunnerError::Generator(err, run_output.clone()))?;

        run_output.statistics = Some(statistics.clone());
        run_output.expected_checksum = Some(expected_checksum.unwrap());

        let expected_checksum = expected_checksum.unwrap();
        let rust_file = self.tmp_dir.join(self.base_name.clone() + ".rs");

        // Save program
        fs::write(&rust_file, program).expect("Unable to write file");
        run_output.rust_file_path = Some(rust_file.clone());

        // Write statistics
        let stats_file = self.tmp_dir.join("statistics.txt");
        write_as_ron(
            fs::File::create(&stats_file).expect("Unable to create file"),
            statistics,
        );

        run_output.files = vec![rust_file.clone(), stats_file];

        if self.no_compile {
            return Ok(run_output);
        }

        // Compile and run program (with multiple optimizations)
        for version in &self.versions {
            for opt in &self.opts {
                run_output
                    .subruns
                    .push(self.subrun(opt, version, &rust_file));
            }
        }

        // Push subrun files
        for subrun in &run_output.subruns {
            let file = match subrun {
                Ok(subrun_output) => subrun_output,
                Err(err) => err.subrun_output(),
            }
            .executable_file
            .clone();
            if let Some(file) = file {
                run_output.files.push(file)
            }
        }

        let subrun_outputs = subrun_validation(&run_output)?;

        // Compare outputs
        if !subrun_outputs
            .iter()
            .all(|run| run.checksum == subrun_outputs[0].checksum)
        {
            return Err(RunnerError::DifferingChecksum(
                DifferingChecksumError {
                    checksums: subrun_outputs.into_iter().map(|x| x.clone()).collect(),
                },
                run_output,
            ));
        }

        // Compare checksum
        if !subrun_outputs
            .iter()
            .all(|run| run.checksum == run_output.expected_checksum)
        {
            return Err(RunnerError::UnexpectedChecksum(
                UnexpectedChecksumError {
                    expected_checksum,
                    checksums: subrun_outputs.into_iter().map(|x| x.clone()).collect(),
                },
                run_output,
            ));
        }

        // Run rustfmt
        if self.rustfmt {
            run_rustfmt(rust_file, &run_output)
                .map_err(|err| RunnerError::RustFmt(err, run_output.clone()))?
        }

        Ok(run_output)
    }

    pub fn save_and_clean_up<P: AsRef<Path>>(
        output: &RunResult,
        i: u64,
        output_path: P,
        save_passing_programs: bool,
        include_binaries: bool,
    ) -> PathBuf {
        match &output {
            Ok(run_output) => {
                let directory = output_path.as_ref().join("pass").join(i.to_string());
                if save_passing_programs {
                    fs::create_dir_all(&directory).expect("Unable to create directory");
                }
                for file in &run_output.files {
                    let file_name = &Path::new(file.file_name().unwrap()).to_path_buf();
                    if save_passing_programs && (include_binaries || !is_binary(file_name)) {
                        fs::rename(file, directory.join(file_name)).expect("Cannot move file");
                    } else {
                        fs::remove_file(file).expect("Unable to remove file");
                    }
                }
                directory
            }
            Err(err) => {
                let directory = output_path
                    .as_ref()
                    .join("fail")
                    .join(err.folder_name())
                    .join(i.to_string());
                fs::create_dir_all(&directory).expect("Unable to create directory");
                for file in &err.files() {
                    let file_name = &Path::new(file.file_name().unwrap()).to_path_buf();
                    if include_binaries || !is_binary(file_name) {
                        fs::rename(file, directory.join(file_name)).expect("Cannot move file");
                    }
                }
                directory
            }
        }
    }

    fn subrun<P: AsRef<Path>>(
        &self,
        opt: &OptLevel,
        version: &RustVersion,
        rust_file: P,
    ) -> SubRunResult {
        let mut subrun_output = SubRunOutput {
            compiler_name: "rustc".to_string(),
            opt: opt.clone(),
            version: version.clone(),
            executable_file: None,
            compilation_duration: None,
            run_duration: None,
            checksum: None,
        };
        let output_file_name =
            self.base_name.clone() + "-" + &version.to_string() + "-" + &opt.to_string();
        let output_file = self.tmp_dir.join(output_file_name);
        let compilation_result = timed_compile_program(
            Duration::from_secs(1),
            &rust_file,
            &output_file,
            opt,
            version,
        );
        subrun_output.compilation_duration = Some(compilation_result.0);
        subrun_output.executable_file = Some(
            compilation_result
                .1
                .ok_or(SubRunError::CompilationTimeout(
                    CompilationTimeoutError::new(opt.clone(), version.clone(), compilation_result.0),
                    subrun_output.clone(),
                ))?
                .map_err(|err| SubRunError::CompilationFailure(err, subrun_output.clone()))?,
        );
        let run_executable_result =
            timed_run_program(Duration::from_secs(1), &rust_file, &output_file);
        subrun_output.run_duration = Some(run_executable_result.0);
        subrun_output.checksum = Some(
            run_executable_result
                .1
                .ok_or(SubRunError::RunTimeout(
                    RunTimeoutError::new(opt.clone(), version.clone(), run_executable_result.0),
                    subrun_output.clone(),
                ))?
                .map_err(|err| SubRunError::RunFailure(err, subrun_output.clone()))?,
        );
        Ok(subrun_output)
    }
}

fn timed_run_generator(
    timeout: Duration,
    seed: Option<u64>,
    policy: &Policy,
    add_checksum: bool,
    add_assertions: bool,
) -> Timed<GeneratorResult> {
    let arc_policy = Arc::new(policy.clone());
    Timed::<GeneratorResult>::run_with_timeout(
        timeout,
        Box::new(move || run_generator(seed, &arc_policy, add_checksum, add_assertions)),
    )
}

fn is_binary<P: AsRef<Path>>(input_file: P) -> bool {
    let extension = input_file.as_ref().extension();
    match extension {
        None => true,
        Some(extension) => extension.to_string_lossy().contains('-'),
    }
}

type CompilationResult = Result<PathBuf, CompilationError>;

fn timed_compile_program<P: AsRef<Path>, S: AsRef<Path>>(
    timeout: Duration,
    input_file: P,
    output_file: S,
    opt_level: &OptLevel,
    version: &RustVersion,
) -> Timed<CompilationResult> {
    let input_file = Arc::new(input_file.as_ref().to_path_buf());
    let output_file = Arc::new(output_file.as_ref().to_path_buf());
    let opt_level = Arc::new(opt_level.clone());
    let version = Arc::new(version.clone());
    Timed::<CompilationResult>::run_with_timeout(
        timeout,
        Box::new(move || compile_program(&*input_file, &*output_file, &*opt_level, &*version)),
    )
}

fn compile_program<P: AsRef<Path>, S: AsRef<Path>>(
    input_file: P,
    output_file: S,
    opt_level: &OptLevel,
    version: &RustVersion,
) -> CompilationResult {
    let output = Command::new("rustc")
        .args([
            &format!("+{}", version.to_string()),
            "-A",
            "warnings",
            "-C",
            &format!("opt-level={}", opt_level.to_string()),
            input_file.as_ref().to_str().unwrap(),
            "-o",
            output_file.as_ref().to_str().unwrap(),
        ])
        .output()
        .expect("Failed to execute compile process");

    if !output.status.success() {
        return Err(CompilationError::new(
            input_file.as_ref().to_path_buf(),
            &output,
            opt_level.clone(),
            version.clone(),
        ));
    }
    Ok(output_file.as_ref().to_path_buf())
}

type RunExecutableResult = Result<u128, RunError>;

fn timed_run_program<P: AsRef<Path>, S: AsRef<Path>>(
    timeout: Duration,
    rust_file: P,
    executable: S,
) -> Timed<RunExecutableResult> {
    let input_file = Arc::new(rust_file.as_ref().to_path_buf());
    let output_file = Arc::new(executable.as_ref().to_path_buf());
    Timed::<RunExecutableResult>::run_with_timeout(
        timeout,
        Box::new(move || run_program(&*input_file, &*output_file)),
    )
}

fn run_program<P: AsRef<Path>, S: AsRef<Path>>(rust_file: P, executable: S) -> RunExecutableResult {
    let rust_file = rust_file.as_ref();
    let executable = executable.as_ref();
    let output = Command::new(executable.to_str().unwrap())
        .output()
        .expect("Failed to execute runtime process");
    if !output.status.success() {
        return Err(RunError::new(
            rust_file.to_path_buf(),
            executable.to_path_buf(),
            &output,
        ));
    }
    Ok(u128::from_str(
        String::from_utf8(output.stdout)
            .expect("Invalid stdout")
            .trim_end(),
    )
    .expect("Unexpected execution output"))
}

fn run_rustfmt<P: AsRef<Path>>(rust_file: P, run_output: &RunOutput) -> Result<(), RustFmtError> {
    let output = Command::new(format!("rustfmt"))
        .arg(rust_file.as_ref())
        .output()
        .expect("Failed to execute runtime process");
    if !output.status.success() {
        return Err(RustFmtError::new(output, run_output.clone()));
    }
    Ok(())
}

fn subrun_validation(run_output: &RunOutput) -> Result<Vec<&SubRunOutput>, RunnerError> {
    let mut errors: BTreeMap<SubRunErrorKind, Vec<SubRunError>> = BTreeMap::new();

    run_output
        .subruns
        .iter()
        .filter_map(|subrun| subrun.as_ref().err())
        .for_each(|err| {
            let error_kind = match &err {
                SubRunError::CompilationFailure(_, _) => SubRunErrorKind::CompilationFailure,
                SubRunError::CompilationTimeout(_, _) => SubRunErrorKind::CompilationTimeout,
                SubRunError::RunFailure(_, _) => SubRunErrorKind::RunFailure,
                SubRunError::RunTimeout(_, _) => SubRunErrorKind::RunFailure,
            };
            errors.entry(error_kind).or_insert(vec![]).push(err.clone());
        });

    for error in errors {
        return Err(match error.0 {
            SubRunErrorKind::CompilationFailure => RunnerError::Compilation(
                error
                    .1
                    .into_iter()
                    .map(|err| {
                        if let SubRunError::CompilationFailure(err, _) = err {
                            err
                        } else {
                            panic!()
                        }
                    })
                    .collect(),
                run_output.clone(),
            ),
            SubRunErrorKind::CompilationTimeout => RunnerError::CompilationTimeout(
                error
                    .1
                    .into_iter()
                    .map(|err| {
                        if let SubRunError::CompilationTimeout(err, _) = err {
                            err
                        } else {
                            panic!()
                        }
                    })
                    .collect(),
                run_output.clone(),
            ),
            SubRunErrorKind::RunFailure => RunnerError::Run(
                error
                    .1
                    .into_iter()
                    .map(|err| {
                        if let SubRunError::RunFailure(err, _) = err {
                            err
                        } else {
                            panic!()
                        }
                    })
                    .collect(),
                run_output.clone(),
            ),
            SubRunErrorKind::RunTimeout => RunnerError::RunTimeout(
                error
                    .1
                    .into_iter()
                    .map(|err| {
                        if let SubRunError::RunTimeout(err, _) = err {
                            err
                        } else {
                            panic!()
                        }
                    })
                    .collect(),
                run_output.clone(),
            ),
        });
    }
    return Ok(run_output
        .subruns
        .iter()
        .map(|subrun| subrun.as_ref().ok().unwrap())
        .collect());
}
