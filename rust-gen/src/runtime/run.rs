use crate::generator::{run_generator, GeneratorOutput, GeneratorResult};
use crate::policy::Policy;
use crate::runtime::config::{OptLevel, RustCompiler, RustVersion};
use crate::runtime::error::{
    CompilationError, CompilationTimeoutError, DifferingChecksumError, GeneratorTimeoutError,
    RunError, RunTimeoutError, RunnerError, RustFmtError, RustFmtTimeoutError,
    UnexpectedChecksumError,
};
use crate::statistics::generation::FullGenerationStatistics;
use crate::statistics::program::FullProgramStatistics;
use crate::utils::write_as_ron;
use std::collections::BTreeMap;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::str::FromStr;
use std::sync::mpsc::RecvTimeoutError;
use std::sync::{mpsc, Arc};
use std::time::{Duration, Instant};
use std::{env, fs, thread};

pub type Statistics = (FullGenerationStatistics, Option<FullProgramStatistics>);

#[derive(Debug, Default, Clone)]
pub struct RunOutput {
    pub files: Vec<PathBuf>,
    pub statistics: Option<Statistics>,
    pub expected_checksum: Option<u128>,
    pub rust_file_path: Option<PathBuf>,
    pub subruns: Vec<SubRunResult>,
    pub generation_time: Option<Duration>,
    pub rustfmt_time: Option<Duration>,
}

impl RunOutput {
    pub fn from_run_result(output: &RunResult) -> &RunOutput {
        match output {
            Ok(output) => &output,
            Err(err) => err.run_output(),
        }
    }
}
pub type SubRunResult = Result<SubRunOutput, SubRunError>;

#[derive(Debug, Clone)]
#[allow(unused)]
pub struct SubRunOutput {
    pub compiler_name: String,
    pub opt: OptLevel,
    pub version: RustVersion,
    pub executable_file: Option<PathBuf>,
    pub compilation_duration: Option<Duration>,
    pub run_duration: Option<Duration>,
    pub checksum: Option<u128>,
}

impl SubRunOutput {
    pub fn from_sub_run_result(sub_run_result: &SubRunResult) -> &SubRunOutput {
        match sub_run_result {
            Ok(output) => output,
            Err(err) => err.subrun_output(),
        }
    }
}

impl Error for SubRunError {}

#[derive(Debug, Clone)]
pub enum SubRunError {
    CompilationFailure(CompilationError, SubRunOutput),
    CompilationTimeout(CompilationTimeoutError, SubRunOutput),
    RunFailure(RunError, SubRunOutput),
    RunTimeout(RunTimeoutError, SubRunOutput),
}

impl SubRunError {
    pub fn error_kind(&self) -> SubRunErrorKind {
        match self {
            SubRunError::CompilationFailure(_, _) => SubRunErrorKind::CompilationFailure,
            SubRunError::CompilationTimeout(_, _) => SubRunErrorKind::CompilationTimeout,
            SubRunError::RunFailure(_, _) => SubRunErrorKind::RunFailure,
            SubRunError::RunTimeout(_, _) => SubRunErrorKind::RunTimeout,
        }
    }
}

impl Display for SubRunError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SubRunError::CompilationFailure(err, _) => Display::fmt(err, f),
            SubRunError::CompilationTimeout(err, _) => Display::fmt(err, f),
            SubRunError::RunFailure(err, _) => Display::fmt(err, f),
            SubRunError::RunTimeout(err, _) => Display::fmt(err, f),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub enum SubRunErrorKind {
    CompilationFailure,
    CompilationTimeout,
    RunFailure,
    RunTimeout,
}

impl ToString for SubRunErrorKind {
    fn to_string(&self) -> String {
        match self {
            SubRunErrorKind::CompilationFailure => "compilation_failure",
            SubRunErrorKind::CompilationTimeout => "compilation_timeout",
            SubRunErrorKind::RunFailure => "run_failure",
            SubRunErrorKind::RunTimeout => "run_timeout",
        }
        .to_string()
    }
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
pub type RunMapping = BTreeMap<(OptLevel, RustVersion), u128>;

pub struct Runner {
    pub base_name: String,
    pub tmp_dir: PathBuf,
    pub add_assertions: bool,
    pub no_compile: bool,
    pub opts: Vec<OptLevel>,
    pub versions: Vec<RustVersion>,
    pub run_rustc: bool,
    pub run_mrustc: bool,
    pub run_gccrs: bool,
    pub rustfmt: bool,
    pub generate_timeout: Duration,
    pub compile_timeout: Duration,
    pub run_timeout: Duration,
    pub rustfmt_timeout: Duration,
}

#[derive(Debug)]
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
                    return Timed(duration, None);
                }
                Timed(time_taken, Some(res))
            }
            Err(RecvTimeoutError::Timeout) => Timed(duration, None),
            Err(RecvTimeoutError::Disconnected) => {
                panic!("Timeout function panicked")
            }
        }
    }
}

impl Runner {
    pub fn run(&self, seed: Option<u64>, policy: &Policy) -> RunResult {
        let mut run_output = RunOutput::default();
        let gen_output = timed_run_generator(
            self.generate_timeout,
            seed,
            policy,
            true,
            self.add_assertions,
        );
        run_output.generation_time = Some(gen_output.0);
        let GeneratorOutput {
            program,
            generation_statistics,
            program_statistics,
            expected_checksum,
        } = gen_output
            .1
            .ok_or(RunnerError::GeneratorTimeout(
                GeneratorTimeoutError::new(gen_output.0),
                run_output.clone(),
            ))?
            .map_err(|err| RunnerError::GeneratorFailure(err, run_output.clone()))?;

        run_output.statistics = Some((generation_statistics.clone(), program_statistics.clone()));
        run_output.expected_checksum = Some(expected_checksum.unwrap());

        let expected_checksum = expected_checksum.unwrap();
        let rust_file = self.tmp_dir.join(self.base_name.clone() + ".rs");

        // Save program
        fs::write(&rust_file, &program).expect("Unable to write file");
        run_output.rust_file_path = Some(rust_file.clone());

        // Write statistics
        let stats_file = self.tmp_dir.join("statistics.txt");
        write_as_ron(
            fs::File::create(&stats_file).expect("Unable to create file"),
            run_output.statistics.as_ref().unwrap(),
        );

        run_output.files = vec![rust_file.clone(), stats_file];

        // Run rustfmt
        if self.rustfmt {
            let rustfmt_output = timed_run_rustfmt(self.rustfmt_timeout, &rust_file);
            rustfmt_output
                .1
                .ok_or(RunnerError::RustFmtTimeout(
                    RustFmtTimeoutError::new(rustfmt_output.0),
                    run_output.clone(),
                ))?
                .map_err(|err| RunnerError::RustFmtFailure(err, run_output.clone()))?;
            run_output.rustfmt_time = Some(rustfmt_output.0);
        }

        if self.no_compile {
            return Ok(run_output);
        }

        // Compile and run program (with multiple optimizations)
        if self.run_rustc {
            for version in &self.versions {
                for opt in &self.opts {
                    run_output
                        .subruns
                        .push(self.subrun(&RustCompiler::RustC, opt, version, &rust_file));
                }
            }
        }

        if self.run_mrustc {
            run_output
                .subruns
                .push(self.subrun(&RustCompiler::MrustC, &OptLevel::no_opt(), &RustVersion::mrustc_version(), &rust_file));
            if self.opts.len() > 1 {
                run_output
                    .subruns
                    .push(self.subrun(&RustCompiler::MrustC, &OptLevel::some_opt(), &RustVersion::mrustc_version(), &rust_file));
            }
        }

        if self.run_gccrs {
            let mut program_gccrs = GCC_RS_DRIVER.to_owned();
            program_gccrs.push_str(&program.replace("println!(\"{}\", checksum)", "print_int(checksum);"));
            let gcc_rs_path = self.tmp_dir.join(self.base_name.clone() + "-gccrs" + ".rs");
            fs::write(&gcc_rs_path, &program_gccrs).expect("Unable to write file");
            run_output
                .subruns
                .push(self.subrun(&RustCompiler::GCCRS, &OptLevel::no_opt(), &RustVersion::gccrs_version(), &gcc_rs_path));
            run_output.files.push(gcc_rs_path);
        }

        // Push subrun files
        for subrun in &run_output.subruns {
            let file = SubRunOutput::from_sub_run_result(subrun)
                .executable_file
                .clone();
            if let Some(file) = file {
                run_output.files.push(file)
            }
        }

        let subrun_outputs = subrun_validation(&run_output)?;

        // Compare outputs
        if !subrun_outputs.is_empty() && !subrun_outputs
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
                    .join(&format!("{}_error", err.error_kind()))
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
        compiler: &RustCompiler,
        opt: &OptLevel,
        version: &RustVersion,
        rust_file: P,
    ) -> SubRunResult {
        let mut subrun_output = SubRunOutput {
            compiler_name: compiler.to_string(),
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
        let compilation_result =
            timed_compile_program(self.compile_timeout, &rust_file, &output_file, compiler,opt, version);
        subrun_output.compilation_duration = Some(compilation_result.0);
        subrun_output.executable_file = Some(
            compilation_result
                .1
                .ok_or(SubRunError::CompilationTimeout(
                    CompilationTimeoutError::new(
                        opt.clone(),
                        version.clone(),
                        compilation_result.0,
                    ),
                    subrun_output.clone(),
                ))?
                .map_err(|err| SubRunError::CompilationFailure(err, subrun_output.clone()))?,
        );
        let run_executable_result = timed_run_program(self.run_timeout, &rust_file, &output_file, *compiler == RustCompiler::GCCRS);
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
    compiler: &RustCompiler,
    opt_level: &OptLevel,
    version: &RustVersion,
) -> Timed<CompilationResult> {
    let input_file = Arc::new(input_file.as_ref().to_path_buf());
    let output_file = Arc::new(output_file.as_ref().to_path_buf());
    let compiler = Arc::new(compiler.clone());
    let opt_level = Arc::new(opt_level.clone());
    let version = Arc::new(version.clone());
    Timed::<CompilationResult>::run_with_timeout(
        timeout,
        Box::new(move || compile_program(&*input_file, &*output_file, &*compiler, &*opt_level, &*version)),
    )
}

fn compile_program<P: AsRef<Path>, S: AsRef<Path>>(
    input_file: P,
    output_file: S,
    compiler: &RustCompiler,
    opt_level: &OptLevel,
    version: &RustVersion,
) -> CompilationResult {
    let mut command = Command::new(compiler.to_string());
    let args: Vec<String> = match compiler {
        RustCompiler::RustC => {
            vec![
                format!("+{}", version.to_string()),
                "-A".to_owned(),
                "warnings".to_owned(),
                "-C".to_owned(),
                format!("opt-level={}", opt_level.to_string()),
                input_file.as_ref().to_str().unwrap().to_owned(),
                "-o".to_owned(),
                output_file.as_ref().to_str().unwrap().to_owned(),
            ]
        },
        RustCompiler::MrustC => {
            let mut args = vec![
                input_file.as_ref().to_str().unwrap().to_owned(),
                "-L".to_owned(),
                env::var("MRUSTC_STD_PATH").expect("MRUSTC_STD_PATH not set"),
                "-o".to_owned(),
                output_file.as_ref().to_str().unwrap().to_owned(),
            ];
            if opt_level != &OptLevel::no_opt() {
                args.push("-O".to_owned())
            }
            args
        }
        RustCompiler::GCCRS => {
            vec![
                input_file.as_ref().to_str().unwrap().to_owned(),
                "-o".to_owned(),
                output_file.as_ref().to_str().unwrap().to_owned(),
            ]
        }
    };

    let output = command.args(args).output().expect("Failed to execute compile process");

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
    running_gccrs: bool,
) -> Timed<RunExecutableResult> {
    let input_file = Arc::new(rust_file.as_ref().to_path_buf());
    let output_file = Arc::new(executable.as_ref().to_path_buf());
    Timed::<RunExecutableResult>::run_with_timeout(
        timeout,
        Box::new(move || run_program(&*input_file, &*output_file, running_gccrs)),
    )
}

fn run_program<P: AsRef<Path>, S: AsRef<Path>>(rust_file: P, executable: S, running_gccrs: bool) -> RunExecutableResult {
    let rust_file = rust_file.as_ref();
    let executable = executable.as_ref();
    let output = Command::new(executable.to_str().unwrap())
        .output()
        .expect("Failed to execute runtime process");
    if !output.status.success() && (!running_gccrs || matches!(output.status.code(), Some(1)))  {
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

type RunRustfmtResult = Result<(), RustFmtError>;

fn run_rustfmt<P: AsRef<Path>>(rust_file: P) -> RunRustfmtResult {
    let output = Command::new(format!("rustfmt"))
        .arg(rust_file.as_ref())
        .output()
        .expect("Failed to execute runtime process");
    if !output.status.success() {
        return Err(RustFmtError::new(output));
    }
    Ok(())
}

fn timed_run_rustfmt<P: AsRef<Path>>(timeout: Duration, rust_file: P) -> Timed<RunRustfmtResult> {
    let input_file = Arc::new(rust_file.as_ref().to_path_buf());
    Timed::<RunRustfmtResult>::run_with_timeout(
        timeout,
        Box::new(move || run_rustfmt(&*input_file)),
    )
}

fn subrun_validation(run_output: &RunOutput) -> Result<Vec<&SubRunOutput>, RunnerError> {
    let mut errors: BTreeMap<SubRunErrorKind, Vec<SubRunError>> = BTreeMap::new();

    run_output
        .subruns
        .iter()
        .filter_map(|subrun| subrun.as_ref().err())
        .for_each(|err| {
            errors
                .entry(err.error_kind())
                .or_insert(vec![])
                .push(err.clone());
        });

    for error in errors {
        return Err(match error.0 {
            SubRunErrorKind::CompilationFailure => RunnerError::CompilationFailure(
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
            SubRunErrorKind::RunFailure => RunnerError::RunFailure(
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

const GCC_RS_DRIVER: &'static str = r#"
macro_rules! impl_int {
    ($($ty:ident = $lang:literal),*) => {
        $(
            impl $ty {
                pub fn wrapping_add(self, rhs: Self) -> Self {
                    self + rhs
                }

                pub fn wrapping_sub(self, rhs: Self) -> Self {
                    self - rhs
                }

                pub fn wrapping_mul(self, rhs: Self) -> Self {
                    self * rhs
                }

                pub fn wrapping_div(self, rhs: Self) -> Self {
                    self / rhs
                }

                pub fn wrapping_rem(self, rhs: Self) -> Self {
                    self % rhs
                }

                pub fn wrapping_shl(self, rhs: u32) -> Self {
                    self << rhs
                }

                pub fn wrapping_shr(self, rhs: u32) -> Self {
                    self >> rhs
                }
            }
        )*
    }
}

impl_int!(
    i8 = "i8",
    i16 = "i16",
    i32 = "i32",
    i64 = "i64",
    i128 = "i128",
    isize = "isize",
    u8 = "u8",
    u16 = "u16",
    u32 = "u32",
    u64 = "u64",
    u128 = "u128",
    usize = "usize"
);

extern "C" {
    fn printf(s: *const i8, ...);
}

fn print_int(value: u128) {
    let s = "%d\n\0";
    let s_p = s as *const str;
    let c_p = s_p as *const i8;
    unsafe {
        printf(c_p, value as isize);
    }
}

"#;