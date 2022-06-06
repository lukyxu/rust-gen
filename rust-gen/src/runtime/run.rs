use crate::generator::{run_generator, GeneratorOutput};
use crate::policy::Policy;
use crate::runtime::config::{OptLevel, RustVersion};
use crate::runtime::error::{
    CompilationError, DifferingChecksumError, RunError, RunnerError, RustFmtError,
    UnexpectedChecksumError,
};
use crate::utils::write_as_ron;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::str::FromStr;

pub type RunResult = Result<Vec<PathBuf>, RunnerError>;

pub type ChecksumMapping = Vec<((OptLevel, RustVersion), u128)>;

pub struct Runner {
    pub policy: Policy,
    pub base_name: String,
    pub tmp_dir: PathBuf,
    pub no_compile: bool,
    pub opts: Vec<OptLevel>,
    pub versions: Vec<RustVersion>,
    pub rustfmt: bool,
}

impl Runner {
    pub fn run(&self, seed: Option<u64>) -> RunResult {
        // Generate program
        let GeneratorOutput {
            program,
            statistics,
            expected_checksum,
        } = run_generator(seed, &self.policy, true).map_err(RunnerError::Generator)?;
        let expected_checksum = expected_checksum.unwrap();
        let rust_file = self.tmp_dir.join(self.base_name.clone() + ".rs");

        // Save program
        fs::write(&rust_file, program).expect("Unable to write file");

        // Write statistics
        let stats_file = self.tmp_dir.join("statistics.txt");
        write_as_ron(
            fs::File::create(&stats_file).expect("Unable to create file"),
            statistics,
        );

        let mut files: Vec<PathBuf> = vec![rust_file.clone(), stats_file];

        if self.no_compile {
            return Ok(files);
        }

        // Compile program (with multiple optimizations)
        let mut runs: ChecksumMapping = vec![];

        for version in &self.versions {
            for opt in &self.opts {
                let output_file_name =
                    self.base_name.clone() + "-" + &version.to_string() + "-" + &opt.to_string();
                let output_file = self.tmp_dir.join(output_file_name);
                files.push(output_file.clone());
                compile_program(rust_file.clone(), output_file.clone(), opt, version)?;
                let checksum = run_program(rust_file.clone(), output_file.clone())?;
                runs.push(((opt.clone(), version.clone()), checksum));
            }
        }

        // Compare outputs
        if !runs.iter().all(|run| run.1 == runs[0].1) {
            return Err(DifferingChecksumError {
                files,
                checksums: runs,
            }
            .into());
        }

        // Compare checksum
        if !runs.iter().all(|output| output.1 == expected_checksum) {
            return Err(UnexpectedChecksumError {
                files,
                expected_checksum,
                checksums: runs,
            }
            .into());
        }

        // Run rustfmt
        if self.rustfmt {
            if let Err(err) = run_rustfmt(rust_file, &files) {
                return Err(err.into());
            }
        }

        Ok(files)
    }

    pub fn save_and_clean_up<P: AsRef<Path>>(
        output: &RunResult,
        i: u64,
        output_path: P,
        save_passing_programs: bool,
        include_binaries: bool,
    ) -> PathBuf {
        match &output {
            Ok(files) => {
                let directory = output_path.as_ref().join("pass").join(i.to_string());
                if save_passing_programs {
                    fs::create_dir_all(&directory).expect("Unable to create directory");
                }
                for file in files {
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
}

fn is_binary<P: AsRef<Path>>(input_file: P) -> bool {
    let extension = input_file.as_ref().extension();
    match extension {
        None => true,
        Some(extension) => extension.to_string_lossy().contains('-'),
    }
}

fn compile_program<P: AsRef<Path>, S: AsRef<Path>>(
    input_file: P,
    output_file: S,
    opt_level: &OptLevel,
    version: &RustVersion,
) -> Result<(), CompilationError> {
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
        ));
    }
    Ok(())
}

fn run_program<P: AsRef<Path>, S: AsRef<Path>>(
    rust_file: P,
    executable: S,
) -> Result<u128, RunError> {
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

fn run_rustfmt<P: AsRef<Path>>(rust_file: P, files: &Vec<PathBuf>) -> Result<(), RustFmtError> {
    let output = Command::new(format!("rustfmt"))
        .arg(rust_file.as_ref())
        .output()
        .expect("Failed to execute runtime process");
    if !output.status.success() {
        return Err(RustFmtError::new(output, files.clone()));
    }
    Ok(())
}
