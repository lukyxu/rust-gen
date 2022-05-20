use crate::generator::{run_generator, GeneratorOutput};
use crate::policy::Policy;
use crate::runtime::config::{OptLevel, RustVersion};
use crate::runtime::error::{
    CompilationError, DifferingChecksumError, RunError, RunnerError, UnexpectedChecksumError,
};
use crate::utils::write_as_ron;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::str::FromStr;

type RunOutput = Result<Vec<String>, RunnerError>;

pub type ChecksumMapping = Vec<((OptLevel, RustVersion), u128)>;

pub struct Runner {
    pub policy: Policy,
    pub base_name: String,
    pub opts: Vec<OptLevel>,
    pub versions: Vec<RustVersion>,
}

impl Runner {
    pub fn run(&self, seed: Option<u64>) -> RunOutput {
        // Generate program
        let GeneratorOutput {
            program,
            statistics,
            expected_checksum,
        } = run_generator(seed, &self.policy, true).map_err(RunnerError::Generator)?;
        let expected_checksum = expected_checksum.unwrap();
        // Save program
        let rust_file = self.base_name.clone() + ".rs";
        fs::write(&rust_file, program).expect("Unable to write file");

        // Write statistics
        let stats_file = "statistics.txt".to_owned();
        write_as_ron(
            fs::File::create(&stats_file).expect("Unable to create file"),
            statistics,
        );

        // Compile program (with multiple optimizations)
        let mut runs: ChecksumMapping = vec![];
        let mut files: Vec<String> = vec![rust_file.clone(), stats_file];

        for version in &self.versions {
            for opt in &self.opts {
                let output_file =
                    self.base_name.clone() + "-" + &version.to_string() + "-" + &opt.to_string();
                files.push(output_file.clone());
                compile_program(&rust_file, &output_file, &opt, &version)?;
                let checksum = run_program(&rust_file, &output_file)?;
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

        Ok(files)
    }

    pub fn save_and_clean_up(
        output: &RunOutput,
        i: u64,
        output_path: &String,
        save_passing_programs: bool,
        include_binaries: bool,
    ) -> PathBuf {
        match &output {
            Ok(files) => {
                let directory = &format!("{}/pass/{}", output_path, i);
                let directory: &Path = Path::new(directory);
                if save_passing_programs {
                    fs::create_dir_all(directory).expect("Unable to create directory");
                }
                for file in files {
                    if save_passing_programs && (include_binaries || !is_binary(file)) {
                        fs::rename(file, directory.join(file)).expect("Cannot move file");
                    } else {
                        fs::remove_file(file).expect("Unable to remove file");
                    }
                }
                directory.to_path_buf()
            }
            Err(err) => {
                let directory = &format!("{}/fail/{}/{}", &output_path, err.folder_name(), i);
                let directory: &Path = Path::new(directory);
                fs::create_dir_all(directory).expect("Unable to create directory");
                for file in &err.files() {
                    if include_binaries || !is_binary(file) {
                        fs::rename(file, directory.join(file)).expect("Cannot move file");
                    }
                }
                directory.to_path_buf()
            }
        }
    }
}

fn is_binary(input_file: &String) -> bool {
    let extension = Path::new(input_file).extension();
    match extension {
        None => true,
        Some(extension) => extension.to_string_lossy().contains("-"),
    }
}

fn compile_program(
    input_file: &str,
    output_file: &str,
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
            input_file,
            "-o",
            output_file,
        ])
        .output()
        .expect("Failed to execute compile process");

    if !output.status.success() {
        return Err(CompilationError::new(input_file, &output));
    }
    Ok(())
}

fn run_program(rust_file: &str, executable: &str) -> Result<u128, RunError> {
    let output = Command::new(format!("./{}", &executable))
        .output()
        .expect("Failed to execute runtime process");
    if !output.status.success() {
        return Err(RunError::new(rust_file, executable, &output));
    }
    Ok(u128::from_str(
        String::from_utf8(output.stdout)
            .expect("Invalid stdout")
            .trim_end(),
    )
    .expect("Unexpected execution output"))
}
