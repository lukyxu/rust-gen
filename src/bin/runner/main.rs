use clap::Parser;

use std::fmt::Debug;

use error::{
    CompilationError, DifferingChecksumError, RunError, RunnerError, UnexpectedChecksumError,
};
use indicatif::{ProgressBar, ProgressStyle};
use rust_gen::generator::{run_generator, GeneratorOutput, GeneratorError};
use rust_gen::policy::Policy;
use rust_gen::utils::write_as_ron;
use std::fs;
use std::path::Path;
use std::process::Command;
use std::str::FromStr;

mod error;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    #[clap(
        short,
        long,
        help = "Number of programs to be generated, compiled and run."
    )]
    num_runs: Option<u64>,
    #[clap(short, long, help = "Generation policy [default: default]. Use the flag \"-p help\" for a list of available policies.")]
    policy: Option<String>,
    #[clap(
        short,
        long,
        help = "Output base name of generated program.",
        default_value = "base"
    )]
    base_name: String,
    #[clap(short, long, help = "Output path", default_value = "output")]
    output_path: String,
    #[clap(short, long, help = "Store passing programs in output path.")]
    save_passing_programs: bool,
    #[clap(
        long,
        help = "Option to not run differential testing with different optimizations."
    )]
    no_opt: bool,
}

pub fn main() {
    let args: Args = Args::parse();
    let policy = Policy::parse_policy_args(args.policy);
    let num_rums = args.num_runs.unwrap_or(u64::MAX);
    let output_path = args.output_path;
    let base_name = args.base_name;
    let progress_bar = ProgressBar::new(num_rums);
    progress_bar.set_style(ProgressStyle::default_bar()
        .template("{spinner:.green} [{elapsed_precise}] [{bar:50.cyan/blue}] Program {pos:>5}/{len:5} (ETA {eta})")
        .progress_chars("#>-"));

    if Path::exists(Path::new(&output_path)) {
        fs::remove_dir_all(&output_path).expect("Unable to remove directory");
    }

    for i in 0..num_rums {
        let opts = if args.no_opt {
            vec!["0"]
        } else {
            vec!["0", "1", "2", "3", "s"]
        };
        match &run(Some(i), &policy, &base_name, opts) {
            Ok(files) => {
                let directory = &format!("{}/pass/{}", output_path, i);
                let directory: &Path = Path::new(directory);
                if args.save_passing_programs {
                    fs::create_dir_all(directory)
                        .expect("Unable to create directory");
                }
                for file in files {
                    if args.save_passing_programs {
                        fs::rename(file, directory.join(file))
                            .expect("Cannot move file");
                    } else {
                        fs::remove_file(file).expect("Unable to remove file");
                    }
                }
            }
            Err(err) => {
                println!("Failed seed {}", i);
                println!("{}", err);
                let directory = &format!("{}/fail/{}/{}", &output_path, err.folder_name(), i);
                let directory: &Path = Path::new(directory);
                fs::create_dir_all(directory)
                    .expect("Unable to create directory");
                for file in err.files() {
                    fs::rename(&file, directory.join(&file)).expect("Cannot move file")
                }
            }
        }
        progress_bar.inc(1);
    }
}

type RunOutput = Result<Vec<String>, RunnerError>;

// TODO: Make policy take in a pointer
fn run(seed: Option<u64>, policy: &Policy, base_name: &str, opts: Vec<&'static str>) -> RunOutput {
    // Generate program
    let GeneratorOutput {
        program,
        statistics,
        expected_checksum,
    } = run_generator(seed, &policy).or_else(|err| Err(RunnerError::Generator(err)))?;
    // Save program
    let rust_file = base_name.to_string() + ".rs";
    fs::write(&rust_file, program).expect("Unable to write file");

    // Write statistics
    let stats_file = "statistics.txt".to_owned();
    write_as_ron(
        fs::File::create(&stats_file).expect("Unable to create file"),
        statistics,
    );

    // Compile program (with multiple optimizations)
    let mut runs: Vec<(&str, u128)> = vec![];
    let mut files: Vec<String> = vec![rust_file.clone(), stats_file];

    for opt in opts {
        let output_file = base_name.to_string() + "-" + opt.to_string().as_str();
        files.push(output_file.clone());
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
            "-o",
            output_file,
        ])
        .output()
        .expect("failed to execute compile process");

    if !output.status.success() {
        return Err(CompilationError::new(input_file, &output));
    }
    Ok(())
}

fn run_program(rust_file: &str, executable: &str) -> Result<u128, RunError> {
    let output = Command::new(format!("./{}", &executable))
        .output()
        .expect("failed to execute run process");
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
