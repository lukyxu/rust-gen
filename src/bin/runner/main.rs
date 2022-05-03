use clap::Parser;

use rust_gen::generator::{run_generator, GeneratorOutput};
use rust_gen::policy::Policy;
use std::fs;
use std::process::{Command, Output};
use std::str::FromStr;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    #[clap(short, long)]
    num_runs: Option<u64>,
    // #[clap(short, long)]
    // policy: Option<String>,
    // output_path: String,
}

pub fn main() {
    let args: Args = Args::parse();
    let policy = Policy::simple_debug();
    let num_rums = args.num_runs.unwrap_or(u64::MAX);
    let base_name = "prog";

    for i in 0..num_rums {
        if i % 50 == 0 {
            println!("{i}");
        }
        let mut fail = false;
        // Generate program
        let GeneratorOutput {
            program,
            expected_checksum,
        } = run_generator(Some(i), policy.clone());

        // Save program
        let rs_file = base_name.to_string() + ".rs";
        fs::write(&rs_file, program).expect("Unable to write file");

        // Compile program (with multiple optimizations
        let mut output: Vec<u128> = vec![];

        for opt in ["0", "1", "2", "3", "s"] {
            let output_file = base_name.to_string() + "-" + opt.to_string().as_str();
            if let Err(error) = compile_program(&rs_file, &output_file, opt) {
                error.print_compile_error();
                fail = true;
                continue;
            }
            let checksum = match run_program(&output_file) {
                Ok(checksum) => checksum,
                Err(error) => {
                    error.print_run_error();
                    fail = true;
                    continue;
                }
            };

            output.push(checksum)
        }
        // let expected_checksum = output[0];
        // Compare outputs
        if !fail {
            fail = !output.iter().all(|output| *output == expected_checksum);
        }

        // println!("{:?}", output);
        // println!("{}", expected_checksum);
        if fail {
            // Store programs on failure
            println!("Failed seed {}", i);
        } else {
            // Delete programs on pass
            fs::remove_file(&rs_file).expect("Unable to remove file");
            // println!("Passed seed {}", i);
        }
    }
}

struct CompilationError {
    input_file: String,
    status_code: i32,
    std_err: String,
}

impl CompilationError {
    fn new(input: &str, output: Output) -> CompilationError {
        return CompilationError {
            input_file: input.to_owned(),
            status_code: output.status.code().unwrap_or(-1),
            std_err: String::from_utf8_lossy(output.stderr.as_ref())
                .parse()
                .unwrap(),
        };
    }

    fn print_compile_error(&self) {
        println!("Failed to compile {}", self.input_file);
        self.print_error();
    }

    fn print_run_error(&self) {
        println!("Failed to run {}", self.input_file);
        self.print_error();
    }

    fn print_error(&self) {
        println!("Status code {}", self.status_code);
        println!("Standard error");
        println!("{}", self.std_err);
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

fn run_program(executable: &str) -> Result<u128, CompilationError> {
    let output = Command::new(format!("./{}", &executable))
        .output()
        .expect("failed to execute run process");
    if !output.status.success() {
        return Err(CompilationError::new(executable, output));
    }
    Ok(u128::from_str(
        String::from_utf8(output.stdout)
            .expect("Invalid stdout")
            .trim_end(),
    )
    .expect("Unexpected execution output"))
}
