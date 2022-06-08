use clap::Parser;

use std::fmt::Debug;

use indicatif::{ProgressBar, ProgressStyle};
use rust_gen::policy::Policy;
use rust_gen::runtime::config::{OptLevel, RustVersion};
use rust_gen::runtime::run::Runner;
use rust_gen::utils::write_as_ron;
use std::fs;
use std::fs::File;
use std::path::{Path, PathBuf};
use uuid::Uuid;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    #[clap(
        short,
        long,
        help = "Number of programs to be generated, compiled and runtime."
    )]
    num_runs: Option<u64>,
    #[clap(
        short,
        long,
        help = "Generation policy [default: default]. Use the flag \"-p help\" for a list of available policies."
    )]
    policy: Option<String>,
    #[clap(
        short,
        long,
        help = "Output base name of generated program.",
        default_value = "base"
    )]
    base_name: String,
    #[clap(short, long, help = "Output path", default_value = "output")]
    output_path: PathBuf,
    #[clap(short, long, help = "Store passing programs in output path.")]
    save_passing_programs: bool,
    #[clap(short, long, help = "Include binaries from output.")]
    include_binaries: bool,
    #[clap(long, help = "Option to not compile any of the generated programs")]
    no_compile: bool,
    #[clap(
        long,
        help = "Option to not runtime differential testing with different optimizations."
    )]
    no_opt: bool,
    #[clap(
        long,
        help = "Option to not runtime differential testing with different versions."
    )]
    no_version: bool,
    #[clap(long, help = "Add assertions.")]
    add_assertions: bool,
    #[clap(long, help = "Run rustfmt on generated output.")]
    rustfmt: bool,
    #[clap(long, help = "Removes unremoved temp output files in tmp directory.")]
    clean: bool,
}

pub fn main() {
    let args: Args = Args::parse();
    if args.clean {
        clean_tmp_files();
        return;
    }

    let num_rums = args.num_runs.unwrap_or(u64::MAX);
    let output_path = args.output_path.clone();
    let base_name = args.base_name.clone();
    let progress_bar = ProgressBar::new(num_rums);
    progress_bar.set_style(ProgressStyle::default_bar()
        .template("{spinner:.green} [{elapsed_precise}] [{bar:50.cyan/blue}] Program {pos:>5}/{len:5} (ETA {eta})")
        .progress_chars("#>-"));

    if Path::exists(Path::new(&output_path)) {
        fs::remove_dir_all(&output_path).expect("Unable to remove directory");
    }
    let opts = args
        .no_opt
        .then(|| vec![OptLevel::no_opt()])
        .unwrap_or(OptLevel::all_opt_levels());
    let versions = args
        .no_version
        .then(|| vec![RustVersion::stable()])
        .unwrap_or(RustVersion::supported_rust_versions());

    let tmp_dir = std::env::temp_dir().join(format!("rust-gen-{}", Uuid::new_v4()));
    fs::create_dir(tmp_dir.as_path()).expect("Unable to create directory");
    let runner = Runner {
        tmp_dir: tmp_dir.clone(),
        add_assertions: args.add_assertions,
        no_compile: args.no_compile,
        base_name,
        opts,
        versions,
        rustfmt: args.rustfmt,
    };

    fs::create_dir_all(&output_path).expect("Unable to create directory");
    if args.policy.is_some() {
        let file = File::create(output_path.join("policy.txt")).expect("Unable to create file");
        write_as_ron(file, Policy::parse_policy_args(&args.policy));
    }

    for i in 0..num_rums {
        run(&runner, &args, i);
        progress_bar.inc(1);
    }
    fs::remove_dir_all(tmp_dir.as_path()).expect("Unable to delete directory");
}

fn run(runner: &Runner, args: &Args, seed: u64) {
    let policy = Policy::parse_policy_args_or_random(&args.policy);
    let output = runner.run(Some(seed), &policy);
    if let Err(err) = &output {
        eprintln!("Failed seed {}", seed);
        eprintln!("{}", err);
    }
    let output_path = Runner::save_and_clean_up(
        &output,
        seed,
        &args.output_path,
        args.save_passing_programs,
        args.include_binaries,
    );
    if args.policy.is_none() {
        fs::create_dir_all(&output_path).expect("Unable to create directory");
        let file =
            File::create(output_path.as_path().join("policy.txt")).expect("Unable to create file");
        write_as_ron(file, policy);
    }
}

pub fn clean_tmp_files() {
    let tmp_dir = std::env::temp_dir();
    let dir = match fs::read_dir(&tmp_dir) {
        Ok(iter) => iter,
        Err(err) => {
            eprintln!("{:?}", err.kind());
            return;
        }
    };
    for dir_entry in dir {
        if let Ok(dir_entry) = dir_entry {
            if let Some(str) = dir_entry.file_name().to_str() {
                if str.contains("rust-gen") {
                    std::fs::remove_dir_all(dir_entry.path()).expect("Unable to remove directory")
                }
            }
        }
    }
}
