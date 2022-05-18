use clap::Parser;

use std::fmt::Debug;

use indicatif::{ProgressBar, ProgressStyle};
use rand::prelude::SliceRandom;
use rust_gen::policy::Policy;
use rust_gen::runtime::config::{OptLevel, RustVersion};
use rust_gen::runtime::run::Runner;
use std::fs;
use std::fs::File;
use std::path::Path;
use rust_gen::utils::write_as_ron;

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
    #[clap(short, long, help = "Output base name of generated program.", default_value = "base")]
    base_name: String,
    #[clap(short, long, help = "Output path", default_value = "output")]
    output_path: String,
    #[clap(short, long, help = "Store passing programs in output path.")]
    save_passing_programs: bool,
    #[clap(short, long, help = "Include binaries from output.")]
    include_binaries: bool,
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
}

pub fn main() {
    let args: Args = Args::parse();
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
    let opts = if args.no_opt {
        vec![OptLevel::no_opt()]
    } else {
        OptLevel::all_opt_levels()
    };
    let versions = if args.no_version {
        vec![RustVersion::stable()]
    } else {
        RustVersion::supported_rust_versions()
    };

    let mut runner = Runner {
        policy: Policy::default(),
        base_name,
        opts,
        versions,
    };

    for i in 0..num_rums {
        runner.policy = get_policy(&args.policy);
        let output = runner.run(Some(i));
        if let Err(err) = &output {
            eprintln!("Failed seed {}", i);
            eprintln!("{}", err);
        }
        let output_path = Runner::save_and_clean_up(
            &output,
            i,
            &output_path,
            args.save_passing_programs,
            args.include_binaries,
        );
        if args.policy.is_none() {
            let file = File::create(output_path.as_path().join("policy.txt")).expect("Unable to create file");
            write_as_ron(file, &runner.policy)
        }
        progress_bar.inc(1);
    }
}

fn get_policy(policy: &Option<String>) -> Policy {
    match policy {
        None => Policy::get_policies()
            .choose(&mut rand::thread_rng())
            .cloned()
            .unwrap(),
        Some(policy) => Policy::parse_policy_args(Some(policy.clone())),
    }
}