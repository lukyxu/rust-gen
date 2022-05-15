extern crate core;

use clap::Parser;
use rust_gen::generator::{GeneratorOutput, run_generator};
use rust_gen::policy::Policy;
use rust_gen::utils::write_as_ron;

#[derive(Parser, Debug)]
#[clap(author, version, about, about = "Randomized rust program generator")]
struct GeneratorArgs {
    #[clap(short, long, help = "Optional seed")]
    seed: Option<u64>,
    #[clap(short, long, help = "Generation policy [default: default]")]
    policy: Option<String>,
    #[clap(long, help = "Output statistics instead of program")]
    statistics: bool
}

pub fn main() {
    let args: GeneratorArgs = GeneratorArgs::parse();
    let policy = Policy::parse_policy_args(args.policy);
    let GeneratorOutput { program, statistics, expected_checksum } = run_generator(args.seed, &policy);
    if args.statistics {
        write_as_ron(std::io::stdout(), statistics);
    } else {
        println!("{}", program);
    }
}
