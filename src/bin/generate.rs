extern crate core;

use clap::Parser;
use rust_gen::generator::run_generator;
use rust_gen::policy::Policy;

#[derive(Parser, Debug)]
#[clap(author, version, about, about = "Randomized rust program generator")]
struct GeneratorArgs {
    #[clap(short, long, help = "Optional seed")]
    seed: Option<u64>,
    #[clap(short, long, help = "Generation policy [default: default]")]
    policy: Option<String>,
}

pub fn main() {
    let args: GeneratorArgs = GeneratorArgs::parse();
    let policy = Policy::parse_policy_args(args.policy);
    let program = run_generator(args.seed, &policy).program;
    println!("{}", program)
}
