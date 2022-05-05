extern crate core;

use clap::Parser;
use rust_gen::generator::run_generator;
use rust_gen::policy::Policy;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct GeneratorArgs {
    #[clap(short, long)]
    seed: Option<u64>,
    #[clap(short, long)]
    policy: Option<String>,
}

pub fn main() {
    let args: GeneratorArgs = GeneratorArgs::parse();
    let policy = Policy::parse_policy_args(args.policy);
    let program = run_generator(args.seed, &policy).program;
    println!("{}", program)
}
