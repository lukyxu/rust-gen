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
    let policy = if let Some(policy) = args.policy {
        if let Some(policy) = Policy::get_policy(&policy) {
            policy
        } else {
            eprintln!(
                "Invalid policy, choose from from {:?}",
                Policy::get_policies()
                    .iter()
                    .map(|p| p.name)
                    .collect::<Vec<&str>>()
            );
            std::process::exit(2)
        }
    } else {
        Policy::default()
    };
    let prog = run_generator(args.seed, &policy).program;
    println!("{}", prog)
}
