extern crate core;

use clap::Parser;
use rust_gen::generator::run_generator;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    #[clap(short, long)]
    seed: Option<u64>,
}

pub fn main() {
    let args: Args = Args::parse();
    // TODO: Parse configuation files as generation policies
    run_generator(args.seed);
}
