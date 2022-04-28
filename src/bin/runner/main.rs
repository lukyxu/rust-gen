use clap::Parser;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    #[clap(short, long)]
    num_runs: Option<u64>,
    #[clap(short, long)]
    policy: String,
}

pub fn main() {}
