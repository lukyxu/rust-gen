use clap::Parser;
use rust_gen::generator::{run_generator, GeneratorOutput};
use rust_gen::policy::Policy;
use rust_gen::utils::write_as_ron;

#[derive(Parser, Debug)]
#[clap(author, version, about, about = "Randomized rust program generator.")]
struct GeneratorArgs {
    #[clap(short, long, help = "Optional seed.")]
    seed: Option<u64>,
    #[clap(
        short,
        long,
        help = "Generation policy [default: default]. Use the flag \"-p help\" for a list of available policies"
    )]
    policy: Option<String>,
    #[clap(long, help = "Output statistics instead of program.")]
    statistics: bool,
    #[clap(long, help = "Removes checksum.")]
    no_checksum: bool,
    #[clap(long, help = "Add assertions.")]
    add_assertions: bool,
}

pub fn main() {
    let args: GeneratorArgs = GeneratorArgs::parse();
    let policy = Policy::parse_policy_args(&args.policy);
    let GeneratorOutput {
        program,
        generation_statistics: statistics,
        ..
    } = run_generator(args.seed, &policy, !args.no_checksum, args.add_assertions).unwrap();
    if args.statistics {
        write_as_ron(std::io::stdout(), statistics);
    } else {
        println!("{}", program);
    }
}
