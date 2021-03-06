pub mod model;
pub mod schema;

#[macro_use]
extern crate diesel;

use crate::model::statistic::StatisticsInfo;
use crate::model::statistic_map::StatisticsMapInfo;
use clap::Parser;
use diesel::{Connection, MysqlConnection};
use dotenv::dotenv;
use model::policy::PolicyInfo;
use model::run::RunInfo;
use model::sub_run::SubRunInfo;
use rand::Rng;
use rust_gen::policy::Policy;
use rust_gen::runtime::config::{OptLevel, RustVersion};
use rust_gen::runtime::run::{RunOutput, Runner};
use std::time::Duration;
use uuid::Uuid;

pub fn establish_connection() -> MysqlConnection {
    dotenv().ok();

    let database_url = std::env::var("DATABASE_URL").expect("DATABASE_URL must be set");
    MysqlConnection::establish(&database_url)
        .expect(&format!("Error connecting to {}", database_url))
}

#[derive(Parser, Debug)]
#[clap(author, version, about, about = "Randomized rust program generator.")]
struct Args {
    #[clap(
        short,
        long,
        help = "Generation policy. By default, the policy is randomly chosen. Use the flag \"-p help\" for a list of available policies"
    )]
    policy: Option<String>,
    #[clap(
        short,
        long,
        help = "Number of programs to be generated, compiled and run."
    )]
    num_runs: Option<u64>,
    #[clap(long, help = "Include mrustc subruns.")]
    include_mrustc: bool,
    #[clap(long, help = "Include gccrs subruns.")]
    include_gccrs: bool,
}

pub fn main() {
    let args: Args = Args::parse();
    let tmp_dir = std::env::temp_dir().join(format!("rust-gen-{}", Uuid::new_v4()));
    std::fs::create_dir(tmp_dir.as_path()).expect("Unable to create directory");
    let connection = establish_connection();
    let runner = Runner {
        tmp_dir: tmp_dir.clone(),
        add_assertions: false,
        no_compile: false,
        base_name: "base".to_owned(),
        opts: OptLevel::all_opt_levels(),
        // opts: vec![OptLevel::no_opt()],
        versions: RustVersion::supported_rust_versions(),
        // versions: vec![RustVersion::stable()],
        run_rustc: true,
        run_mrustc: args.include_mrustc,
        run_gccrs: args.include_gccrs,
        rustfmt: true,
        generate_timeout: Duration::from_secs(30),
        compile_timeout: Duration::from_secs(60),
        run_timeout: Duration::from_secs(1),
        rustfmt_timeout: Duration::from_secs(120),
    };
    for i in 0..args.num_runs.unwrap_or(u64::MAX) {
        let policy = Policy::parse_policy_args_or_random(&args.policy);
        let seed = rand::thread_rng().gen();
        println!("Running policy {} seed {} run {}", policy.name, seed, i);
        let output = runner.run(Some(seed), &policy);
        let files = RunOutput::from_run_result(&output).files.clone();
        let program = RunOutput::from_run_result(&output)
            .rust_file_path
            .as_ref()
            .map(|path| {
                String::from_utf8(std::fs::read(path).expect("Unable to read rust file"))
                    .expect("Unable to read utf-8")
            });

        let new_policy: PolicyInfo = policy.into();
        let previous_policy = PolicyInfo::query(&new_policy, &connection);
        let new_policy_id = match previous_policy {
            Some(policy) => policy.policy_id.unwrap(),
            None => {
                new_policy.insert_new(&connection);
                new_policy.query(&connection).unwrap().policy_id.unwrap()
            }
        };

        let sub_runs = RunOutput::from_run_result(&output).subruns.clone();
        let stats = RunOutput::from_run_result(&output).statistics.clone();
        let run_id =
            RunInfo::new(seed, output, new_policy_id, &stats, &runner).insert_new(&connection);
        for sub_run in sub_runs {
            SubRunInfo::new(run_id, sub_run).insert_new(&connection)
        }

        if let Some(stats) = stats {
            let (gen_stats, prog_stats) = &stats;
            let successful_map: StatisticsMapInfo = gen_stats.successful_mapping.clone().into();
            let failed_map: StatisticsMapInfo = gen_stats.failed_mapping.clone().into();
            let prog_map: StatisticsMapInfo = prog_stats.as_ref().unwrap().mapping.clone().into();
            let successful_map_id = successful_map.insert_new(&connection);
            let failed_map_id = failed_map.insert_new(&connection);
            let prog_map_id = prog_map.insert_new(&connection);
            StatisticsInfo::new(
                run_id,
                successful_map_id,
                failed_map_id,
                prog_map_id,
                &stats,
                program.unwrap(),
            )
            .insert_new(&connection);
        }

        for file in files {
            std::fs::remove_file(&file).unwrap();
        }
    }
}
