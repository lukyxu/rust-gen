pub mod model;
pub mod schema;

#[macro_use]
extern crate diesel;

use crate::model::statistic::StatisticsInfo;
use crate::model::statistic_map::StatisticsMapInfo;
use diesel::{Connection, Insertable, MysqlConnection};
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

pub fn main() {
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
        rustfmt: true,
        generate_timeout: Duration::from_secs(30),
        compile_timeout: Duration::from_secs(60),
        run_timeout: Duration::from_secs(1),
    };
    for i in 0..1000000 {
        let policy = Policy::parse_policy_args_or_random(&None);
        let seed = rand::thread_rng().gen();
        println!("Running policy {} seed {} run {}", policy.name, seed, i);
        let output = runner.run(Some(seed), &policy);
        let files = RunOutput::from_run_result(&output).files.clone();

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
        let run_stats = RunOutput::from_run_result(&output).statistics.clone();
        let run_id = RunInfo::new(seed, output, new_policy_id, &runner).insert_new(&connection);
        for sub_run in sub_runs {
            SubRunInfo::new(run_id, sub_run).insert_new(&connection)
        }

        if let Some(run_stats) = run_stats {
            let (gen_stats, prog_stats) = &run_stats;
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
                &run_stats,
            ).insert_new(&connection);
        }

        for file in files {
            std::fs::remove_file(&file).unwrap();
        }
    }
}
