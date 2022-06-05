pub mod model;
pub mod schema;

#[macro_use]
extern crate diesel;

use crate::model::{PolicyInfo, RunInfo};
use diesel::{Connection, MysqlConnection};
use dotenv::dotenv;
use rand::Rng;
use rust_gen::policy::Policy;
use rust_gen::runtime::config::{OptLevel, RustVersion};
use rust_gen::runtime::run::Runner;
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
    let mut runner = Runner {
        policy: Policy::default(),
        tmp_dir: tmp_dir.clone(),
        base_name: "base".to_owned(),
        opts: vec![OptLevel::no_opt()],
        versions: vec![RustVersion::stable()],
        rustfmt: false,
    };
    for i in 0..100000 {
        runner.policy = Policy::parse_policy_args_or_random(&None);
        let seed = rand::thread_rng().gen();
        println!("Running policy {} seed {} run {}", runner.policy.name, seed, i);
        let output = runner.run(Some(seed));
        let files = match &output {
            Ok(files) => files.clone(),
            Err(err) => err.files(),
        };

        let new_policy: PolicyInfo = runner.policy.into();
        let previous_policy = PolicyInfo::query(&new_policy, &connection);
        let new_policy_id = match previous_policy {
            Some(policy) => policy.policy_id.unwrap(),
            None => {
                new_policy.insert_new(&connection);
                new_policy.query(&connection).unwrap().policy_id.unwrap()
            }
        };

        RunInfo::new(seed, output, new_policy_id).insert_new(&connection);

        for file in files {
            std::fs::remove_file(&file).unwrap();
        }
    }
}
