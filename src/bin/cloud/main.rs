mod model;

#[macro_use]
extern crate diesel;

use crate::model::PolicyInfo;
use diesel::{Connection, EqAll, insert_into, MysqlConnection, QueryDsl, RunQueryDsl};
use diesel::result::Error;
use dotenv::dotenv;
use rust_gen::policy::Policy;
use rust_gen::runtime::config::{OptLevel, RustVersion};
use rust_gen::runtime::run::Runner;
use rust_gen::schema::policies::dsl::{policies, policy_info};
use uuid::Uuid;

pub fn establish_connection() -> MysqlConnection {
    dotenv().ok();

    let database_url = std::env::var("DATABASE_URL").expect("DATABASE_URL must be set");
    MysqlConnection::establish(&database_url)
        .expect(&format!("Error connecting to {}", database_url))
}

pub fn main() {
    let tmp_dir = std::env::temp_dir().join(format!("rust-gen-{}", Uuid::new_v4()));
    println!("{}", tmp_dir.file_name().unwrap().to_str().unwrap());
    std::fs::create_dir(tmp_dir.as_path()).expect("Unable to create directory");
    let connection = establish_connection();
    let mut runner = Runner {
        policy: Policy::default(),
        tmp_dir: tmp_dir.clone(),
        base_name: "base".to_owned(),
        opts: OptLevel::all_opt_levels(),
        versions: RustVersion::supported_rust_versions(),
        rustfmt: false,
    };
    for i in 0..100 {
        runner.policy = Policy::parse_policy_args_or_random(&None);
        let output = runner.run(Some(i));
        let files = match output {
            Ok(files) => files,
            Err(err) => err.files(),
        };

        let new_policy: PolicyInfo = runner.policy.into();
        let previous_policy = PolicyInfo::query(&new_policy, &connection);
        let new_policy_id = match previous_policy {
            Some(policy) => {
                policy.policy_id.unwrap()
            }
            None => {
                insert_into(policies).values(new_policy.clone()).execute(&connection).unwrap();
                PolicyInfo::query(&new_policy, &connection).unwrap().policy_id.unwrap()
            }
        };

        for file in files {
            std::fs::remove_file(&file).unwrap();
        }
    }
}
