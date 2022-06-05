use crate::schema::policies;
use crate::schema::policies::dsl::policies as policy_table;
use crate::schema::policies::policy_info;
use crate::schema::runs;
use crate::schema::runs::dsl::runs as run_table;
use diesel::result::Error;
use diesel::{insert_into, EqAll, MysqlConnection, QueryDsl, RunQueryDsl};
use rust_gen::policy::Policy;
use rust_gen::runtime::error::RunnerError;
use rust_gen::runtime::run::RunResult;
use rust_gen::utils::to_ron_string;

#[derive(Insertable, Queryable)]
#[diesel(primary_key(run_id))]
#[table_name = "runs"]
pub struct RunInfo {
    #[diesel(deserialize_as = "i32")]
    pub run_id: Option<i32>,
    pub git_hash: String,
    pub version: String,
    pub hostname: String,
    pub seed: u64,
    pub success: bool,
    pub policy_id: i32,
    pub statistics: Option<String>,
    pub error: Option<String>,
}

impl RunInfo {
    pub fn new(seed: u64, run_output: RunResult, policy_id: i32) -> RunInfo {
        let files = match &run_output {
            Ok(files) => files.clone(),
            Err(error) => error.files(),
        };
        RunInfo {
            run_id: None,
            git_hash: env!("GIT_HASH").to_string(),
            version: env!("CARGO_PKG_VERSION").to_string(),
            hostname: env!("HOSTNAME").to_string(),
            seed,
            success: run_output.is_ok(),
            policy_id,
            statistics: files
                .iter()
                .filter_map(|path| {
                    let path = path.to_string_lossy().to_string();
                    path.contains("statistics.txt").then(|| {
                        String::from_utf8(
                            std::fs::read(path).expect("Unable to read statistics.txt"),
                        )
                        .expect("Unable to read utf-8")
                    })
                })
                .next(),
            error: run_output.err().as_ref().map(RunnerError::to_string),
        }
    }

    pub fn insert_new(&self, connection: &MysqlConnection) {
        insert_into(run_table)
            .values(self.clone())
            .execute(connection)
            .unwrap();
    }
}

#[derive(Insertable, Queryable, Clone)]
#[diesel(primary_key(policy_id))]
#[table_name = "policies"]
pub struct PolicyInfo {
    #[diesel(deserialize_as = "i32")]
    pub policy_id: Option<i32>,
    pub policy_name: String,
    pub policy_info: String,
}

impl PolicyInfo {
    pub fn insert_new(&self, connection: &MysqlConnection) {
        insert_into(policy_table)
            .values(self.clone())
            .execute(connection)
            .unwrap();
    }

    pub fn query(&self, connection: &MysqlConnection) -> Option<PolicyInfo> {
        let res = policy_table
            .filter(policy_info.eq_all(self.policy_info.clone()))
            .first::<PolicyInfo>(connection);
        res.map_err(|err| match err {
            Error::NotFound => Error::NotFound,
            _ => panic!(),
        })
        .ok()
    }
}

impl From<Policy> for PolicyInfo {
    fn from(policy: Policy) -> PolicyInfo {
        PolicyInfo {
            policy_id: None,
            policy_name: policy.name.to_string(),
            policy_info: to_ron_string(policy),
        }
    }
}
