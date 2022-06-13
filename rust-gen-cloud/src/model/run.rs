use crate::model::last_insert_id;
use crate::schema::runs;
use bigdecimal::BigDecimal;
use chrono::NaiveDateTime;
use diesel::{insert_into, select, MysqlConnection, RunQueryDsl};
use num_bigint::ToBigInt;
use rust_gen::runtime::error::RunnerError;
use rust_gen::runtime::run::{RunOutput, RunResult, Runner, Statistics};
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
    pub reviewed: bool,
    pub policy_id: i32,
    pub generation_duration_in_millis: Option<u64>,
    pub total_sub_runs: u64,
    pub expected_checksum: Option<BigDecimal>,
    pub statistics: Option<String>,
    pub error_kind: Option<String>,
    pub error_message: Option<String>,
    pub run_timeout: u64,
    pub generate_timeout: u64,
    pub compile_timeout: u64,
    pub rustfmt_timeout: u64,
    #[diesel(deserialize_as = "NaiveDateTime")]
    pub created_at: Option<NaiveDateTime>,
    #[diesel(deserialize_as = "NaiveDateTime")]
    pub updated_at: Option<NaiveDateTime>,
}

impl RunInfo {
    pub fn new(
        seed: u64,
        run_output: RunResult,
        policy_id: i32,
        statistics: &Option<Statistics>,
        runner: &Runner,
    ) -> RunInfo {
        RunInfo {
            run_id: None,
            git_hash: env!("GIT_HASH").to_string(),
            version: env!("CARGO_PKG_VERSION").to_string(),
            hostname: env!("HOSTNAME").to_string(),
            seed,
            success: run_output.is_ok(),
            reviewed: run_output.is_ok(),
            policy_id,
            generation_duration_in_millis: RunOutput::from_run_result(&run_output)
                .generation_time
                .map(|duration| duration.as_millis() as u64),
            total_sub_runs: RunOutput::from_run_result(&run_output).subruns.len() as u64,
            expected_checksum: RunOutput::from_run_result(&run_output)
                .expected_checksum
                .map(|checksum| BigDecimal::new(checksum.to_bigint().unwrap(), 0)),
            statistics: statistics.as_ref().map(|stats| to_ron_string(stats)),
            error_kind: run_output
                .as_ref()
                .err()
                .map(|err| err.error_kind().to_owned()),
            error_message: run_output.as_ref().err().map(RunnerError::to_string),
            run_timeout: runner.run_timeout.as_secs(),
            generate_timeout: runner.generate_timeout.as_secs(),
            compile_timeout: runner.compile_timeout.as_secs(),
            rustfmt_timeout: runner.rustfmt_timeout.as_secs(),
            created_at: None,
            updated_at: None,
        }
    }

    pub fn insert_new(&self, connection: &MysqlConnection) -> i32 {
        use crate::schema::runs::dsl::runs;
        insert_into(runs).values(self).execute(connection).unwrap();
        select(last_insert_id).first(connection).unwrap()
    }
}
