use crate::schema::sub_runs;
use bigdecimal::BigDecimal;
use chrono::NaiveDateTime;
use diesel::{insert_into, MysqlConnection, RunQueryDsl};
use num_bigint::ToBigInt;
use rust_gen::runtime::run::{SubRunError, SubRunOutput, SubRunResult};

#[derive(Insertable, Queryable, Debug, Clone, PartialEq)]
#[diesel(primary_key(policy_id))]
#[table_name = "sub_runs"]
pub struct SubRunInfo {
    #[diesel(deserialize_as = "i32")]
    pub sub_run_id: Option<i32>,
    pub run_id: i32,
    pub compiler_name: String,
    pub opt: String,
    pub version: String,
    pub success: bool,
    pub compilation_duration_in_millis: Option<u64>,
    pub run_duration_in_micros: Option<u64>,
    pub checksum: Option<BigDecimal>,
    pub error_kind: Option<String>,
    pub error_message: Option<String>,
    #[diesel(deserialize_as = "NaiveDateTime")]
    pub created_at: Option<NaiveDateTime>,
    #[diesel(deserialize_as = "NaiveDateTime")]
    pub updated_at: Option<NaiveDateTime>,
}

impl SubRunInfo {
    pub fn new(run_id: i32, sub_run: SubRunResult) -> SubRunInfo {
        let output = SubRunOutput::from_sub_run_result(&sub_run).clone();
        SubRunInfo {
            sub_run_id: None,
            run_id,
            compiler_name: output.compiler_name,
            opt: output.opt.to_char().to_string(),
            version: output.version.to_string(),
            success: sub_run.as_ref().is_ok(),
            compilation_duration_in_millis: output
                .compilation_duration
                .map(|duration| duration.as_millis() as u64),
            run_duration_in_micros: output
                .run_duration
                .map(|duration| duration.as_micros() as u64),
            checksum: output
                .checksum
                .map(|checksum| BigDecimal::new(checksum.to_bigint().unwrap(), 0)),
            error_kind: sub_run
                .as_ref()
                .err()
                .map(|err| err.error_kind().to_string()),
            error_message: sub_run.as_ref().err().map(SubRunError::to_string),
            created_at: None,
            updated_at: None,
        }
    }

    pub fn insert_new(&self, connection: &MysqlConnection) {
        use crate::schema::sub_runs::dsl::sub_runs;
        insert_into(sub_runs)
            .values(self)
            .execute(connection)
            .unwrap();
    }
}
