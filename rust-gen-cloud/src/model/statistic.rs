use crate::schema::statistics;
use chrono::NaiveDateTime;
use diesel::{insert_into, MysqlConnection, RunQueryDsl};
use rust_gen::runtime::run::RunStatistics;

#[derive(Insertable, Queryable)]
#[diesel(primary_key(run_id))]
#[table_name = "statistics"]
pub struct StatisticsInfo {
    pub run_id: i32,
    gen_success_statistics_id: i32,
    gen_failure_statistics_id: i32,
    program_statistics_id: i32,
    max_failed_item_depth: u64,
    max_failed_stmt_depth: u64,
    max_failed_expr_depth: u64,
    max_failed_ty_depth: u64,
    #[diesel(deserialize_as = "NaiveDateTime")]
    pub created_at: Option<NaiveDateTime>,
    #[diesel(deserialize_as = "NaiveDateTime")]
    pub updated_at: Option<NaiveDateTime>,
}

impl StatisticsInfo {
    pub fn new(
        run_id: i32,
        gen_success_statistics_id: i32,
        gen_failure_statistics_id: i32,
        program_statistics_id: i32,
        run_statistics: &RunStatistics,
    ) -> StatisticsInfo {
        let gen_stats = &run_statistics.0;
        StatisticsInfo {
            run_id,
            gen_success_statistics_id,
            gen_failure_statistics_id,
            program_statistics_id,
            max_failed_item_depth: gen_stats.max_failed_item_depth as u64,
            max_failed_stmt_depth: gen_stats.max_failed_stmt_depth as u64,
            max_failed_expr_depth: gen_stats.max_failed_expr_depth as u64,
            max_failed_ty_depth: gen_stats.max_failed_ty_depth as u64,
            created_at: None,
            updated_at: None,
        }
    }

    pub fn insert_new(&self, connection: &MysqlConnection) {
        use crate::schema::statistics::dsl::statistics;
        insert_into(statistics).values(self).execute(connection).unwrap();
    }
}
