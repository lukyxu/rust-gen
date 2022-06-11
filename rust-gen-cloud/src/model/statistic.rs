use crate::schema::statistics;
use chrono::NaiveDateTime;

#[derive(Insertable, Queryable)]
#[diesel(primary_key(run_id))]
#[table_name = "statistics"]
pub struct StatisticsInfo {
    #[diesel(deserialize_as = "i32")]
    pub run_id: Option<i32>,
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
