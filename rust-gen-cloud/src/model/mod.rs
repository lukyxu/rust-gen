pub mod policy;
pub mod run;
pub mod statistic;
pub mod statistic_map;
pub mod sub_run;

use diesel::sql_types;

no_arg_sql_function!(last_insert_id, sql_types::Integer);
