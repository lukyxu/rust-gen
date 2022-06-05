use diesel::{EqAll, insert_into, MysqlConnection, QueryDsl, RunQueryDsl};
use diesel::result::Error;
use ron::ser::PrettyConfig;
use rust_gen::policy::Policy;
use rust_gen::runtime::run::RunResult;
use rust_gen::schema::policies::dsl::policies as policy_table;
use rust_gen::schema::policies;
use rust_gen::schema::policies::policy_info;
use rust_gen::schema::runs;

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
    pub statistics: String,
    pub error: String,
}

// impl RunInfo {
//     pub fn new(run_output: RunResult) -> RunInfo {
//
//     }
// }


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
    pub fn insert_new(new: &PolicyInfo, connection: &MysqlConnection) {
        insert_into(policy_table).values(new.clone()).execute(connection).unwrap();
    }

    pub fn query(other: &PolicyInfo, connection: &MysqlConnection) -> Option<PolicyInfo> {
        let res = policy_table
            .filter(policy_info.eq_all(other.policy_info.clone()))
            .first::<PolicyInfo>(connection);
        res.map_err(|err|{
            match err {
                Error::NotFound => Error::NotFound,
                _ => panic!()
            }
        }).ok()
    }
}

impl From<Policy> for PolicyInfo {
    fn from(policy: Policy) -> PolicyInfo {
        PolicyInfo {
            policy_id: None,
            policy_name: policy.name.to_string(),
            policy_info: ron::ser::to_string_pretty(&policy, PrettyConfig::new()).unwrap(),
        }
    }
}
