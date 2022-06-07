use chrono::{NaiveDateTime};
use crate::schema::runs;
use crate::schema::policies;
use diesel::result::Error;
use diesel::{ExpressionMethods, insert_into, MysqlConnection, Queryable, QueryDsl, QueryResult, RunQueryDsl};
use sha2::{Sha256, Digest};
use rust_gen::policy::Policy;
use rust_gen::runtime::error::RunnerError;
use rust_gen::runtime::run::RunResult;
use rust_gen::utils::{from_ron_string, to_ron_string};

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
    #[diesel(deserialize_as = "NaiveDateTime")]
    pub created_at: Option<NaiveDateTime>,
    #[diesel(deserialize_as = "NaiveDateTime")]
    pub updated_at: Option<NaiveDateTime>
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
            created_at: None,
            updated_at: None,
        }
    }

    pub fn insert_new(&self, connection: &MysqlConnection) {
        use crate::schema::runs::dsl::runs;
        insert_into(runs)
            .values(self.clone())
            .execute(connection)
            .unwrap();
    }
}

#[derive(Insertable, Queryable, Debug, Clone, PartialEq)]
#[diesel(primary_key(policy_id))]
#[table_name = "policies"]
pub struct PolicyInfo {
    #[diesel(deserialize_as = "i32")]
    pub policy_id: Option<i32>,
    pub policy_sha256: String,
    pub name: String,
    pub max_file_attempts: u64,
    pub max_item_attempts: u64,
    pub max_fn_attempts: u64,
    pub max_ty_attempts: u64,
    pub max_stmt_attempts: u64,
    pub max_expr_attempts: u64,
    pub num_item_dist: String,
    pub item_dist: String,
    pub type_dist: String,
    pub prim_type_dist: String,
    pub num_stmt_dist: String,
    pub stmt_dist: String,
    pub mutability_prob: f64,
    pub expr_dist: String,
    pub bool_true_prob: f64,
    pub otherwise_if_stmt_prob: f64,
    pub max_if_else_depth: u64,
    pub max_block_depth: u64,
    pub max_arith_depth: u64,
    pub max_expr_depth: u64,
    pub array_length_dist: String,
    pub default_array_type_dist: String,
    pub new_array_prob: f64,
    pub max_array_depth: u64,
    pub max_expr_depth_in_array: u64,
    pub tuple_length_dist: String,
    pub default_tuple_type_dist: String,
    pub new_tuple_prob: f64,
    pub max_tuple_depth: u64,
    pub max_expr_depth_in_tuple: u64,
    pub struct_length_dist: String,
    pub default_struct_type_dist: String,
    pub field_struct_prob: f64,
    pub field_struct_copy_prob: f64,
    pub tuple_struct_copy_prob: f64,
    pub max_struct_depth: u64,
    pub max_expr_depth_in_struct: u64,
    pub binary_op_dist: String,
    pub unary_op_dist: String,
    pub new_lifetime_prob: f64,
    pub disable_lifetime: bool,
    #[diesel(deserialize_as = "NaiveDateTime")]
    pub created_at: Option<NaiveDateTime>,
    #[diesel(deserialize_as = "NaiveDateTime")]
    pub updated_at: Option<NaiveDateTime>
}

impl PolicyInfo {
    pub fn insert_new(&self, connection: &MysqlConnection) {
        use crate::schema::policies::dsl::policies;
        insert_into(policies)
            .values(self.clone())
            .execute(connection)
            .unwrap();
    }

    pub fn query(&self, connection: &MysqlConnection) -> Option<PolicyInfo> {
        use crate::schema::policies::dsl::*;
        let res: QueryResult<Vec<PolicyInfo>> = policies.filter(policy_sha256.eq(&self.policy_sha256)).load::<PolicyInfo>(connection);
        res.and_then(|res|res.into_iter().filter(|policy| {
            Policy::from(self.clone()) == Policy::from(policy.clone())
        }).next().ok_or(Error::NotFound)).map_err(|err| match err {
            Error::NotFound => Error::NotFound,
            _ => panic!(),
        }).ok()
    }
}

impl From<Policy> for PolicyInfo {
    fn from(policy: Policy) -> PolicyInfo {
        PolicyInfo {
            policy_id: None,
            policy_sha256: format!("{:X}", Sha256::digest(to_ron_string(&policy))),
            name: policy.name,
            max_file_attempts: policy.max_file_attempts as u64,
            max_item_attempts: policy.max_item_attempts as u64,
            max_fn_attempts: policy.max_fn_attempts as u64,
            max_ty_attempts: policy.max_ty_attempts as u64,
            max_stmt_attempts: policy.max_stmt_attempts as u64,
            max_expr_attempts: policy.max_expr_attempts as u64,
            num_item_dist: to_ron_string(policy.num_item_dist),
            item_dist: to_ron_string(policy.item_dist),
            type_dist: to_ron_string(policy.type_dist),
            prim_type_dist: to_ron_string(policy.prim_type_dist),
            num_stmt_dist: to_ron_string(policy.num_stmt_dist),
            stmt_dist: to_ron_string(policy.stmt_dist),
            mutability_prob: policy.mutability_prob,
            expr_dist: to_ron_string(policy.expr_dist),
            bool_true_prob: policy.bool_true_prob,
            otherwise_if_stmt_prob: policy.otherwise_if_stmt_prob,
            max_if_else_depth: policy.max_if_else_depth as u64,
            max_block_depth: policy.max_block_depth as u64,
            max_arith_depth: policy.max_arith_depth as u64,
            max_expr_depth: policy.max_expr_depth as u64,
            array_length_dist: to_ron_string(policy.array_length_dist),
            default_array_type_dist: to_ron_string(policy.default_array_type_dist),
            new_array_prob: policy.new_array_prob,
            max_array_depth: policy.max_array_depth as u64,
            max_expr_depth_in_array: policy.max_expr_depth_in_array as u64,
            tuple_length_dist: to_ron_string(policy.tuple_length_dist),
            default_tuple_type_dist: to_ron_string(policy.default_tuple_type_dist),
            new_tuple_prob: policy.new_tuple_prob,
            max_tuple_depth: policy.max_tuple_depth as u64,
            max_expr_depth_in_tuple: policy.max_expr_depth_in_tuple as u64,
            struct_length_dist: to_ron_string(policy.struct_length_dist),
            default_struct_type_dist: to_ron_string(policy.default_struct_type_dist),
            field_struct_prob: policy.field_struct_prob,
            field_struct_copy_prob: policy.field_struct_copy_prob,
            tuple_struct_copy_prob: policy.tuple_struct_copy_prob,
            max_struct_depth: policy.max_struct_depth as u64,
            max_expr_depth_in_struct: policy.max_expr_depth_in_struct as u64,
            binary_op_dist: to_ron_string(policy.binary_op_dist),
            unary_op_dist: to_ron_string(policy.unary_op_dist),
            new_lifetime_prob: policy.new_lifetime_prob,
            disable_lifetime: policy.disable_lifetime,
            created_at: None,
            updated_at: None,
        }
    }
}

impl From<PolicyInfo> for Policy {
    fn from(policy: PolicyInfo) -> Policy {
        Policy {
            name: policy.name.to_string(),
            max_file_attempts: policy.max_file_attempts as usize,
            max_item_attempts: policy.max_item_attempts as usize,
            max_fn_attempts: policy.max_fn_attempts as usize,
            max_ty_attempts: policy.max_ty_attempts as usize,
            max_stmt_attempts: policy.max_stmt_attempts as usize,
            max_expr_attempts: policy.max_expr_attempts as usize,
            num_item_dist: from_ron_string(&policy.num_item_dist),
            item_dist: from_ron_string(&policy.item_dist),
            type_dist: from_ron_string(&policy.type_dist),
            prim_type_dist: from_ron_string(&policy.prim_type_dist),
            num_stmt_dist: from_ron_string(&policy.num_stmt_dist),
            stmt_dist: from_ron_string(&policy.stmt_dist),
            mutability_prob: policy.mutability_prob,
            expr_dist: from_ron_string(&policy.expr_dist),
            bool_true_prob: policy.bool_true_prob,
            otherwise_if_stmt_prob: policy.otherwise_if_stmt_prob,
            max_if_else_depth: policy.max_if_else_depth as usize,
            max_block_depth: policy.max_block_depth as usize,
            max_arith_depth: policy.max_arith_depth as usize,
            max_expr_depth: policy.max_expr_depth as usize,
            array_length_dist: from_ron_string(&policy.array_length_dist),
            default_array_type_dist: from_ron_string(&policy.default_array_type_dist),
            new_array_prob: policy.new_array_prob,
            max_array_depth: policy.max_array_depth as usize,
            max_expr_depth_in_array: policy.max_expr_depth_in_array as usize,
            tuple_length_dist: from_ron_string(&policy.tuple_length_dist),
            default_tuple_type_dist: from_ron_string(&policy.default_tuple_type_dist),
            new_tuple_prob: policy.new_tuple_prob,
            max_tuple_depth: policy.max_tuple_depth as usize,
            max_expr_depth_in_tuple: policy.max_expr_depth_in_tuple as usize,
            struct_length_dist: from_ron_string(&policy.struct_length_dist),
            default_struct_type_dist: from_ron_string(&policy.default_struct_type_dist),
            field_struct_prob: policy.field_struct_prob,
            field_struct_copy_prob: policy.field_struct_copy_prob,
            tuple_struct_copy_prob: policy.tuple_struct_copy_prob,
            max_struct_depth: policy.max_struct_depth as usize,
            max_expr_depth_in_struct: policy.max_expr_depth_in_struct as usize,
            binary_op_dist: from_ron_string(&policy.binary_op_dist),
            unary_op_dist: from_ron_string(&policy.unary_op_dist),
            new_lifetime_prob: policy.new_lifetime_prob,
            disable_lifetime: policy.disable_lifetime
        }
    }
}

#[cfg(test)]
mod tests {
    use rust_gen::policy::Policy;
    use crate::PolicyInfo;

    #[test]
    fn convert_between_policies() {
        let original_policy = Policy::default();
        let original_policy_info: PolicyInfo = original_policy.clone().into();
        let final_policy: Policy = original_policy_info.clone().into();
        let final_policy_info: PolicyInfo = final_policy.clone().into();
        assert_eq!(original_policy, final_policy);
        assert_eq!(original_policy_info, final_policy_info);
    }
}