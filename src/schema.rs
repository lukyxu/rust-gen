table! {
    policies (policy_id) {
        policy_id -> Integer,
        policy_name -> Varchar,
        policy_info -> Varchar,
    }
}

table! {
    runs (run_id) {
        run_id -> Integer,
        git_hash -> Varchar,
        version -> Varchar,
        seed -> Unsigned<Bigint>,
        success -> Bool,
        policy_id -> Integer,
        statistics -> Nullable<Varchar>,
        error -> Nullable<Varchar>,
    }
}

allow_tables_to_appear_in_same_query!(
    policies,
    runs,
);
