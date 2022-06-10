table! {
    policies (policy_id) {
        policy_id -> Integer,
        policy_sha256 -> Char,
        policy_name -> Varchar,
        max_file_attempts -> Unsigned<Bigint>,
        max_item_attempts -> Unsigned<Bigint>,
        max_fn_attempts -> Unsigned<Bigint>,
        max_ty_attempts -> Unsigned<Bigint>,
        max_stmt_attempts -> Unsigned<Bigint>,
        max_expr_attempts -> Unsigned<Bigint>,
        num_item_dist -> Varchar,
        item_dist -> Text,
        type_dist -> Text,
        prim_type_dist -> Text,
        num_stmt_dist -> Varchar,
        stmt_dist -> Text,
        mutability_prob -> Double,
        expr_dist -> Text,
        bool_true_prob -> Double,
        otherwise_if_stmt_prob -> Double,
        max_if_else_depth -> Unsigned<Bigint>,
        max_block_depth -> Unsigned<Bigint>,
        max_arith_depth -> Unsigned<Bigint>,
        max_expr_depth -> Unsigned<Bigint>,
        array_length_dist -> Varchar,
        default_array_type_dist -> Text,
        new_array_prob -> Double,
        max_array_depth -> Unsigned<Bigint>,
        max_expr_depth_in_array -> Unsigned<Bigint>,
        tuple_length_dist -> Varchar,
        default_tuple_type_dist -> Text,
        new_tuple_prob -> Double,
        max_tuple_depth -> Unsigned<Bigint>,
        max_expr_depth_in_tuple -> Unsigned<Bigint>,
        struct_length_dist -> Varchar,
        default_struct_type_dist -> Text,
        field_struct_prob -> Double,
        field_struct_copy_prob -> Double,
        tuple_struct_copy_prob -> Double,
        max_struct_depth -> Unsigned<Bigint>,
        max_expr_depth_in_struct -> Unsigned<Bigint>,
        binary_op_dist -> Text,
        unary_op_dist -> Text,
        new_lifetime_prob -> Double,
        disable_lifetime -> Bool,
        created_at -> Timestamp,
        updated_at -> Timestamp,
    }
}

table! {
    runs (run_id) {
        run_id -> Integer,
        git_hash -> Varchar,
        version -> Varchar,
        hostname -> Varchar,
        seed -> Unsigned<Bigint>,
        success -> Bool,
        policy_id -> Integer,
        generation_duration_in_millis -> Nullable<Unsigned<Bigint>>,
        total_sub_runs -> Unsigned<Bigint>,
        expected_checksum -> Nullable<Decimal>,
        statistics -> Nullable<Text>,
        error_kind -> Nullable<Varchar>,
        error_message -> Nullable<Text>,
        run_timeout -> Unsigned<Bigint>,
        generate_timeout -> Unsigned<Bigint>,
        compile_timeout -> Unsigned<Bigint>,
        created_at -> Timestamp,
        updated_at -> Timestamp,
    }
}

table! {
    sub_runs (sub_run_id) {
        sub_run_id -> Integer,
        run_id -> Integer,
        compiler_name -> Varchar,
        opt -> Char,
        version -> Varchar,
        compilation_duration_in_millis -> Nullable<Unsigned<Bigint>>,
        run_duration_in_micros -> Nullable<Unsigned<Bigint>>,
        checksum -> Nullable<Decimal>,
        error_kind -> Nullable<Varchar>,
        error_message -> Nullable<Text>,
        created_at -> Timestamp,
        updated_at -> Timestamp,
    }
}

joinable!(runs -> policies (policy_id));
joinable!(sub_runs -> runs (run_id));

allow_tables_to_appear_in_same_query!(
    policies,
    runs,
    sub_runs,
);
