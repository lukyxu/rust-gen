
CREATE TABLE policies (
  policy_id INTEGER AUTO_INCREMENT,
  policy_sha256 CHAR(64) NOT NULL,
  policy_name VARCHAR(32) NOT NULL,
  max_file_attempts BIGINT UNSIGNED NOT NULL,
  max_item_attempts BIGINT UNSIGNED NOT NULL,
  max_fn_attempts BIGINT UNSIGNED NOT NULL,
  max_ty_attempts BIGINT UNSIGNED NOT NULL,
  max_stmt_attempts BIGINT UNSIGNED NOT NULL,
  max_expr_attempts BIGINT UNSIGNED NOT NULL,
  num_item_dist VARCHAR(255) NOT NULL,
  item_dist TEXT NOT NULL,
  type_dist TEXT NOT NULL,
  prim_type_dist TEXT NOT NULL,
  num_stmt_dist VARCHAR(255) NOT NULL,
  stmt_dist TEXT NOT NULL,
  mutability_prob DOUBLE NOT NULL,
  expr_dist TEXT NOT NULL,
  bool_true_prob DOUBLE NOT NULL,
  otherwise_if_stmt_prob DOUBLE NOT NULL,
  max_if_else_depth BIGINT UNSIGNED NOT NULL,
  max_block_depth BIGINT UNSIGNED NOT NULL,
  max_arith_depth BIGINT UNSIGNED NOT NULL,
  max_expr_depth BIGINT UNSIGNED NOT NULL,
  array_length_dist VARCHAR(255) NOT NULL,
  default_array_type_dist TEXT NOT NULL,
  new_array_prob DOUBLE NOT NULL,
  max_array_depth BIGINT UNSIGNED NOT NULL,
  max_expr_depth_in_array BIGINT UNSIGNED NOT NULL,
  tuple_length_dist VARCHAR(255) NOT NULL,
  default_tuple_type_dist TEXT NOT NULL,
  new_tuple_prob DOUBLE NOT NULL,
  max_tuple_depth BIGINT UNSIGNED NOT NULL,
  max_expr_depth_in_tuple BIGINT UNSIGNED NOT NULL,
  struct_length_dist VARCHAR(255) NOT NULL,
  default_struct_type_dist TEXT NOT NULL,
  field_struct_prob DOUBLE NOT NULL,
  field_struct_copy_prob DOUBLE NOT NULL,
  tuple_struct_copy_prob DOUBLE NOT NULL,
  max_struct_depth BIGINT UNSIGNED NOT NULL,
  max_expr_depth_in_struct BIGINT UNSIGNED NOT NULL,
  binary_op_dist TEXT NOT NULL,
  unary_op_dist TEXT NOT NULL,
  new_lifetime_prob DOUBLE NOT NULL,
  disable_lifetime BOOLEAN NOT NULL,
  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (policy_id)
);

CREATE TABLE runs (
    run_id INTEGER AUTO_INCREMENT,
    git_hash VARCHAR(255) NOT NULL,
    version VARCHAR(16) NOT NULL,
    hostname VARCHAR(32) NOT NULL,
    seed BIGINT UNSIGNED NOT NULL,
    success BOOLEAN NOT NULL,
    policy_id INTEGER NOT NULL,
    generation_duration_in_millis BIGINT UNSIGNED,
    total_sub_runs BIGINT UNSIGNED NOT NULL,
    expected_checksum NUMERIC(39,0),
    statistics TEXT,
    error_kind VARCHAR(31),
    error_message TEXT,
    run_timeout BIGINT UNSIGNED NOT NULL,
    generate_timeout BIGINT UNSIGNED NOT NULL,
    compile_timeout BIGINT UNSIGNED NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    PRIMARY KEY (run_id),
    FOREIGN KEY (policy_id) REFERENCES policies(policy_id),
    INDEX (version),
    INDEX (hostname),
    INDEX (seed),
    INDEX (success),
    INDEX (policy_id),
    INDEX (generation_duration_in_millis),
    INDEX (total_sub_runs),
    INDEX (error_kind)
);

CREATE TABLE sub_runs(
   sub_run_id INTEGER AUTO_INCREMENT,
   run_id INTEGER NOT NULL,
   compiler_name VARCHAR(16) NOT NULL,
   opt CHAR(1) NOT NULL,
   version VARCHAR(16) NOT NULL,
   success BOOLEAN NOT NULL,
   compilation_duration_in_millis BIGINT UNSIGNED,
   run_duration_in_micros BIGINT UNSIGNED,
   checksum NUMERIC(39,0),
   error_kind VARCHAR(31),
   error_message TEXT,
   created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
   updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
   PRIMARY KEY (sub_run_id),
   FOREIGN KEY (run_id) REFERENCES runs(run_id),
   INDEX (run_id),
   INDEX (compiler_name),
   INDEX (opt),
   INDEX (version),
   INDEX (success),
   INDEX (compilation_duration_in_millis),
   INDEX (run_duration_in_micros),
   INDEX (error_kind)
);