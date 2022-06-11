CREATE TABLE policies
(
    policy_id                INTEGER AUTO_INCREMENT,
    policy_sha256            CHAR(64)        NOT NULL,
    policy_name              VARCHAR(32)     NOT NULL,
    max_file_attempts        BIGINT UNSIGNED NOT NULL,
    max_item_attempts        BIGINT UNSIGNED NOT NULL,
    max_fn_attempts          BIGINT UNSIGNED NOT NULL,
    max_ty_attempts          BIGINT UNSIGNED NOT NULL,
    max_stmt_attempts        BIGINT UNSIGNED NOT NULL,
    max_expr_attempts        BIGINT UNSIGNED NOT NULL,
    num_item_dist            VARCHAR(255)    NOT NULL,
    item_dist                TEXT            NOT NULL,
    type_dist                TEXT            NOT NULL,
    prim_type_dist           TEXT            NOT NULL,
    num_stmt_dist            VARCHAR(255)    NOT NULL,
    stmt_dist                TEXT            NOT NULL,
    mutability_prob          DOUBLE          NOT NULL,
    expr_dist                TEXT            NOT NULL,
    bool_true_prob           DOUBLE          NOT NULL,
    otherwise_if_stmt_prob   DOUBLE          NOT NULL,
    max_if_else_depth        BIGINT UNSIGNED NOT NULL,
    max_block_depth          BIGINT UNSIGNED NOT NULL,
    max_arith_depth          BIGINT UNSIGNED NOT NULL,
    max_expr_depth           BIGINT UNSIGNED NOT NULL,
    array_length_dist        VARCHAR(255)    NOT NULL,
    default_array_type_dist  TEXT            NOT NULL,
    new_array_prob           DOUBLE          NOT NULL,
    max_array_depth          BIGINT UNSIGNED NOT NULL,
    max_expr_depth_in_array  BIGINT UNSIGNED NOT NULL,
    tuple_length_dist        VARCHAR(255)    NOT NULL,
    default_tuple_type_dist  TEXT            NOT NULL,
    new_tuple_prob           DOUBLE          NOT NULL,
    max_tuple_depth          BIGINT UNSIGNED NOT NULL,
    max_expr_depth_in_tuple  BIGINT UNSIGNED NOT NULL,
    struct_length_dist       VARCHAR(255)    NOT NULL,
    default_struct_type_dist TEXT            NOT NULL,
    field_struct_prob        DOUBLE          NOT NULL,
    field_struct_copy_prob   DOUBLE          NOT NULL,
    tuple_struct_copy_prob   DOUBLE          NOT NULL,
    max_struct_depth         BIGINT UNSIGNED NOT NULL,
    max_expr_depth_in_struct BIGINT UNSIGNED NOT NULL,
    binary_op_dist           TEXT            NOT NULL,
    unary_op_dist            TEXT            NOT NULL,
    new_lifetime_prob        DOUBLE          NOT NULL,
    disable_lifetime         BOOLEAN         NOT NULL,
    created_at               TIMESTAMP       NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at               TIMESTAMP       NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    PRIMARY KEY (policy_id)
);

CREATE TABLE runs
(
    run_id                        INTEGER AUTO_INCREMENT,
    git_hash                      CHAR(40)        NOT NULL,
    version                       VARCHAR(16)     NOT NULL,
    hostname                      VARCHAR(32)     NOT NULL,
    seed                          BIGINT UNSIGNED NOT NULL,
    success                       BOOLEAN         NOT NULL,
    policy_id                     INTEGER         NOT NULL,
    generation_duration_in_millis BIGINT UNSIGNED,
    total_sub_runs                BIGINT UNSIGNED NOT NULL,
    expected_checksum             NUMERIC(39, 0),
    statistics                    TEXT,
    error_kind                    VARCHAR(31),
    error_message                 TEXT,
    run_timeout                   BIGINT UNSIGNED NOT NULL,
    generate_timeout              BIGINT UNSIGNED NOT NULL,
    compile_timeout               BIGINT UNSIGNED NOT NULL,
    created_at                    TIMESTAMP       NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at                    TIMESTAMP       NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    PRIMARY KEY (run_id),
    FOREIGN KEY (policy_id) REFERENCES policies (policy_id),
    INDEX (git_hash),
    INDEX (version),
    INDEX (hostname),
    INDEX (seed),
    INDEX (success),
    INDEX (policy_id),
    INDEX (generation_duration_in_millis),
    INDEX (total_sub_runs),
    INDEX (error_kind)
);

CREATE TABLE sub_runs
(
    sub_run_id                     INTEGER AUTO_INCREMENT,
    run_id                         INTEGER     NOT NULL,
    compiler_name                  VARCHAR(16) NOT NULL,
    opt                            CHAR(1)     NOT NULL,
    version                        VARCHAR(16) NOT NULL,
    success                        BOOLEAN     NOT NULL,
    compilation_duration_in_millis BIGINT UNSIGNED,
    run_duration_in_micros         BIGINT UNSIGNED,
    checksum                       NUMERIC(39, 0),
    error_kind                     VARCHAR(31),
    error_message                  TEXT,
    created_at                     TIMESTAMP   NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at                     TIMESTAMP   NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    PRIMARY KEY (sub_run_id),
    FOREIGN KEY (run_id) REFERENCES runs (run_id),
    INDEX (run_id),
    INDEX (compiler_name),
    INDEX (opt),
    INDEX (version),
    INDEX (success),
    INDEX (compilation_duration_in_millis),
    INDEX (run_duration_in_micros),
    INDEX (error_kind)
);

CREATE TABLE statistics_map
(
    statistics_map_id      INTEGER AUTO_INCREMENT,
    total_items            BIGINT UNSIGNED,
    total_stmts            BIGINT UNSIGNED,
    total_exprs            BIGINT UNSIGNED,
    total_tys              BIGINT UNSIGNED,
    total_binary_ops       BIGINT UNSIGNED,
    total_unary_ops        BIGINT UNSIGNED,
    item_struct            BIGINT UNSIGNED,
    item_function          BIGINT UNSIGNED,
    stmt_local             BIGINT UNSIGNED,
    stmt_semi              BIGINT UNSIGNED,
    stmt_expr              BIGINT UNSIGNED,
    expr_literal           BIGINT UNSIGNED,
    expr_binary            BIGINT UNSIGNED,
    expr_unary             BIGINT UNSIGNED,
    expr_cast              BIGINT UNSIGNED,
    expr_if                BIGINT UNSIGNED,
    expr_block             BIGINT UNSIGNED,
    expr_ident             BIGINT UNSIGNED,
    expr_assign            BIGINT UNSIGNED,
    expr_index             BIGINT UNSIGNED,
    expr_field             BIGINT UNSIGNED,
    expr_reference         BIGINT UNSIGNED,
    ty_unit                BIGINT UNSIGNED,
    ty_prim                BIGINT UNSIGNED,
    ty_tuple               BIGINT UNSIGNED,
    ty_array               BIGINT UNSIGNED,
    ty_struct              BIGINT UNSIGNED,
    ty_reference           BIGINT UNSIGNED,
    binary_op_add          BIGINT UNSIGNED,
    binary_op_sub          BIGINT UNSIGNED,
    binary_op_mul          BIGINT UNSIGNED,
    binary_op_div          BIGINT UNSIGNED,
    binary_op_and          BIGINT UNSIGNED,
    binary_op_or           BIGINT UNSIGNED,
    binary_op_bit_xor      BIGINT UNSIGNED,
    binary_op_bit_and      BIGINT UNSIGNED,
    binary_op_bit_or       BIGINT UNSIGNED,
    binary_op_eq           BIGINT UNSIGNED,
    binary_op_lq           BIGINT UNSIGNED,
    binary_op_le           BIGINT UNSIGNED,
    binary_op_ne           BIGINT UNSIGNED,
    binary_op_ge           BIGINT UNSIGNED,
    binary_op_gt           BIGINT UNSIGNED,
    binary_op_wrapping_add BIGINT UNSIGNED,
    binary_op_wrapping_sub BIGINT UNSIGNED,
    binary_op_wrapping_mul BIGINT UNSIGNED,
    binary_op_wrapping_div BIGINT UNSIGNED,
    binary_op_wrapping_rem BIGINT UNSIGNED,
    binary_op_wrapping_shl BIGINT UNSIGNED,
    binary_op_wrapping_shr BIGINT UNSIGNED,
    unary_op_deref         BIGINT UNSIGNED,
    unary_op_not           BIGINT UNSIGNED,
    unary_op_neg           BIGINT UNSIGNED,
    created_at             TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at             TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    PRIMARY KEY (statistics_map_id),
    INDEX (total_items),
    INDEX (total_stmts),
    INDEX (total_exprs),
    INDEX (total_tys),
    INDEX (total_binary_ops),
    INDEX (total_unary_ops)
);

CREATE TABLE statistics
(
    run_id                    INTEGER         NOT NULL,
    gen_success_statistics_id INTEGER         NOT NULL,
    gen_failure_statistics_id INTEGER         NOT NULL,
    program_statistics_id     INTEGER         NOT NULL,
    max_failed_item_depth     BIGINT UNSIGNED NOT NULL,
    max_failed_stmt_depth     BIGINT UNSIGNED NOT NULL,
    max_failed_expr_depth     BIGINT UNSIGNED NOT NULL,
    max_failed_ty_depth       BIGINT UNSIGNED NOT NULL,
    word_count                BIGINT UNSIGNED NOT NULL,
    line_count                BIGINT UNSIGNED NOT NULL,
    created_at                TIMESTAMP       NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at                TIMESTAMP       NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    PRIMARY KEY (run_id),
    FOREIGN KEY (gen_success_statistics_id) REFERENCES statistics_map (statistics_map_id),
    FOREIGN KEY (gen_failure_statistics_id) REFERENCES statistics_map (statistics_map_id),
    FOREIGN KEY (program_statistics_id) REFERENCES statistics_map (statistics_map_id),
    INDEX (gen_success_statistics_id),
    INDEX (gen_failure_statistics_id),
    INDEX (program_statistics_id),
    INDEX (max_failed_item_depth),
    INDEX (max_failed_stmt_depth),
    INDEX (max_failed_expr_depth),
    INDEX (max_failed_ty_depth),
    INDEX (word_count),
    INDEX (line_count)
);

