CREATE TABLE runs (
    run_id INTEGER AUTO_INCREMENT,
    git_hash VARCHAR(255) NOT NULL,
    version VARCHAR(255) NOT NULL,
    seed BIGINT UNSIGNED NOT NULL,
    success BOOLEAN NOT NULL,
    policy_id INTEGER NOT NULL,
    statistics VARCHAR(4095),
    error VARCHAR(255),
    PRIMARY KEY (run_id)
);

CREATE TABLE policies (
   policy_id INTEGER AUTO_INCREMENT,
   policy_name VARCHAR(255) NOT NULL,
   policy_info VARCHAR(4095) NOT NULL,
   PRIMARY KEY (policy_id)
);