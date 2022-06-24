SELECT compiler_name, sr.error_kind, sr.checksum = runs.expected_checksum, COUNT(*) FROM runs JOIN policies p on runs.policy_id = p.policy_id JOIN sub_runs sr on runs.run_id = sr.run_id WHERE opt = '0' AND (compiler_name != 'rustc' or sr.version = 'stable') AND sr.error_kind IS NULL GROUP BY compiler_name, sr.error_kind, sr.checksum = runs.expected_checksum