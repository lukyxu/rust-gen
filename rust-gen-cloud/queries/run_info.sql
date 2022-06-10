SELECT runs.run_id,
       runs.version,
       hostname,
       seed,
       success,
       policy_name,
       expected_checksum,
       statistics,
       runs.error_kind,
       runs.error_message,
       sub_run_id,
       opt,
       sub_runs.version,
       compilation_duration_in_millis,
       run_duration_in_micros,
       checksum,
       sub_runs.error_kind,
       sub_runs.error_message
FROM runs
         INNER JOIN sub_runs ON runs.run_id = sub_runs.run_id
         INNER JOIN policies p on runs.policy_id = p.policy_id
WHERE runs.version = '0.2.0'
