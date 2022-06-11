-- This file should undo anything in `up.sql`
SET FOREIGN_KEY_CHECKS = 0;
DROP TABLE runs;
DROP TABLE policies;
DROP TABLE sub_runs;
DROP TABLE statistics_map;
DROP TABLE statistics;
SET FOREIGN_KEY_CHECKS = 1;