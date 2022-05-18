//! Rust gen is a random program that can generate correct runnable Rust programs along with the
//! expected output of the program. It's primary purpose is to find compiler bugs either through
//! comparing the output of running the test program with the expected output, or through
//! differential testing.

pub mod ast;
pub mod context;
pub mod distribution;
pub mod generator;
pub mod policy;
pub mod runtime;
pub mod statistics;
pub mod symbol_table;
pub mod utils;
pub mod visitor;
