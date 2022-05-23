use crate::ast::item::{FunctionItem, Item};
use crate::context::Context;
use std::cmp::max;

/// Rust file. The current root node of a program.
pub struct RustFile {
    pub items: Vec<Item>,
}

impl RustFile {
    /// Attempts multiple times given by `ctx.policy.max_file_attempts` to generate a valid Rust file.
    pub fn fuzz_file(ctx: &mut Context) -> Option<RustFile> {
        let mut res: Option<RustFile> = None;
        let mut num_failed_attempts = 0;
        while res.is_none() && num_failed_attempts < ctx.policy.max_file_attempts {
            res = RustFile::generate_file(ctx);
            if res.is_none() {
                num_failed_attempts += 1;
            }
        }
        res
    }

    /// Attempts a single attempt to generate a valid Rust file.
    pub fn generate_file(ctx: &mut Context) -> Option<RustFile> {
        let mut items = vec![];
        for _ in 0..max(ctx.choose_num_items(), 1) - 1 {
            items.push(Item::fuzz_item(ctx)?);
        }
        items.push(Item::Function(FunctionItem::generate_main(ctx)?));
        Some(RustFile { items })
    }
}
