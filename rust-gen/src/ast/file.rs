//! File nodes.

use crate::ast::item::Item;

/// Rust file. The current root node of a program.
pub struct RustFile {
    pub items: Vec<Item>,
}
