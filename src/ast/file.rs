use crate::ast::item::{FunctionItem, Item};
use crate::context::Context;

pub struct RustFile {
    pub items: Vec<Item>,
}

impl RustFile {
    pub fn generate_file(ctx: &mut Context) -> Option<RustFile> {
        let mut items = vec![];
        for _ in 0..ctx.choose_num_items() - 1 {
            items.push(Item::generate_item(ctx)?);
        }
        items.push(Item::Function(FunctionItem::generate_main(ctx)?));
        Some(RustFile { items })
    }
}