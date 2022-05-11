use crate::ast::item::{FunctionItem, Item, StructItem};
use crate::context::Context;

pub struct RustFile {
    pub items: Vec<Item>,
}

impl RustFile {
    pub fn generate_file(ctx: &mut Context) -> Option<RustFile> {
        let mut items = vec![];
        items.push(Item::Struct(StructItem::generate_item(ctx)?));
        items.push(Item::Struct(StructItem::generate_item(ctx)?));
        items.push(Item::Struct(StructItem::generate_item(ctx)?));
        items.push(Item::Function(FunctionItem::generate_main(ctx)?));
        Some(RustFile { items })
    }
}
