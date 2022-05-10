use crate::ast::item::{FunctionItem, Item, StructItem};
use crate::context::Context;

pub struct File {
    items: Vec<Item>
}

impl File {
    fn generate_file(ctx: &mut Context) {
        let mut items = vec![];
        items.push(StructItem::);
        items.push(Item::generate_item(ctx));
        items.push(Item::generate_item(ctx));
        items.push(FunctionItem::generate_main(ctx).map(Item::Function));
    }
}