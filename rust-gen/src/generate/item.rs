use crate::ast::function::Function;
use crate::ast::item::{FunctionItem, Item, ItemKind, StructItem};
use crate::ast::ty::StructTy;
use crate::context::Context;
use crate::generate::utils::track_item;
use std::cmp::max;

impl Item {
    /// Attempts multiple times given by `ctx.policy.max_item_attempts` to generate a valid item.
    pub fn fuzz_item(ctx: &mut Context) -> Option<Item> {
        let mut res: Option<Item> = None;
        let mut num_failed_attempts = 0;
        while res.is_none() && num_failed_attempts < ctx.policy.max_item_attempts {
            res = Item::generate_item(ctx);
            if res.is_none() {
                num_failed_attempts += 1;
                ctx.statistics.max_failed_item_depth =
                    max(ctx.statistics.max_failed_item_depth, num_failed_attempts);
            }
        }
        res
    }

    /// Attempts a single attempt to generate a valid item.
    pub fn generate_item(ctx: &mut Context) -> Option<Item> {
        let item_kind = ctx.choose_item_kind();
        match item_kind {
            ItemKind::Struct => StructItem::generate_item(ctx).map(From::from),
            ItemKind::Function => FunctionItem::generate_item(ctx).map(From::from),
        }
    }
}

impl FunctionItem {
    pub fn generate_main(ctx: &mut Context) -> Option<FunctionItem> {
        Some(FunctionItem {
            function: Function::fuzz_main_fn(ctx)?,
        })
    }
    fn generate_item(ctx: &mut Context) -> Option<FunctionItem> {
        Some(FunctionItem {
            function: Function::generate_fn(ctx)?,
        })
    }
}

impl StructItem {
    pub fn generate_item(ctx: &mut Context) -> Option<StructItem> {
        track_item(
            ItemKind::Struct,
            Box::new(StructItem::generate_item_internal),
        )(ctx)
    }

    fn generate_item_internal(ctx: &mut Context) -> Option<StructItem> {
        Some(StructItem {
            struct_ty: StructTy::generate_new_type(ctx)?,
        })
    }
}
