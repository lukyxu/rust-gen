use crate::ast::function::Function;
use crate::ast::ty::StructTy;
use crate::context::Context;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq)]
#[non_exhaustive]
pub enum Item {
    Struct(StructItem),
    Function(FunctionItem),
}

impl Item {
    pub fn generate_item(ctx: &mut Context) -> Option<Item> {
        let mut res: Option<Item> = None;
        let mut num_failed_attempts = 0;
        while res.is_none() && num_failed_attempts < ctx.policy.max_item_attempts {
            let item_kind = ctx.choose_item_kind();
            res = match item_kind {
                ItemKind::Struct => StructItem::generate_item(ctx).map(From::from),
                ItemKind::Function => FunctionItem::generate_item(ctx).map(From::from),
            };
            if res.is_none() {
                num_failed_attempts += 1;
                *ctx.statistics
                    .failed_item_counter
                    .entry(item_kind)
                    .or_insert(0) += 1;
            } else {
                *ctx.statistics
                    .successful_item_counter
                    .entry(item_kind)
                    .or_insert(0) += 1;
            }
        }
        res
        // None
    }
}

impl From<StructItem> for Item {
    fn from(item: StructItem) -> Item {
        Item::Struct(item)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionItem {
    pub function: Function,
}

impl From<FunctionItem> for Item {
    fn from(item: FunctionItem) -> Item {
        Item::Function(item)
    }
}

impl FunctionItem {
    pub fn generate_main(ctx: &mut Context) -> Option<FunctionItem> {
        Some(FunctionItem {
            function: Function::generate_main_fn(ctx)?,
        })
    }
    fn generate_item(_ctx: &mut Context) -> Option<FunctionItem> {
        todo!()
        // FunctionItem {
        //     function: Function::
        // }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructItem {
    pub struct_ty: StructTy,
}

impl StructItem {
    pub fn generate_item(ctx: &mut Context) -> Option<StructItem> {
        Some(StructItem {
            struct_ty: StructTy::generate_new_type(ctx)?,
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[non_exhaustive]
pub enum ItemKind {
    Struct,
    Function,
}
