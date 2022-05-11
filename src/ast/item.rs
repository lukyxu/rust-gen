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
        while res.is_none() && num_failed_attempts < ctx.policy.max_expr_attempts {
            res = match ctx.choose_item_kind() {
                ItemKind::Struct => StructItem::generate_item(ctx).map(Item::Struct),
                ItemKind::Function => FunctionItem::generate_item(ctx).map(Item::Function),
            };
            if res.is_none() {
                num_failed_attempts += 1;
            }
        }
        res
        // None
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionItem {
    pub function: Function,
}

impl FunctionItem {
    pub fn generate_main(ctx: &mut Context) -> Option<FunctionItem> {
        Some(FunctionItem {
            function: Function::create_main_fn(ctx)?,
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
            struct_ty: StructTy::generate_new_type(ctx, None)?,
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[non_exhaustive]
pub enum ItemKind {
    Struct,
    Function,
}
