use crate::ast::function::Function;
use crate::ast::ty::StructTy;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq)]
#[non_exhaustive]
pub enum Item {
    Struct(StructItem),
    Function(FunctionItem),
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

#[derive(Debug, Clone, PartialEq)]
pub struct StructItem {
    pub struct_ty: StructTy,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
#[non_exhaustive]
pub enum ItemKind {
    Struct,
    Function,
}
