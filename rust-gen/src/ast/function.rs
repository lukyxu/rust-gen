use crate::ast::expr::BlockExpr;
use crate::ast::ty::Ty;

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: String,
    pub return_ty: Ty,
    pub block: BlockExpr,
}
