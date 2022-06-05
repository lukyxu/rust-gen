use crate::ast::expr::BlockExpr;

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: String,
    pub block: BlockExpr,
}
