use crate::ast::expr::{BlockExpr, Expr};
use crate::ast::ty::Ty;
use crate::Context;

#[non_exhaustive]
// Make sure Expr is BlockExpr
pub struct Function {
    pub name: String,
    // TODO: Check If it's better to make this a BlockExpr
    pub block: Expr,
}

impl Function {
    pub fn create_main_fn(ctx: &mut Context) -> Function {
        Function {
            name: String::from("main"),
            block: BlockExpr::generate_expr(ctx, &Ty::unit_type()).into(),
        }
    }
}
