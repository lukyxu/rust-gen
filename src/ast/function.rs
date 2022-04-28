use crate::ast::expr::BlockExpr;
use crate::ast::ty::Ty;
use crate::context::Context;

#[non_exhaustive]
// Make sure Expr is BlockExpr
pub struct Function {
    pub name: String,
    pub block: BlockExpr,
}

impl Function {
    pub fn create_main_fn(ctx: &mut Context) -> Function {
        Function {
            name: String::from("main"),
            block: BlockExpr::generate_block_expr(ctx, &Ty::unit_type()).unwrap(),
        }
    }
}
