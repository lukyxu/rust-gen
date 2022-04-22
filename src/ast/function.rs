use crate::ast::expr::BlockExpr;
use crate::ast::ty::Ty;
use crate::Context;

#[non_exhaustive]
// Make sure Expr is BlockExpr
pub struct Function<'ir> {
    pub name: String,
    // TODO: Check If it's better to make this a BlockExpr
    pub block: BlockExpr<'ir>,
}

impl <'ir> Function<'ir> {
    pub fn create_main_fn(ctx: &mut Context) -> Function<'ir> {
        Function {
            name: String::from("main"),
            block: BlockExpr::generate_block_expr(ctx, &Ty::unit_type()).unwrap(),
        }
    }
}
