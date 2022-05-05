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
        let block = BlockExpr::generate_block_expr(ctx, &Ty::unit_type()).unwrap();
        ctx.statistics.main_fn_stmts = block.stmts.len();
        Function {
            name: String::from("main"),
            block
        }
    }
}
