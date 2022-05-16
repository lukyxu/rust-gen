use crate::ast::expr::BlockExpr;
use crate::ast::ty::Ty;
use crate::context::Context;

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: String,
    pub block: BlockExpr,
}

impl Function {
    pub fn create_main_fn(ctx: &mut Context) -> Option<Function> {
        let block = BlockExpr::generate_expr(ctx, &Ty::unit_type())?;
        ctx.statistics.main_fn_stmts = block.stmts.len();
        Some(Function {
            name: String::from("main"),
            block,
        })
    }
}
