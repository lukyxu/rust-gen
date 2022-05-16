use crate::ast::expr::BlockExpr;
use crate::ast::ty::Ty;
use crate::context::Context;

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: String,
    pub block: BlockExpr,
}

impl Function {
    pub fn fuzz_main_fn(ctx: &mut Context) -> Option<Function> {
        let mut res: Option<Function> = None;
        let mut num_failed_attempts = 0;
        while res.is_none() && num_failed_attempts < ctx.policy.max_main_fn_attempts {
            res = Function::generate_main_fn(ctx);
            if res.is_none() {
                num_failed_attempts += 1;
            }
        }
        res
    }

    pub fn generate_main_fn(ctx: &mut Context) -> Option<Function> {
        let block = BlockExpr::generate_expr(ctx, &Ty::unit_type())?;
        ctx.statistics.main_fn_stmts = block.stmts.len();
        Some(Function {
            name: String::from("main"),
            block,
        })
    }
}
