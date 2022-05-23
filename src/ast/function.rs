use crate::ast::expr::BlockExpr;
use crate::ast::ty::{Ty};
use crate::context::Context;

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: String,
    pub block: BlockExpr,
}

impl Function {
    /// Attempts multiple times given by `ctx.policy.max_main_fn_attempts` to generate a valid main function.
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

    /// Attempts a single attempt to generate a valid main function.
    pub fn generate_main_fn(ctx: &mut Context) -> Option<Function> {
        let block = BlockExpr::generate_expr(ctx, &Ty::unit_type())?;
        ctx.statistics.main_fn_stmts = block.stmts.len();
        Some(Function {
            name: String::from("main"),
            block,
        })
    }

    pub fn generate_fn(ctx: &mut Context) -> Option<Function> {
        let block = BlockExpr::generate_expr(ctx, &Ty::unit_type())?;
        ctx.statistics.main_fn_stmts = block.stmts.len();
        Some(Function {
            name: ctx.create_function_name(),
            block,
        })
    }
}
