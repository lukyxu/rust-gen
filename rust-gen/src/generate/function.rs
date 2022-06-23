use crate::ast::expr::BlockExpr;
use crate::ast::function::Function;
use crate::ast::ty::Ty;
use crate::context::Context;

impl Function {
    /// Attempts multiple times given by `ctx.policy.max_main_fn_attempts` to generate a valid main function.
    pub fn fuzz_main_fn(ctx: &mut Context) -> Option<Function> {
        let mut res: Option<Function> = None;
        let mut num_failed_attempts = 0;
        while res.is_none() && num_failed_attempts < ctx.policy.max_fn_attempts {
            res = Function::generate_main_fn(ctx);
            if res.is_none() {
                num_failed_attempts += 1;
            }
        }
        res
    }

    /// Attempts a single attempt to generate a valid main function.
    pub fn generate_main_fn(ctx: &mut Context) -> Option<Function> {
        let res_ty = Ty::unit_type();
        let block = BlockExpr::generate_expr_internal(ctx, &res_ty)?;
        Some(Function {
            name: String::from("main"),
            return_ty: res_ty,
            block,
        })
    }

    pub fn generate_fn(ctx: &mut Context) -> Option<Function> {
        let res_ty = Ty::fuzz_type(ctx)?;
        let block = BlockExpr::generate_expr_internal(ctx, &res_ty)?;
        let fn_name = ctx.create_function_name();
        ctx.function_symbol_table.add_var(fn_name.clone(), &res_ty, true);
        if !ctx.generable_function_call_type_map.contains(&res_ty) {
            ctx.generable_function_call_type_map = ctx.generable_function_call_type_map.insert(res_ty.clone());
        }
        Some(Function {
            name: fn_name,
            return_ty: res_ty,
            block,
        })
    }
}
