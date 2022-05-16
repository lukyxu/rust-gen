use crate::ast::expr::ExprKind;
use crate::ast::ty::Ty;
use crate::context::Context;

macro_rules! limit_function {
    ($function_name: ident, $curr_depth: ident, $max_depth: ident) => {
        pub fn $function_name<T: 'static>(
            f: Box<dyn FnOnce(&mut Context, &Ty) -> Option<T>>,
        ) -> Box<dyn FnOnce(&mut Context, &Ty) -> Option<T>> {
            Box::new(|ctx, res_type| -> Option<T> {
                if ctx.$curr_depth > ctx.policy.$max_depth {
                    return None;
                }
                ctx.$curr_depth += 1;
                let res = f(ctx, res_type);
                ctx.$curr_depth -= 1;
                res
            })
        }
    };
}

limit_function!(limit_arith_depth, arith_depth, max_arith_depth);
limit_function!(limit_expr_depth, expr_depth, max_expr_depth);
limit_function!(limit_if_else_depth, if_else_depth, max_if_else_depth);
limit_function!(limit_block_depth, block_depth, max_block_depth);

pub fn track_statistics<T: 'static>(
    expr_kind: ExprKind,
    f: Box<dyn FnOnce(&mut Context, &Ty) -> Option<T>>,
) -> Box<dyn FnOnce(&mut Context, &Ty) -> Option<T>> {
    Box::new(move |ctx, res_type| -> Option<T> {
        let res = f(ctx, res_type);
        if res.is_some() {
            *ctx.statistics
                .successful_expr_counter
                .entry(expr_kind)
                .or_insert(0) += 1
        } else {
            *ctx.statistics
                .failed_expr_counter
                .entry(expr_kind)
                .or_insert(0) += 1
        }
        res
    })
}
