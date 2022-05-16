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

macro_rules! track_function {
    ($function_name: ident, $kind: ident, $success_counter: ident, $failed_counter: ident) => {
        pub fn $function_name<T: 'static>(
            kind: $kind,
            f: Box<dyn FnOnce(&mut Context, &Ty) -> Option<T>>,
        ) -> Box<dyn FnOnce(&mut Context, &Ty) -> Option<T>> {
            Box::new(move |ctx, res_type| -> Option<T> {
                let res = f(ctx, res_type);
                if res.is_some() {
                    *ctx.statistics
                        .$success_counter
                        .entry(kind)
                        .or_insert(0) += 1
                } else {
                    *ctx.statistics
                        .$failed_counter
                        .entry(kind)
                        .or_insert(0) += 1
                }
                res
            })
        }
    };
}

track_function!(track_expr, ExprKind, successful_expr_counter, failed_expr_counter);