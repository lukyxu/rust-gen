use crate::ast::expr::ExprKind;
use crate::ast::item::ItemKind;
use crate::ast::stmt::StmtKind;
use crate::ast::ty::{Ty, TyKind};
use crate::context::Context;
use std::cmp::{min};
use std::collections::HashMap;
use std::hash::Hash;

macro_rules! limit_function {
    ($function_name: ident, $curr_depth: ident, $max_depth: ident) => {
        /// Wrapper function that controls various depths.
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

macro_rules! apply_limit_function_composite {
    ($function_name: ident, $max_depth: ident, $curr_depth: ident, $max_depth_delta: ident) => {
        /// Wrapper function that controls various depths.
        pub fn $function_name<T: 'static, S: 'static>(
            f: fn (&mut Context, &S) -> Option<T>,
            ctx: &mut Context,
            res_type: &S
        ) -> Option<T> {
            let prev_max_expr_depth = ctx.policy.$max_depth;
            ctx.policy.$max_depth = min(
                ctx.policy.$max_depth,
                ctx.$curr_depth + ctx.policy.$max_depth_delta,
            );
            let res = f(ctx, res_type);
            ctx.policy.$max_depth = prev_max_expr_depth;
            res
        }
    };
}

apply_limit_function_composite!(apply_limit_expr_depth_in_tuple, max_expr_depth, expr_depth, max_expr_depth_in_tuple);
apply_limit_function_composite!(apply_limit_expr_depth_in_array, max_expr_depth, expr_depth, max_expr_depth_in_array);
apply_limit_function_composite!(apply_limit_expr_depth_in_struct, max_expr_depth, expr_depth, max_expr_depth_in_struct);


/// Increments statistic counters.
pub fn increment_counter<T, K: Eq + Hash, S: ::std::hash::BuildHasher>(
    res: &Option<T>,
    key: K,
    success_counter: &mut HashMap<K, usize, S>,
    failed_counter: &mut HashMap<K, usize, S>,
) {
    if res.is_some() {
        *success_counter.entry(key).or_insert(0) += 1;
    } else {
        *failed_counter.entry(key).or_insert(0) += 1;
    }
}

macro_rules! track_function_with_ty {
    ($function_name: ident, $kind: ident, $success_counter: ident, $failed_counter: ident) => {
        pub fn $function_name<T: 'static>(
            kind: $kind,
            f: Box<dyn FnOnce(&mut Context, &Ty) -> Option<T>>,
        ) -> Box<dyn FnOnce(&mut Context, &Ty) -> Option<T>> {
            Box::new(move |ctx, res_type| -> Option<T> {
                let res = f(ctx, res_type);
                increment_counter(
                    &res,
                    kind,
                    &mut ctx.statistics.$success_counter,
                    &mut ctx.statistics.$failed_counter,
                );
                res
            })
        }
    };
}

macro_rules! track_function {
    ($function_name: ident, $kind: ident, $success_counter: ident, $failed_counter: ident) => {
        /// Adds statistic counter calculations.
        pub fn $function_name<T: 'static>(
            kind: $kind,
            f: Box<dyn FnOnce(&mut Context) -> Option<T>>,
        ) -> Box<dyn FnOnce(&mut Context) -> Option<T>> {
            Box::new(move |ctx| -> Option<T> {
                let res = f(ctx);
                increment_counter(
                    &res,
                    kind,
                    &mut ctx.statistics.$success_counter,
                    &mut ctx.statistics.$failed_counter,
                );
                res
            })
        }
    };
}

track_function_with_ty!(
    track_expr,
    ExprKind,
    successful_expr_counter,
    failed_expr_counter
);
track_function_with_ty!(
    track_stmt,
    StmtKind,
    successful_stmt_counter,
    failed_stmt_counter
);
track_function!(
    track_item,
    ItemKind,
    successful_item_counter,
    failed_item_counter
);
track_function!(track_type, TyKind, successful_ty_counter, failed_ty_counter);
