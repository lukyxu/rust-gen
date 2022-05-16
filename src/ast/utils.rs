use crate::ast::expr::ExprKind;
use crate::ast::item::ItemKind;
use crate::ast::stmt::StmtKind;
use crate::ast::ty::{Ty, TyKind};
use crate::context::Context;
use std::collections::HashMap;
use std::hash::Hash;

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

pub fn increment_counter<T, K: Eq + Hash>(
    res: &Option<T>,
    key: K,
    success_counter: &mut HashMap<K, usize>,
    failed_counter: &mut HashMap<K, usize>,
) {
    if res.is_some() {
        *success_counter.entry(key).or_insert(0) += 1
    } else {
        *failed_counter.entry(key).or_insert(0) += 1
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
