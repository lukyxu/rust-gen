use crate::ast::expr::Expr;
use crate::ast::stmt::{ExprStmt, InitLocalStmt, LocalStmt, SemiStmt, Stmt, StmtKind};
use crate::ast::ty::Ty;
use crate::context::Context;
use crate::generate::utils::{revert_ctx_on_failure, track_stmt};
use std::cmp::max;

impl Stmt {
    /// Attempts multiple times given by `ctx.policy.max_stmt_attempts` to generate a valid non expression statement (local and semi expressions).
    pub fn fuzz_non_expr_stmt(ctx: &mut Context) -> Option<Stmt> {
        let mut res: Option<Stmt> = None;
        let mut num_failed_attempts = 0;
        while res.is_none() && num_failed_attempts < ctx.policy.max_stmt_attempts {
            res = Stmt::generate_non_expr_stmt(ctx);
            if res.is_none() {
                num_failed_attempts += 1;
                ctx.statistics.max_failed_stmt_depth =
                    max(ctx.statistics.max_failed_stmt_depth, num_failed_attempts);
            }
        }
        res
    }

    /// Attempts a single attempt to generate a valid non expression statement (local and semi expressions).
    pub fn generate_non_expr_stmt(ctx: &mut Context) -> Option<Stmt> {
        let res_type = &Ty::generate_type(ctx)?;
        let stmt_kind = ctx.choose_stmt_kind();
        match stmt_kind {
            StmtKind::Local => LocalStmt::generate_stmt(ctx, res_type).map(From::from),
            StmtKind::Semi => SemiStmt::generate_stmt(ctx, res_type).map(From::from),
            StmtKind::Expr => panic!("Cannot generate expr stmt using generate_non_expr_stmt"),
            StmtKind::Custom => panic!("Cannot generate custom stmt using generate_non_expr_stmt"),
        }
    }

    /// Attempts multiple times given by `ctx.policy.max_stmt_attempts` to generate a valid expression statement.
    pub fn fuzz_expr_stmt(ctx: &mut Context, res_type: &Ty) -> Option<Stmt> {
        let mut res: Option<Stmt> = None;
        let mut num_failed_attempts = 0;
        while res.is_none() && num_failed_attempts < ctx.policy.max_stmt_attempts {
            res = Stmt::generate_expr_stmt(ctx, res_type);
            if res.is_none() {
                num_failed_attempts += 1;
            }
        }
        res
    }

    /// Attempts a single attempt to generate a valid expression statement.
    pub fn generate_expr_stmt(ctx: &mut Context, res_type: &Ty) -> Option<Stmt> {
        ExprStmt::generate_stmt(ctx, res_type).map(From::from)
    }
}

impl LocalStmt {
    // TODO: Decl statements
    pub fn generate_stmt(ctx: &mut Context, res_type: &Ty) -> Option<LocalStmt> {
        track_stmt(
            StmtKind::Local,
            revert_ctx_on_failure(Box::new(LocalStmt::generate_stmt_internal)),
        )(ctx, res_type)
    }

    fn generate_stmt_internal(ctx: &mut Context, res_type: &Ty) -> Option<LocalStmt> {
        let name = ctx.create_var_name();
        let mutable = ctx.choose_mutability();

        let res = Some(LocalStmt::Init(InitLocalStmt {
            name: name.clone(),
            ty: res_type.clone(),
            rhs: Expr::fuzz_move_expr(ctx, res_type)?,
            mutable,
        }));
        ctx.type_symbol_table.add_var(name, res_type, mutable);
        if !ctx.generable_ident_type_map.contains(res_type) {
            ctx.generable_ident_type_map = ctx.generable_ident_type_map.insert(res_type.clone());
        }
        res
    }
}

impl ExprStmt {
    pub fn generate_stmt(ctx: &mut Context, res_type: &Ty) -> Option<ExprStmt> {
        track_stmt(StmtKind::Expr, Box::new(ExprStmt::generate_stmt_internal))(ctx, res_type)
    }

    fn generate_stmt_internal(ctx: &mut Context, res_type: &Ty) -> Option<ExprStmt> {
        Some(ExprStmt {
            expr: Expr::fuzz_move_expr(ctx, res_type)?,
        })
    }
}

impl SemiStmt {
    pub fn generate_stmt(ctx: &mut Context, res_type: &Ty) -> Option<SemiStmt> {
        track_stmt(
            StmtKind::Semi,
            revert_ctx_on_failure(Box::new(SemiStmt::generate_stmt_internal)),
        )(ctx, res_type)
    }

    fn generate_stmt_internal(ctx: &mut Context, res_type: &Ty) -> Option<SemiStmt> {
        Some(SemiStmt {
            expr: Expr::fuzz_move_expr(ctx, res_type)?,
        })
    }
}
