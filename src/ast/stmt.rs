use crate::ast::expr::Expr;
use crate::ast::ty::Ty;
use crate::context::Context;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq)]
#[non_exhaustive]
pub enum Stmt {
    /// Let binding
    Local(LocalStmt),
    // Item definition including struct, enums, etc.
    // Item(ItemStmt),
    /// Expr without trailing semi-colon.
    Expr(ExprStmt),
    /// Expr with trailing semi-colon.
    Semi(SemiStmt),
    Custom(CustomStmt), // TODO: Macros and empty statements
}

impl Stmt {
    pub fn generate_non_expr_stmt(ctx: &mut Context) -> Option<Stmt> {
        let mut res: Option<Stmt> = None;
        let mut num_failed_attempts = 0;
        while res.is_none() && num_failed_attempts < ctx.policy.max_stmt_attempts {
            let ty = Ty::generate_type(ctx)?;
            let stmt_kind = ctx.choose_stmt_kind();
            res = match stmt_kind {
                StmtKind::Local => LocalStmt::generate_stmt(ctx, &ty).map(From::from),
                StmtKind::Semi => SemiStmt::generate_stmt(ctx, &ty).map(From::from),
            };
            if res.is_none() {
                num_failed_attempts += 1;
                *ctx.statistics
                    .failed_stmt_counter
                    .entry(stmt_kind)
                    .or_insert(0) += 1;
            } else {
                *ctx.statistics
                    .successful_stmt_counter
                    .entry(stmt_kind)
                    .or_insert(0) += 1;
            }
        }
        res
    }

    pub fn generate_expr_stmt(ctx: &mut Context, res_type: &Ty) -> Option<Stmt> {
        ExprStmt::generate_stmt(ctx, res_type).map(From::from)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LocalStmt {
    /// Local declaration such as `let x;`
    #[allow(dead_code)]
    Decl(DeclLocalStmt),
    /// Local declaration with initializer such as `let x = y`
    Init(InitLocalStmt), // TODO: InitElse
}

impl From<LocalStmt> for Stmt {
    fn from(stmt: LocalStmt) -> Stmt {
        Stmt::Local(stmt)
    }
}

impl LocalStmt {
    // TODO: Decl statements
    fn generate_stmt(ctx: &mut Context, res_type: &Ty) -> Option<LocalStmt> {
        let name = ctx.create_var_name();
        let mutable = ctx.choose_mutability();

        let res = Some(LocalStmt::Init(InitLocalStmt {
            name: name.clone(),
            ty: res_type.clone(),
            rhs: Expr::fuzz_expr(ctx, res_type)?,
            mutable,
        }));
        ctx.type_symbol_table
            .add_var(name.clone(), res_type.clone(), mutable);
        res
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct DeclLocalStmt {
    pub name: String,
    pub ty: Ty,
}

// TODO: Make type optional/inferred/Non-shown?
#[derive(Debug, Clone, PartialEq)]
pub struct InitLocalStmt {
    pub name: String,
    pub ty: Ty,
    pub rhs: Expr,
    pub mutable: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprStmt {
    pub expr: Expr,
}

impl From<ExprStmt> for Stmt {
    fn from(stmt: ExprStmt) -> Stmt {
        Stmt::Expr(stmt)
    }
}

impl ExprStmt {
    fn generate_stmt(ctx: &mut Context, res_type: &Ty) -> Option<ExprStmt> {
        Some(ExprStmt {
            expr: Expr::fuzz_expr(ctx, res_type)?,
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SemiStmt {
    pub expr: Expr,
}

impl From<SemiStmt> for Stmt {
    fn from(stmt: SemiStmt) -> Stmt {
        Stmt::Semi(stmt)
    }
}

impl SemiStmt {
    fn generate_stmt(ctx: &mut Context, res_type: &Ty) -> Option<SemiStmt> {
        Some(SemiStmt {
            expr: Expr::fuzz_expr(ctx, res_type)?,
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CustomStmt {
    pub stmt: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[non_exhaustive]
pub enum StmtKind {
    Local,
    Semi,
}

#[allow(dead_code)]
pub enum LocalStmtKind {
    Decl,
    Init,
}
