use crate::ast::expr::Expr;
use crate::ast::ty::Ty;
use crate::Context;

pub enum Stmt {
    /// Let binding
    Local(LocalStmt),
    // Item definition including struct, enums, etc.
    // Item(ItemStmt),
    /// Expr without trailing semi-colon.
    Expr(ExprStmt),
    /// Expr with trailing semi-colon.
    Semi(SemiStmt),
    // TODO: Macros and empty statements
}

impl Stmt {
    pub fn generate_non_expr_stmt(ctx: &mut Context) -> Stmt {
        let ty = ctx.choose_type();
        match ctx.choose_stmt_kind() {
            StmtKind::Local => {
                // TODO: Decl statements
                Stmt::Local(LocalStmt::Init(InitLocalStmt {
                    name: ctx.create_var_name(),
                    ty: ty.clone(),
                    rhs: Expr::generate_expr(ctx, &ty),
                }))
            }
            StmtKind::Semi => {
                Stmt::Semi(SemiStmt {
                    expr: Expr::generate_expr(ctx, &ty)
                })
            }
            StmtKind::Expr => {
                panic!()
            }
        }
    }

    pub fn generate_expr_stmt(ctx: &mut Context, res_type: &Ty) -> Stmt {
        Stmt::Expr(ExprStmt {
            expr: Expr::generate_expr(ctx, &res_type)
        })
    }
}

pub enum LocalStmt {
    /// Local declaration such as `let x;`
    Decl(DeclLocalStmt),
    /// Local declaration with initializer such as `let x = y`
    Init(InitLocalStmt), // TODO: InitElse
}

pub struct DeclLocalStmt {
    pub name: String,
    pub ty: Ty,
}

// TODO: Make type optional/inferred/Non-shown?
pub struct InitLocalStmt {
    pub name: String,
    pub ty: Ty,
    pub rhs: Expr,
}

pub struct ExprStmt {
    pub expr: Expr,
}

impl From<ExprStmt> for Stmt {
    fn from(expr_stmt: ExprStmt) -> Self {
        Stmt::Expr(expr_stmt)
    }
}

pub struct SemiStmt {
    pub expr: Expr,
}

#[derive(Debug, Clone, Copy)]
pub enum StmtKind {
    Local,
    Expr,
    Semi,
}

pub enum LocalStmtKind {
    Decl,
    Init,
}
