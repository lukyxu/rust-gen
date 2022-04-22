use crate::ast::expr::Expr;
use crate::ast::ty::Ty;
use crate::Context;

#[derive(Debug, Clone)]
pub enum Stmt<'ir> {
    /// Let binding
    Local(&'ir LocalStmt<'ir>),
    // Item definition including struct, enums, etc.
    // Item(ItemStmt),
    /// Expr without trailing semi-colon.
    Expr(&'ir ExprStmt<'ir>),
    /// Expr with trailing semi-colon.
    Semi(&'ir SemiStmt<'ir>),
    // TODO: Macros and empty statements
}

impl <'ir> Stmt<'ir> {
    pub fn generate_non_expr_stmt(ctx: &mut Context) -> Stmt {
        let ty = ctx.choose_type();
        match ctx.choose_stmt_kind() {
            StmtKind::Local => {
                // TODO: Decl statements
                let name = ctx.create_var_name();
                let mutable = ctx.choose_mutability();
                let stmt: Stmt = Stmt::Local(&LocalStmt::Init(&InitLocalStmt {
                    name: name.clone(),
                    ty: ty.clone(),
                    rhs: &Expr::generate_expr_safe(ctx, &ty),
                    mutable,
                }));
                ctx.type_symbol_table.add_var(name, ty, mutable);
                stmt
            }
            StmtKind::Semi => Stmt::Semi(&SemiStmt {
                expr: &Expr::generate_expr_safe(ctx, &ty),
            }),
            StmtKind::Expr => {
                panic!("Non expression statement cannot be expression")
            }
        }
    }

    pub fn generate_expr_stmt(ctx: &mut Context, res_type: &Ty) -> Stmt<'ir> {
        Stmt::Expr(&ExprStmt {
            expr: &Expr::generate_expr_safe(ctx, &res_type),
        })
    }
}

#[derive(Debug, Clone)]
pub enum LocalStmt<'ir> {
    /// Local declaration such as `let x;`
    Decl(&'ir DeclLocalStmt),
    /// Local declaration with initializer such as `let x = y`
    Init(&'ir InitLocalStmt<'ir>), // TODO: InitElse
}

#[derive(Debug, Clone)]
pub struct DeclLocalStmt {
    pub name: String,
    pub ty: Ty,
}

// TODO: Make type optional/inferred/Non-shown?
#[derive(Debug, Clone)]
pub struct InitLocalStmt<'ir> {
    pub name: String,
    pub ty: Ty,
    pub rhs: &'ir Expr<'ir>,
    pub mutable: bool,
}

#[derive(Debug, Clone)]
pub struct ExprStmt<'ir> {
    pub expr: &'ir Expr<'ir>,
}

impl <'ir> From<&'ir ExprStmt<'ir>> for Stmt<'ir> {
    fn from(expr_stmt: &'ir ExprStmt<'ir>) -> Self {
        Stmt::Expr(expr_stmt)
    }
}

#[derive(Debug, Clone)]
pub struct SemiStmt<'ir> {
    pub expr: &'ir Expr<'ir>,
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
