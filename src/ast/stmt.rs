use crate::ast::expr::Expr;
use crate::ast::ty::Ty;
use crate::context::Context;

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
        let ty = Ty::generate_type(ctx)?;
        let stmt_kind = ctx.choose_stmt_kind();
        let stmt = match stmt_kind {
            StmtKind::Local => {
                // TODO: Decl statements
                let name = ctx.create_var_name();
                let mutable = ctx.choose_mutability();
                let stmt: Stmt = Stmt::Local(LocalStmt::Init(InitLocalStmt {
                    name: name.clone(),
                    ty: ty.clone(),
                    rhs: Expr::generate_expr(ctx, &ty)?,
                    mutable,
                }));
                ctx.type_symbol_table.add_var(name, ty, mutable);
                stmt
            }
            StmtKind::Semi => Stmt::Semi(SemiStmt {
                expr: Expr::generate_expr(ctx, &ty)?,
            }),
            StmtKind::Expr => {
                panic!("Non expression statement cannot be expression")
            }
        };
        *ctx.statistics.stmt_counter.entry(stmt_kind).or_insert(0) += 1;
        Some(stmt)
    }

    pub fn generate_expr_stmt(ctx: &mut Context, res_type: &Ty) -> Option<Stmt> {
        Some(Stmt::Expr(ExprStmt {
            expr: Expr::generate_expr(ctx, res_type)?,
        }))
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
    fn from(expr_stmt: ExprStmt) -> Self {
        Stmt::Expr(expr_stmt)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SemiStmt {
    pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CustomStmt {
    pub stmt: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum StmtKind {
    Local,
    #[allow(dead_code)]
    Expr,
    Semi,
}

#[allow(dead_code)]
pub enum LocalStmtKind {
    Decl,
    Init,
}
