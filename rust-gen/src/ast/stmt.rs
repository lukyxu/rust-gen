use crate::ast::expr::Expr;
use crate::ast::ty::Ty;
use crate::generate::eval_expr::EvalExpr;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq)]
#[non_exhaustive]
pub enum Stmt {
    /// Let binding such as `let x: u32 = 5_u32`.
    Local(LocalStmt),
    // Item definition including struct, enums, etc.
    // Item(ItemStmt),
    /// Expr without trailing semi-colon such as `5_u32 + 5_u32`.
    Expr(ExprStmt),
    /// Expr with trailing semi-colon such as `5_u32 + 5_u32;`.
    Semi(SemiStmt),
    /// Other statements that can have custom behaviour such as `println`.
    Custom(CustomStmt), // TODO: Macros and empty statements
}

#[derive(Debug, Clone, PartialEq)]
pub enum LocalStmt {
    /// Local declaration such as `let x;` (not implemented yet).
    #[allow(dead_code)]
    Decl(DeclLocalStmt),
    /// Local declaration with initializer such as `let x = y`.
    Init(InitLocalStmt), // TODO: InitElse
}

impl From<LocalStmt> for Stmt {
    fn from(stmt: LocalStmt) -> Stmt {
        Stmt::Local(stmt)
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

impl From<InitLocalStmt> for LocalStmt {
    fn from(stmt: InitLocalStmt) -> LocalStmt {
        LocalStmt::Init(stmt)
    }
}

impl From<InitLocalStmt> for Stmt {
    fn from(stmt: InitLocalStmt) -> Self {
        Stmt::Local(LocalStmt::Init(stmt))
    }
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

#[derive(Debug, Clone, PartialEq)]
pub struct SemiStmt {
    pub expr: Expr,
}

impl From<SemiStmt> for Stmt {
    fn from(stmt: SemiStmt) -> Stmt {
        Stmt::Semi(stmt)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum CustomStmt {
    Println(PrintlnStmt),
    Assert(AssertStmt),
}

impl From<CustomStmt> for Stmt {
    fn from(stmt: CustomStmt) -> Stmt {
        Stmt::Custom(stmt)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct PrintlnStmt {
    pub format: String,
    pub args: Vec<String>,
}

impl From<PrintlnStmt> for Stmt {
    fn from(stmt: PrintlnStmt) -> Stmt {
        Stmt::Custom(CustomStmt::Println(stmt))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssertStmt {
    pub lhs_expr: Expr,
    pub rhs_expr: Option<EvalExpr>,
}

impl From<AssertStmt> for Stmt {
    fn from(stmt: AssertStmt) -> Stmt {
        Stmt::Custom(CustomStmt::Assert(stmt))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
#[non_exhaustive]
pub enum StmtKind {
    Local,
    Semi,
    Expr,
}

pub enum LocalStmtKind {
    Decl,
    Init,
}
