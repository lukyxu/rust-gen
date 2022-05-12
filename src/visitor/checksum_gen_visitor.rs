use crate::ast::expr::LitIntTy::Unsigned;
use crate::ast::expr::{
    AssignExpr, BinaryExpr, BlockExpr, CastExpr, Expr, FieldExpr, IdentExpr, IndexExpr, LitExpr,
    LitIntTy, Member,
};
use crate::ast::function::Function;
use crate::ast::op::BinaryOp;
use crate::ast::stmt::{CustomStmt, InitLocalStmt, LocalStmt, SemiStmt, Stmt};
use crate::ast::ty::{PrimTy, Ty, UIntTy};
use crate::symbol_table::ty::TypeSymbolTable;
use crate::visitor::base_visitor::Visitor;

pub struct ChecksumGenVisitor {
    add_checksum: bool,
    local_type_symbol_table: TypeSymbolTable,
    prev_local_type_symbol_tables: Vec<TypeSymbolTable>,
    checksum_name: &'static str,
}

impl ChecksumGenVisitor {
    pub fn new(add_checksum: bool) -> ChecksumGenVisitor {
        ChecksumGenVisitor {
            add_checksum,
            local_type_symbol_table: TypeSymbolTable::default(),
            prev_local_type_symbol_tables: vec![],
            checksum_name: "checksum",
        }
    }
}

impl Visitor for ChecksumGenVisitor {
    fn enter_scope(&mut self) {
        self.prev_local_type_symbol_tables
            .push(self.local_type_symbol_table.clone());
        self.local_type_symbol_table = TypeSymbolTable::default();
    }

    fn exit_scope(&mut self) {
        self.local_type_symbol_table = self.prev_local_type_symbol_tables.pop().unwrap();
    }

    fn visit_function(&mut self, function: &mut Function) {
        function.block.stmts.insert(
            0,
            Stmt::Local(LocalStmt::Init(InitLocalStmt {
                name: self.checksum_name.to_owned(),
                ty: UIntTy::U128.into(),
                rhs: Expr::Literal(LitExpr::Int(0, Unsigned(UIntTy::U128))),
                mutable: true,
            })),
        );
        function.block.stmts.push(Stmt::Custom(CustomStmt {
            stmt: format!("println!(\"{{}}\", {})", self.checksum_name),
        }));
        if self.add_checksum {
            self.visit_block_expr(&mut function.block);
        }
    }

    fn visit_local_init_stmt(&mut self, stmt: &mut InitLocalStmt) {
        self.local_type_symbol_table
            .add_var(stmt.name.clone(), stmt.ty.clone(), stmt.mutable);
        self.visit_expr(&mut stmt.rhs);
    }

    fn visit_block_expr(&mut self, expr: &mut BlockExpr) {
        self.enter_scope();
        for stmt in (&mut expr.stmts).split_last_mut().unwrap().1 {
            self.visit_stmt(stmt);
        }
        for (name, ty_mapping) in &self.local_type_symbol_table {
            if name == self.checksum_name {
                continue;
            }
            let exprs = exprs_from_ident(name, &ty_mapping.ty);
            let cast_exprs: Vec<Expr> = exprs
                .into_iter()
                .map(|expr| {
                    Expr::Cast(CastExpr {
                        expr: Box::new(expr),
                        ty: UIntTy::U128.into(),
                    })
                })
                .collect();
            for cast_expr in cast_exprs {
                let stmt = Stmt::Semi(SemiStmt {
                    expr: Expr::Assign(AssignExpr {
                        name: self.checksum_name.to_owned(),
                        rhs: Box::new(Expr::Binary(BinaryExpr {
                            lhs: Box::new(Expr::Ident(IdentExpr {
                                name: self.checksum_name.to_owned(),
                                ty: UIntTy::U128.into(),
                            })),
                            rhs: Box::new(cast_expr),
                            op: BinaryOp::Add,
                        })),
                    }),
                });
                expr.stmts.insert(expr.stmts.len() - 1, stmt);
            }
        }
        self.visit_stmt((&mut expr.stmts).last_mut().unwrap());
        self.exit_scope();
    }
}

fn exprs_from_ident(name: &str, ty: &Ty) -> Vec<Expr> {
    let mut accumulator = vec![];
    match ty {
        Ty::Prim(PrimTy::Int(_) | PrimTy::UInt(_)) => {
            accumulator.push(Expr::Ident(IdentExpr {
                name: name.to_owned(),
                ty: ty.clone(),
            }));
        }
        Ty::Tuple(tuple_ty) => {
            for (i, t) in tuple_ty.into_iter().enumerate() {
                let tuple_access = Expr::Field(FieldExpr {
                    base: Box::new(Expr::Ident(IdentExpr {
                        name: name.to_owned(),
                        ty: ty.clone(),
                    })),
                    member: Member::Unnamed(i),
                });
                exprs_from_exprs(tuple_access, t, &mut accumulator);
            }
        }
        Ty::Array(array_ty) => {
            for (i, ty) in array_ty.iter().enumerate() {
                let array_access = Expr::Index(IndexExpr {
                    base: Box::new(Expr::Ident(IdentExpr {
                        name: name.to_owned(),
                        ty: ty.clone(),
                    })),
                    index: Box::new(Expr::Literal(LitExpr::Int(
                        i as u128,
                        LitIntTy::Unsigned(UIntTy::USize),
                    ))),
                });
                exprs_from_exprs(array_access, &ty, &mut accumulator);
            }
        }
        _ => {}
    }
    accumulator
}

fn exprs_from_exprs(expr: Expr, ty: &Ty, accumulator: &mut Vec<Expr>) {
    match ty {
        Ty::Prim(PrimTy::Int(_) | PrimTy::UInt(_)) => accumulator.push(expr),
        Ty::Tuple(tuple_ty) => {
            for (i, ty) in tuple_ty.into_iter().enumerate() {
                let tuple_access = Expr::Field(FieldExpr {
                    base: Box::new(expr.clone()),
                    member: Member::Unnamed(i),
                });
                exprs_from_exprs(tuple_access, ty, accumulator);
            }
        }
        Ty::Array(array_ty) => {
            for (i, ty) in array_ty.iter().enumerate() {
                let array_access = Expr::Index(IndexExpr {
                    base: Box::new(expr.clone()),
                    index: Box::new(Expr::Literal(LitExpr::Int(
                        i as u128,
                        LitIntTy::Unsigned(UIntTy::USize),
                    ))),
                });
                exprs_from_exprs(array_access, &ty, accumulator);
            }
        }
        _ => {}
    }
}
