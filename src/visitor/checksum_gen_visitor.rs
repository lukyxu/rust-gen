use crate::ast::expr::LitIntTy::Unsigned;
use crate::ast::expr::{
    AssignExpr, BinaryExpr, BlockExpr, CastExpr, Expr, FieldExpr, IdentExpr, IndexExpr, LitIntExpr,
    LitIntTy, Member,
};

use crate::ast::function::Function;

use crate::ast::op::BinaryOp;
use crate::ast::stmt::{CustomStmt, InitLocalStmt, LocalStmt, SemiStmt, Stmt};
use crate::ast::ty::{PrimTy, UIntTy};
use crate::symbol_table::tracked_ty::{TrackedStructTy, TrackedTy};
use crate::symbol_table::ty::TypeSymbolTable;
use crate::visitor::base_visitor::Visitor;

/// Visitor used to generate and insert checksum calculations into the program.
pub struct ChecksumGenVisitor {
    add_checksum: bool,
    local_type_symbol_table: TypeSymbolTable,
    prev_local_type_symbol_tables: Vec<TypeSymbolTable>,
    full_type_symbol_table: TypeSymbolTable,
    prev_full_type_symbol_tables: Vec<TypeSymbolTable>,
    checksum_name: &'static str,
}

impl ChecksumGenVisitor {
    pub fn new(add_checksum: bool) -> ChecksumGenVisitor {
        ChecksumGenVisitor {
            add_checksum,
            local_type_symbol_table: TypeSymbolTable::default(),
            prev_local_type_symbol_tables: vec![],
            full_type_symbol_table: TypeSymbolTable::default(),
            prev_full_type_symbol_tables: vec![],
            checksum_name: "checksum",
        }
    }
}

impl Visitor for ChecksumGenVisitor {
    fn enter_scope(&mut self) {
        self.prev_local_type_symbol_tables
            .push(self.local_type_symbol_table.clone());
        self.local_type_symbol_table = TypeSymbolTable::default();
        self.prev_full_type_symbol_tables
            .push(self.full_type_symbol_table.clone());
    }

    fn exit_scope(&mut self) {
        self.local_type_symbol_table = self.prev_local_type_symbol_tables.pop().unwrap();
        // self.local_type_symbol_table
        //     .merge_inplace(&self.full_type_symbol_table);
        // self.full_type_symbol_table = self
        //     .prev_full_type_symbol_tables
        //     .pop()
        //     .unwrap()
        //     .merge(&self.full_type_symbol_table);
    }

    fn visit_ident_expr(&mut self, expr: &mut IdentExpr) {
        // self.full_type_symbol_table.move_var(&expr.name);
        // if self.local_type_symbol_table.contains(&expr.name) {
        //     self.local_type_symbol_table.move_var(&expr.name)
        // }
    }

    fn visit_function(&mut self, function: &mut Function) {
        if function.name != "main" {
            return;
        }
        function.block.stmts.insert(
            0,
            Stmt::Local(LocalStmt::Init(InitLocalStmt {
                name: self.checksum_name.to_owned(),
                ty: UIntTy::U128.into(),
                rhs: LitIntExpr::new(0, Unsigned(UIntTy::U128)).into(),
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
        self.full_type_symbol_table
            .add_var(stmt.name.clone(), stmt.ty.clone(), stmt.mutable);
        self.visit_expr(&mut stmt.rhs);
    }

    fn visit_block_expr(&mut self, expr: &mut BlockExpr) {
        self.enter_scope();
        for stmt in (&mut expr.stmts).split_last_mut().unwrap().1 {
            self.visit_stmt(stmt);
        }
        for (name, ty_mapping) in &self.local_type_symbol_table {
            if name == self.checksum_name || ty_mapping.ty.moveable() {
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
                        place: IdentExpr {
                            name: self.checksum_name.to_owned(),
                        }
                        .into(),
                        rhs: Box::new(Expr::Binary(BinaryExpr {
                            lhs: Box::new(Expr::Ident(IdentExpr {
                                name: self.checksum_name.to_owned(),
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

    fn visit_assign_expr(&mut self, expr: &mut AssignExpr) {
        // TODO: Visit place expression
        self.visit_expr(&mut expr.rhs);
    }
}

fn exprs_from_ident(name: &str, ty: &TrackedTy) -> Vec<Expr> {
    let mut accumulator = vec![];
    match ty {
        TrackedTy::Prim(PrimTy::Int(_) | PrimTy::UInt(_)) => {
            accumulator.push(Expr::Ident(IdentExpr {
                name: name.to_owned(),
            }));
        }
        TrackedTy::Tuple(tuple_ty) => {
            for (i, t) in tuple_ty.into_iter().enumerate() {
                let tuple_access = Expr::Field(FieldExpr {
                    base: Box::new(Expr::Ident(IdentExpr {
                        name: name.to_owned(),
                    })),
                    member: Member::Unnamed(i),
                });
                exprs_from_exprs(tuple_access, t, &mut accumulator);
            }
        }
        TrackedTy::Array(array_ty) => {
            for (i, ty) in array_ty.iter().enumerate() {
                let array_access = Expr::Index(IndexExpr {
                    base: Box::new(Expr::Ident(IdentExpr {
                        name: name.to_owned(),
                    })),
                    index: Box::new(
                        LitIntExpr::new(i as u128, LitIntTy::Unsigned(UIntTy::USize)).into(),
                    ),
                });
                exprs_from_exprs(array_access, &ty, &mut accumulator);
            }
        }
        TrackedTy::Struct(struct_ty) => match struct_ty {
            TrackedStructTy::Field(field_struct) => {
                for field in &field_struct.fields {
                    let tuple_access = Expr::Field(FieldExpr {
                        base: Box::new(Expr::Ident(IdentExpr {
                            name: name.to_owned(),
                        })),
                        member: Member::Named(field.name.clone()),
                    });
                    exprs_from_exprs(tuple_access, &*field.ty, &mut accumulator);
                }
            }
            TrackedStructTy::Tuple(tuple_struct) => {
                for (i, ty) in tuple_struct.fields.tuple.iter().enumerate() {
                    let tuple_access = Expr::Field(FieldExpr {
                        base: Box::new(Expr::Ident(IdentExpr {
                            name: name.to_owned(),
                        })),
                        member: Member::Unnamed(i),
                    });
                    exprs_from_exprs(tuple_access, ty, &mut accumulator);
                }
            }
        },
        _ => {}
    }
    accumulator
}

fn exprs_from_exprs(expr: Expr, ty: &TrackedTy, accumulator: &mut Vec<Expr>) {
    match ty {
        TrackedTy::Prim(PrimTy::Int(_) | PrimTy::UInt(_)) => accumulator.push(expr),
        TrackedTy::Tuple(tuple_ty) => {
            for (i, ty) in tuple_ty.into_iter().enumerate() {
                let tuple_access = Expr::Field(FieldExpr {
                    base: Box::new(expr.clone()),
                    member: Member::Unnamed(i),
                });
                exprs_from_exprs(tuple_access, ty, accumulator);
            }
        }
        TrackedTy::Array(array_ty) => {
            for (i, ty) in array_ty.iter().enumerate() {
                let array_access = Expr::Index(IndexExpr {
                    base: Box::new(expr.clone()),
                    index: Box::new(
                        LitIntExpr::new(i as u128, LitIntTy::Unsigned(UIntTy::USize)).into(),
                    ),
                });
                exprs_from_exprs(array_access, &ty, accumulator);
            }
        }
        TrackedTy::Struct(struct_ty) => match struct_ty {
            TrackedStructTy::Field(field_struct) => {
                for field in &field_struct.fields {
                    let tuple_access = Expr::Field(FieldExpr {
                        base: Box::new(expr.clone()),
                        member: Member::Named(field.name.clone()),
                    });
                    exprs_from_exprs(tuple_access, &*field.ty, accumulator);
                }
            }
            TrackedStructTy::Tuple(tuple_struct) => {
                for (i, ty) in tuple_struct.fields.tuple.iter().enumerate() {
                    let tuple_access = Expr::Field(FieldExpr {
                        base: Box::new(expr.clone()),
                        member: Member::Unnamed(i),
                    });
                    exprs_from_exprs(tuple_access, ty, accumulator);
                }
            }
        },
        _ => {}
    }
}
