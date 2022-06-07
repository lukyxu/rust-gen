use crate::ast::expr::LitIntTy::Unsigned;
use crate::ast::expr::{
    AssignExpr, BinaryExpr, BlockExpr, CastExpr, Expr, FieldExpr, IdentExpr, IfExpr, IndexExpr,
    LitIntExpr, LitIntTy, Member, PlaceExpr,
};

use std::collections::BTreeSet;

use crate::ast::function::Function;

use crate::ast::op::BinaryOp;
use crate::ast::stmt::{CustomStmt, InitLocalStmt, LocalStmt, SemiStmt, Stmt};
use crate::ast::ty::{PrimTy, UIntTy};
use crate::symbol_table::tracked_ty::{OwnershipState, TrackedStructTy, TrackedTy};
use crate::symbol_table::ty::TypeSymbolTable;
use crate::visitor::base_visitor::Visitor;

type LocalTypeSymbolTable = BTreeSet<String>;

/// Visitor used to generate and insert checksum calculations into the program.
pub struct ChecksumGenVisitor {
    add_checksum: bool,
    local_type_symbol_table: LocalTypeSymbolTable,
    prev_local_type_symbol_tables: Vec<LocalTypeSymbolTable>,
    full_type_symbol_table: TypeSymbolTable,
    prev_full_type_symbol_tables: Vec<TypeSymbolTable>,
    checksum_name: &'static str,
}

impl ChecksumGenVisitor {
    pub fn new(add_checksum: bool) -> ChecksumGenVisitor {
        ChecksumGenVisitor {
            add_checksum,
            local_type_symbol_table: LocalTypeSymbolTable::new(),
            prev_local_type_symbol_tables: vec![],
            full_type_symbol_table: TypeSymbolTable::default(),
            prev_full_type_symbol_tables: vec![],
            checksum_name: "checksum",
        }
    }
}

impl ChecksumGenVisitor {
    fn visit_field_place(&mut self, expr: &mut Expr) {
        match expr {
            Expr::Field(expr) => self.visit_field_place(&mut expr.base),
            _ => self.visit_non_move_expr(expr),
        }
    }

    fn visit_non_move_expr(&mut self, expr: &mut Expr) {
        match expr {
            Expr::Literal(literal_expr) => self.visit_literal_expr(literal_expr),
            Expr::Binary(binary_expr) => self.visit_binary_expr(binary_expr),
            Expr::Unary(unary_expr) => self.visit_unary_expr(unary_expr),
            Expr::Cast(cast_expr) => self.visit_cast_expr(cast_expr),
            Expr::If(if_expr) => self.visit_if_expr(if_expr),
            Expr::Block(block_expr) => self.visit_block_expr(block_expr),
            Expr::Ident(ident_expr) => self.visit_ident_expr(ident_expr),
            Expr::Tuple(tuple_expr) => self.visit_tuple_expr(tuple_expr),
            Expr::Assign(assign_expr) => self.visit_assign_expr(assign_expr),
            Expr::Array(array_expr) => self.visit_array_expr(array_expr),
            Expr::Field(expr) => self.visit_non_move_expr(&mut expr.base),
            Expr::Index(index_expr) => self.visit_index_expr(index_expr),
            Expr::Struct(struct_expr) => self.visit_struct_expr(struct_expr),
            Expr::Reference(reference_expr) => self.visit_reference_expr(reference_expr),
        }
    }

    fn visit_block_internal(&mut self, expr: &mut BlockExpr) -> TypeSymbolTable {
        self.enter_scope();
        for stmt in (&mut expr.stmts).split_last_mut().unwrap().1 {
            self.visit_stmt(stmt);
        }
        for name in &self.local_type_symbol_table {
            if name == self.checksum_name {
                continue;
            }
            let ty = self.full_type_symbol_table.get_var_type(name).unwrap();
            let exprs = exprs_from_ident(name, &ty);
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
        let res = self.full_type_symbol_table.clone();
        self.exit_scope();
        res
    }
}

impl Visitor for ChecksumGenVisitor {
    fn enter_scope(&mut self) {
        self.prev_local_type_symbol_tables
            .push(self.local_type_symbol_table.clone());
        self.local_type_symbol_table = LocalTypeSymbolTable::new();
        self.prev_full_type_symbol_tables
            .push(self.full_type_symbol_table.clone());
    }

    fn exit_scope(&mut self) {
        self.local_type_symbol_table = self.prev_local_type_symbol_tables.pop().unwrap();
        self.full_type_symbol_table = self.prev_full_type_symbol_tables.pop().unwrap();
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
        self.local_type_symbol_table.insert(stmt.name.clone());
        self.full_type_symbol_table
            .add_var(stmt.name.clone(), &stmt.ty, stmt.mutable);
        self.visit_expr(&mut stmt.rhs);
    }

    // fuzz_move_expr
    fn visit_expr(&mut self, expr: &mut Expr) {
        self.visit_non_move_expr(expr);
        assert!(
            self.full_type_symbol_table.move_expr(expr),
            "Expr {:?} already moved",
            &expr
        );
    }

    fn visit_place_expr(&mut self, expr: &mut PlaceExpr) {
        match expr {
            PlaceExpr::Field(expr) => self.visit_field_place(&mut expr.base),
            PlaceExpr::Ident(_expr) => {}
            PlaceExpr::Index(expr) => self.visit_index_expr(expr),
        }
        self.full_type_symbol_table.regain_ownership(expr);
    }

    fn visit_binary_expr(&mut self, expr: &mut BinaryExpr) {
        self.visit_expr(&mut expr.lhs);
        if expr.op.can_short_circuit() {
            // Statements that can short circuit might not evaluate rhs moves
            let mut symbol_table = self.full_type_symbol_table.clone();
            self.visit_expr(&mut expr.rhs);
            std::mem::swap(&mut symbol_table, &mut self.full_type_symbol_table);
            self.full_type_symbol_table
                .update_branch(&symbol_table, &None);
        } else {
            self.visit_expr(&mut expr.rhs);
        }
    }

    fn visit_if_expr(&mut self, expr: &mut IfExpr) {
        self.visit_expr(&mut expr.condition);
        let then_sym_t = self.visit_block_internal(&mut expr.then);
        let false_sym_t = expr
            .otherwise
            .as_mut()
            .map(|otherwise| self.visit_block_internal(otherwise));
        self.full_type_symbol_table
            .update_branch(&then_sym_t, &false_sym_t);
    }

    fn visit_block_expr(&mut self, expr: &mut BlockExpr) {
        let sym_table = self.visit_block_internal(expr);
        self.full_type_symbol_table.update(&sym_table);
    }

    fn visit_assign_expr(&mut self, expr: &mut AssignExpr) {
        self.visit_expr(&mut expr.rhs);
        self.visit_place_expr(&mut expr.place);
    }
}

fn exprs_from_ident(name: &str, ty: &TrackedTy) -> Vec<Expr> {
    let mut accumulator = vec![];
    if ty.ownership_state() == OwnershipState::Moved {
        return vec![];
    }
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
    if ty.ownership_state() == OwnershipState::Moved {
        return;
    }
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
