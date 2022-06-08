use crate::ast::expr::{AssignExpr, BinaryExpr, BlockExpr, CastExpr, Expr, FieldExpr, IdentExpr, IndexExpr, LitIntExpr, LitIntTy, Member};
use crate::ast::op::BinaryOp;
use crate::ast::stmt::{AssertStmt, CustomStmt, SemiStmt, Stmt};
use crate::ast::ty::{PrimTy, UIntTy};
use crate::symbol_table::tracked_ty::{OwnershipState, TrackedStructTy, TrackedTy};
use crate::symbol_table::ty::TypeSymbolTable;
use crate::visitor::validation_gen_visitor::{ValidationGen, ValidationGenVisitor};

pub type AssertGenVisitor = ValidationGenVisitor<AssertGen>;

pub struct AssertGen;

impl ValidationGen for AssertGen {
    fn add_validation(block_expr: &mut BlockExpr, name: &String, full_type_symbol_table: &TypeSymbolTable, _checksum_name: &'static str) {
        let ty = full_type_symbol_table.get_var_type(name).unwrap();
        let exprs = exprs_from_ident(name, &ty);
        for expr in exprs {
            let stmt = Stmt::Custom(CustomStmt::Assert(
                AssertStmt {
                    lhs_expr: expr.clone(),
                    rhs_expr: None,
                }
            ));
            block_expr.stmts.insert(block_expr.stmts.len() - 1, stmt);
        }
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
