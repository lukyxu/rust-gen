use crate::ast::expr::{AssignExpr, BinaryExpr, BlockExpr, CastExpr, Expr, FieldExpr, IdentExpr, IndexExpr, LitIntExpr, LitIntTy, Member};
use crate::ast::op::BinaryOp;
use crate::ast::stmt::{SemiStmt, Stmt};
use crate::ast::ty::{PrimTy, UIntTy};
use crate::symbol_table::tracked_ty::{OwnershipState, TrackedStructTy, TrackedTy};
use crate::symbol_table::ty::TypeSymbolTable;
use crate::visitor::validation_gen_visitor::{ValidationGen, ValidationGenVisitor};

pub type ChecksumGenVisitor = ValidationGenVisitor<ChecksumGen>;

pub struct ChecksumGen;

impl ValidationGen for ChecksumGen {
    fn add_validation(block_expr: &mut BlockExpr, name: &String, full_type_symbol_table: &TypeSymbolTable, checksum_name: &'static str) {
        if name == checksum_name {
            return
        }
        let ty = full_type_symbol_table.get_var_type(name).unwrap();
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
                        name: checksum_name.to_owned(),
                    }
                        .into(),
                    rhs: Box::new(Expr::Binary(BinaryExpr {
                        lhs: Box::new(Expr::Ident(IdentExpr {
                            name: checksum_name.to_owned(),
                        })),
                        rhs: Box::new(cast_expr),
                        op: BinaryOp::Add,
                    })),
                }),
            });
            block_expr.stmts.insert(block_expr.stmts.len() - 1, stmt);
        }
    }
}

pub fn exprs_from_ident(name: &str, ty: &TrackedTy) -> Vec<Expr> {
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
