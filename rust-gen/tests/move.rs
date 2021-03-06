use rust_gen::ast::expr::{
    AssignExpr, BlockExpr, Expr, FieldExpr, IdentExpr, IfExpr, LitExpr, Member, PlaceExpr,
    TupleExpr, TupleStructExpr,
};
use rust_gen::ast::file::RustFile;
use rust_gen::ast::function::Function;
use rust_gen::ast::item::{FunctionItem, Item, StructItem};
use rust_gen::ast::stmt::{InitLocalStmt, SemiStmt, Stmt};
use rust_gen::ast::ty::{StructTy, TupleStructTy, TupleTy, Ty, UIntTy};
use rust_gen::visitor::base_visitor::Visitor;
use rust_gen::visitor::checksum_gen_visitor::ChecksumGenVisitor;
use rust_gen::visitor::emit_visitor::EmitVisitor;

struct StructTemplate(StructTy, StructTy, RustFile);

// #[derive(PartialEq, Clone)]
// struct Struct1(u32);
//
// #[derive(PartialEq, Clone)]
// struct Struct2(Struct1, Struct1);
//
// fn main() {
//     let s2: Struct2 = Struct2(Struct1(1_u32,), Struct1(2_u32,));
// }

impl Default for StructTemplate {
    fn default() -> Self {
        StructTemplate::new(false)
    }
}

impl StructTemplate {
    fn new(mutable_struct: bool) -> StructTemplate {
        let struct1 = StructTy::Tuple(TupleStructTy {
            name: "Struct1".to_string(),
            is_copy: false,
            is_clone: true,
            fields: TupleTy::new(vec![UIntTy::U32.into()]),
            lifetimes: Default::default(),
            assoc: (),
        });
        let struct2 = StructTy::Tuple(TupleStructTy {
            name: "Struct2".to_string(),
            is_copy: false,
            is_clone: true,
            fields: TupleTy::new(vec![struct1.clone().into(), struct1.clone().into()]),
            lifetimes: Default::default(),
            assoc: (),
        });
        let file = RustFile {
            items: vec![
                StructItem {
                    struct_ty: struct1.clone(),
                }
                .into(),
                StructItem {
                    struct_ty: struct2.clone(),
                }
                .into(),
                FunctionItem {
                    function: Function {
                        name: "main".to_string(),
                        return_ty: Ty::unit_type(),
                        block: BlockExpr {
                            stmts: vec![
                                // let s2 = Struct2(Struct1(1), (Struct1(2)));
                                InitLocalStmt {
                                    name: "s2".to_string(),
                                    ty: struct2.clone().into(),
                                    rhs: StructTemplate::struct2_expr(1, 2),
                                    mutable: mutable_struct,
                                }
                                .into(),
                            ],
                        },
                    },
                }
                .into(),
            ],
        };
        StructTemplate(struct1, struct2, file)
    }

    fn add_stmt(&mut self, stmt: Stmt) {
        match &mut self.2.items[2] {
            Item::Function(item) => item.function.block.stmts.push(stmt),
            _ => panic!(),
        }
    }

    fn struct1_expr(field: u32) -> Expr {
        TupleStructExpr {
            struct_name: "Struct1".to_string(),
            fields: TupleExpr {
                tuple: vec![Expr::u32(field)],
            },
        }
        .into()
    }

    fn struct2_expr(field_1: u32, field_2: u32) -> Expr {
        TupleStructExpr {
            struct_name: "Struct2".to_string(),
            fields: TupleExpr {
                tuple: vec![
                    StructTemplate::struct1_expr(field_1),
                    StructTemplate::struct1_expr(field_2),
                ],
            },
        }
        .into()
    }

    fn struct_ident_stmt(name: &str) -> Stmt {
        SemiStmt {
            expr: IdentExpr {
                name: name.to_string(),
            }
            .into(),
        }
        .into()
    }

    fn struct_2_assign_stmt(name: &str, field_1: u32, field_2: u32) -> Stmt {
        SemiStmt {
            expr: AssignExpr {
                place: PlaceExpr::Ident(IdentExpr {
                    name: name.to_string(),
                }),
                rhs: Box::new(StructTemplate::struct2_expr(field_1, field_2)),
            }
            .into(),
        }
        .into()
    }

    fn struct_2_partial_assign_stmt(name: &str, field: usize, value: u32) -> Stmt {
        SemiStmt {
            expr: AssignExpr {
                place: StructTemplate::tuple_field_expr(name, field).into(),
                rhs: Box::new(StructTemplate::struct1_expr(value)),
            }.into(),
        }
            .into()
    }

    fn block_stmt(stmts: Vec<Stmt>) -> Stmt {
        SemiStmt {
            expr: BlockExpr { stmts }.into(),
        }
        .into()
    }

    fn if_else_stmt(cond: bool, then: Vec<Stmt>, otherwise: Option<Vec<Stmt>>) -> Stmt {
        SemiStmt {
            expr: IfExpr {
                condition: Box::new(LitExpr::Bool(cond).into()),
                then: Box::new(BlockExpr { stmts: then }),
                otherwise: otherwise.map(|otherwise| Box::new(BlockExpr { stmts: otherwise })),
            }
            .into(),
        }
        .into()
    }

    fn tuple_field_expr(name: &str, index: usize) -> FieldExpr {
        FieldExpr {
            base: Box::new(
                IdentExpr {
                    name: name.to_string(),
                }
                .into(),
            ),
            member: Member::Unnamed(index),
        }
        .into()
    }

    fn tuple_field_stmt(name: &str, index: usize) -> Stmt {
        Stmt::Semi(SemiStmt {expr: StructTemplate::tuple_field_expr(name, index).into()})
    }
}

fn generate_checksum_and_emit(file: &mut RustFile) -> String {
    let mut checksum_gen_visitor = ChecksumGenVisitor::new(true, true, false);
    checksum_gen_visitor.visit_file(file);
    let mut emit_visitor = EmitVisitor::default();
    emit_visitor.visit_file(file);
    let output = emit_visitor.output();
    println!("{}", output);
    output
}

// #[derive(PartialEq, Clone)]
// struct Struct1(u32);
//
// #[derive(PartialEq, Clone)]
// struct Struct2(Struct1, Struct1);
//
// fn main() {
//     let s2: Struct2 = Struct2(Struct1(1_u32,), Struct1(2_u32,));
// }

#[test]
fn default_struct() {
    let mut template = StructTemplate::default();
    let output = generate_checksum_and_emit(&mut template.2);
    assert!(output.contains("checksum = (checksum + (((s2.0).0) as u128));"));
    assert!(output.contains("checksum = (checksum + (((s2.1).0) as u128));"));
}

// #[derive(PartialEq, Clone)]
// struct Struct1(u32);
//
// #[derive(PartialEq, Clone)]
// struct Struct2(Struct1, Struct1);
//
// fn main() {
//     let s2: Struct2 = Struct2(Struct1(1_u32,), Struct1(2_u32,));
//     s2;
// }

#[test]
fn struct_move() {
    let mut template = StructTemplate::default();
    template.add_stmt(StructTemplate::struct_ident_stmt("s2"));
    let output = generate_checksum_and_emit(&mut template.2);
    assert!(!output.contains("checksum = (checksum + (((s2.0).0) as u128));"));
    assert!(!output.contains("checksum = (checksum + (((s2.1).0) as u128));"));
}

#[derive(PartialEq, Clone)]
struct Struct1(u32);

#[derive(PartialEq, Clone)]
struct Struct2(Struct1, Struct1);

// fn main() {
//     let mut s2: Struct2 = Struct2(Struct1(1_u32,), Struct1(2_u32,));
//     s2;
//     s2 = Struct2(Struct1(3_u32,), Struct1(4_u32,));
// }

#[test]
fn struct_move_reassign() {
    let mut template = StructTemplate::new(true);
    template.add_stmt(StructTemplate::struct_ident_stmt("s2"));
    template.add_stmt(StructTemplate::struct_2_assign_stmt("s2", 3, 4));
    let output = generate_checksum_and_emit(&mut template.2);
    assert!(output.contains("checksum = (checksum + (((s2.0).0) as u128));"));
    assert!(output.contains("checksum = (checksum + (((s2.1).0) as u128));"));
}

// fn main() {
//     let mut s2: Struct2 = Struct2(Struct1(1_u32,), Struct1(2_u32,));
//     s2;
//     {
//         s2 = Struct2(Struct1(3_u32,), Struct1(4_u32,));
//     };
// }

#[test]
fn struct_move_reassign_block() {
    let mut template = StructTemplate::new(true);
    template.add_stmt(StructTemplate::struct_ident_stmt("s2"));
    let block_stmts = vec![StructTemplate::struct_2_assign_stmt("s2", 3, 4)];
    template.add_stmt(StructTemplate::block_stmt(block_stmts));
    let output = generate_checksum_and_emit(&mut template.2);
    assert!(output.contains("checksum = (checksum + (((s2.0).0) as u128));"));
    assert!(output.contains("checksum = (checksum + (((s2.1).0) as u128));"));
}

// fn main() {
//     let s2: Struct2 = Struct2(Struct1(1_u32,), Struct1(2_u32,));
//     {
//         s2;
//     };
// }
#[test]
fn struct_block_move() {
    let mut template = StructTemplate::default();
    let block_stmts = vec![StructTemplate::struct_ident_stmt("s2")];
    template.add_stmt(StructTemplate::block_stmt(block_stmts));
    let output = generate_checksum_and_emit(&mut template.2);
    assert!(!output.contains("checksum = (checksum + (((s2.0).0) as u128));"));
    assert!(!output.contains("checksum = (checksum + (((s2.1).0) as u128));"));
}

// fn main() {
//     let s2: Struct2 = Struct2(Struct1(1_u32,), Struct1(2_u32,));
//     {
//         s2.0;
//     };
// }
#[test]
fn struct_block_partial_move() {
    let mut template = StructTemplate::default();
    let block_stmts = vec![SemiStmt {
        expr: FieldExpr {
            base: Box::new(
                IdentExpr {
                    name: "s2".to_string(),
                }
                .into(),
            ),
            member: Member::Unnamed(0),
        }
        .into(),
    }
    .into()];
    template.add_stmt(StructTemplate::block_stmt(block_stmts));
    let output = generate_checksum_and_emit(&mut template.2);
    assert!(!output.contains("checksum = (checksum + (((s2.0).0) as u128));"));
    assert!(output.contains("checksum = (checksum + (((s2.1).0) as u128));"));
}

// fn main() {
//     let mut s2: Struct2 = Struct2(Struct1(1_u32,), Struct1(2_u32,));
//     s2;
//     {
//         s2.0 = Struct1(3_32);
//     };
// }

// This program is invalid. Checksum visitor has precondition that the program is valid.
#[test]
fn struct_block_partial_reassign_invalid() {
    let mut template = StructTemplate::new(true);
    template.add_stmt(StructTemplate::struct_ident_stmt("s2"));
    let block_stmts = vec![StructTemplate::struct_2_partial_assign_stmt("s2", 0, 3)];
    template.add_stmt(StructTemplate::block_stmt(block_stmts));
    let output = generate_checksum_and_emit(&mut template.2);
    assert!(!output.contains("checksum = (checksum + (((s2.0).0) as u128));"));
    assert!(!output.contains("checksum = (checksum + (((s2.1).0) as u128));"));
}

// fn main() {
//     let mut s2: Struct2 = Struct2(Struct1(1_u32,), Struct1(2_u32,));
//     s2;
//     if (false) {
//         s2 = Struct2(Struct1(3_u32,), Struct1(4_u32,));
//     };
// }

#[test]
fn struct_move_reassign_single_branch() {
    let mut template = StructTemplate::new(true);
    template.add_stmt(StructTemplate::struct_ident_stmt("s2"));
    let then_stmts = vec![StructTemplate::struct_2_assign_stmt("s2", 3, 4)];
    template.add_stmt(StructTemplate::if_else_stmt(false, then_stmts, None));
    let output = generate_checksum_and_emit(&mut template.2);
    assert!(!output.contains("checksum = (checksum + (((s2.0).0) as u128));"));
    assert!(!output.contains("checksum = (checksum + (((s2.1).0) as u128));"));
}

// fn main() {
//     let mut s2: Struct2 = Struct2(Struct1(1_u32,), Struct1(2_u32,));
//     s2;
//     if (true) {
//         s2 = Struct2(Struct1(3_u32,), Struct1(4_u32,));
//     } else {
//         s2 = Struct2(Struct1(5_u32,), Struct1(6_u32,));
//     };
// }

#[test]
fn struct_move_reassign_both_branch() {
    let mut template = StructTemplate::new(true);
    let then_stmts = vec![StructTemplate::struct_2_assign_stmt("s2", 3, 4)];
    let otherwise_stmts = vec![StructTemplate::struct_2_assign_stmt("s2", 5, 6)];
    template.add_stmt(StructTemplate::if_else_stmt(
        true,
        then_stmts,
        Some(otherwise_stmts),
    ));
    let output = generate_checksum_and_emit(&mut template.2);
    assert!(output.contains("checksum = (checksum + (((s2.0).0) as u128));"));
    assert!(output.contains("checksum = (checksum + (((s2.1).0) as u128));"));
}

// fn main() {
//     let mut s2: Struct2 = Struct2(Struct1(1_u32,), Struct1(2_u32,));
//     if (true) {
//         s2.0;
//     } else {
//         s2.1;
//     };
// }

#[test]
fn struct_move_both_branch() {
    let mut template = StructTemplate::new(true);
    let then_stmts = vec![StructTemplate::tuple_field_stmt("s2", 0)];
    let otherwise_stmts = vec![StructTemplate::tuple_field_stmt("s2", 1)];
    template.add_stmt(StructTemplate::if_else_stmt(
        true,
        then_stmts,
        Some(otherwise_stmts),
    ));
    let output = generate_checksum_and_emit(&mut template.2);
    assert!(!output.contains("checksum = (checksum + (((s2.0).0) as u128));"));
    assert!(!output.contains("checksum = (checksum + (((s2.1).0) as u128));"));
}

// #[derive(PartialEq, Clone)]
// struct Struct1(u32);
//
// #[derive(PartialEq, Clone)]
// struct Struct2(Struct1, Struct1);
//
// fn main() {
//     let s2: Struct2 = Struct2(Struct1(1_u32,), Struct1(2_u32,));
//     (s2.0);
// }

#[test]
fn partial_struct_move() {
    let mut template = StructTemplate::default();
    template.add_stmt(
        StructTemplate::tuple_field_stmt("s2", 0)
    );
    let output = generate_checksum_and_emit(&mut template.2);
    assert!(!output.contains("checksum = (checksum + (((s2.0).0) as u128));"));
    assert!(output.contains("checksum = (checksum + (((s2.1).0) as u128));"));
}


// fn main() {
//     let mut s2: Struct2 = Struct2(Struct1(1_u32,), Struct1(2_u32,));
//     s2;
//     s2 = Struct2(Struct1(3_u32,), Struct1(4_u32,));
//     s2.0
//     if (true) {
//         s2.1;
//         s2.0 = Struct1(5,)
//     }
// }

#[test]
fn struct_move_advanced() {
    let mut template = StructTemplate::new(true);
    template.add_stmt(StructTemplate::struct_ident_stmt("s2"));
    template.add_stmt(StructTemplate::struct_2_assign_stmt("s2", 3, 4));
    template.add_stmt(StructTemplate::tuple_field_stmt("s2", 0));

    let then_stmts = vec![
        StructTemplate::tuple_field_stmt("s2", 1),
        StructTemplate::struct_2_partial_assign_stmt("s2", 0, 5),
    ];
    template.add_stmt(StructTemplate::if_else_stmt(
        true,
        then_stmts,
        None,
    ));
    let output = generate_checksum_and_emit(&mut template.2);
    assert!(!output.contains("checksum = (checksum + (((s2.0).0) as u128));"));
    assert!(!output.contains("checksum = (checksum + (((s2.1).0) as u128));"));
}

// fn main() {
//     let mut s2: Struct2 = Struct2(Struct1(1_u32,), Struct1(2_u32,));
//     s2;
//     s2 = Struct2(Struct1(3_u32,), Struct1(44_u32,));
//     s2.0
//     if (true) {
//         s2.1;
//         s2.0 = Struct1(5,)
//     } else {
//         s2.0 = Struct1(5,)
//     }
// }

#[test]
fn struct_move_advanced_2() {
    let mut template = StructTemplate::new(true);
    template.add_stmt(StructTemplate::struct_ident_stmt("s2"));
    template.add_stmt(StructTemplate::struct_2_assign_stmt("s2", 3, 4));
    template.add_stmt(StructTemplate::tuple_field_stmt("s2", 0));

    let then_stmts = vec![
        StructTemplate::tuple_field_stmt("s2", 1),
        StructTemplate::struct_2_partial_assign_stmt("s2", 0, 5),
    ];
    let other_stmts = vec![
        StructTemplate::struct_2_partial_assign_stmt("s2", 0, 5),
    ];
    template.add_stmt(StructTemplate::if_else_stmt(
        true,
        then_stmts,
        Some(other_stmts),
    ));
    let output = generate_checksum_and_emit(&mut template.2);
    assert!(output.contains("checksum = (checksum + (((s2.0).0) as u128));"));
    assert!(!output.contains("checksum = (checksum + (((s2.1).0) as u128));"));
}