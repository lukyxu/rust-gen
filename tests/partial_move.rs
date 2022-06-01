use rust_gen::ast::expr::{BlockExpr, Expr, FieldExpr, IdentExpr, Member, TupleExpr, TupleStructExpr};
use rust_gen::ast::file::RustFile;
use rust_gen::ast::function::Function;
use rust_gen::ast::item::{FunctionItem, StructItem};
use rust_gen::ast::stmt::{InitLocalStmt, SemiStmt};
use rust_gen::ast::ty::{StructTy, TupleStructTy, TupleTy, UIntTy};
use rust_gen::visitor::base_visitor::Visitor;
use rust_gen::visitor::checksum_gen_visitor::ChecksumGenVisitor;
use rust_gen::visitor::emit_visitor::EmitVisitor;

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
    let mut file = RustFile {
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
                    block: BlockExpr {
                        stmts: vec![
                            // let s2 = Struct2(Struct1(1), (Struct1(2)));
                            InitLocalStmt {
                                name: "s2".to_string(),
                                ty: struct2.clone().into(),
                                rhs: TupleStructExpr {
                                    struct_name: "Struct2".to_string(),
                                    fields: TupleExpr {
                                        tuple: vec![
                                            TupleStructExpr {
                                                struct_name: "Struct1".to_string(),
                                                fields: TupleExpr {
                                                    tuple: vec![Expr::u32(1)],
                                                },
                                            }
                                            .into(),
                                            TupleStructExpr {
                                                struct_name: "Struct1".to_string(),
                                                fields: TupleExpr {
                                                    tuple: vec![Expr::u32(2)],
                                                },
                                            }
                                            .into(),
                                        ],
                                    },
                                }
                                .into(),
                                mutable: false,
                            }
                            .into(),
                            SemiStmt {
                                expr: FieldExpr {
                                    base: Box::new(IdentExpr {
                                        name: "s2".to_string()
                                    }.into()),
                                    member: Member::Unnamed(0)
                                }.into()
                            }.into(),
                        ],
                    },
                },
            }
            .into(),
        ],
    };
    let mut checksum_gen_visitor = ChecksumGenVisitor::new(true);
    checksum_gen_visitor.visit_file(&mut file);
    let mut emit_visitor = EmitVisitor::default();
    emit_visitor.visit_file(&mut file);
    let output = emit_visitor.output();
    println!("{}", output);
    assert!(output.contains("checksum = (checksum + (((s2.0).0) as u128));"));
    assert!(!output.contains("checksum = (checksum + (((s2.1).0) as u128));"));
}
