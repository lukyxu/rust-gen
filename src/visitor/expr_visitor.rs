use crate::ast::eval_expr::{
    EvalArrayExpr, EvalExpr, EvalField, EvalFieldStructExpr, EvalPlaceExpr, EvalReferenceExpr,
    EvalStructExpr, EvalTupleExpr, EvalTupleStructExpr,
};
use crate::ast::expr::{
    ArrayExpr, AssignExpr, BinaryExpr, BlockExpr, CastExpr, Expr, FieldExpr,
    FieldStructExpr, IdentExpr, IfExpr, IndexExpr, LitExpr, LitIntExpr, LitIntTy, Member,
    PlaceExpr, ReferenceExpr, TupleExpr, TupleStructExpr, UnaryExpr,
};





use crate::ast::stmt::{DeclLocalStmt, InitLocalStmt, SemiStmt};
use crate::ast::ty::{GTy, Ty, UIntTy};
use crate::symbol_table::expr::ExprSymbolTable;
use crate::visitor::base_visitor;
use crate::visitor::base_visitor::Visitor;

#[derive(Clone)]
/// Visitor used to correct certain behaviours that would result in runtime errors.
/// Any program generated using generate functions and passed through `ExprVisitor`
/// should result in valid rust programs which are executable and do not crash.
pub struct ExprVisitor {
    expr: Option<EvalExpr>,
    pub symbol_table: ExprSymbolTable,
    prev_symbol_tables: Vec<ExprSymbolTable>,
    max_attempt_fix: usize,
}

impl Default for ExprVisitor {
    fn default() -> ExprVisitor {
        ExprVisitor {
            expr: None,
            symbol_table: ExprSymbolTable::default(),
            prev_symbol_tables: vec![],
            max_attempt_fix: 2,
        }
    }
}

impl ExprVisitor {
    fn safe_expr_visit(&mut self, expr: &mut Expr) -> EvalExpr {
        self.expr = None;
        self.visit_expr(expr);
        assert_ne!(self.expr, Some(EvalExpr::Unknown));
        self.expr.clone().unwrap()
    }

    fn add_expr(&mut self, key: &str, value: &EvalExpr, ty: &Ty) {
        self.symbol_table.add_expr(key, value.clone(), ty.clone());
    }

    fn symbol_table(&self) -> &ExprSymbolTable {
        &self.symbol_table
    }

    fn mut_symbol_table(&mut self) -> &mut ExprSymbolTable {
        &mut self.symbol_table
    }

    fn visit_block_internal(&mut self, block_expr: &mut BlockExpr) -> ExprSymbolTable {
        self.enter_scope();
        for stmt in &mut block_expr.stmts {
            self.visit_stmt(stmt);
        }
        let res = self.symbol_table.clone();
        self.exit_scope();
        res
    }

    fn update_symbol_table(&mut self, new_symbol_table: &ExprSymbolTable) {
        self.symbol_table.update_symbol_table(new_symbol_table);
    }

    fn eval_place_expr(&mut self, expr: &mut Expr) -> Option<EvalPlaceExpr> {
        match expr {
            Expr::Field(expr) => Some(EvalPlaceExpr::Field(
                Box::new(self.eval_place_expr(&mut expr.base)?),
                expr.member.clone(),
            )),
            Expr::Index(expr) => {
                let place = self.eval_place_expr(&mut expr.base);
                let eval_expr = self.safe_expr_visit(&mut expr.index);
                let value = if let EvalExpr::Literal(LitExpr::Int(LitIntExpr {
                    value,
                    ty: LitIntTy::Unsigned(UIntTy::USize),
                })) = eval_expr
                {
                    value
                } else {
                    panic!("Unexpected index type")
                };
                let place = if let Some(place) = place {
                    Box::new(place)
                } else {
                    return None;
                };
                Some(EvalPlaceExpr::Index(place, value as usize))
            }
            Expr::Ident(expr) => Some(EvalPlaceExpr::Ident(expr.name.clone())),
            _ => {
                self.visit_expr(expr);
                return None;
            }
        }
    }

    fn helper<'a>(prev_expr: &'a mut EvalExpr, place_expr: &EvalPlaceExpr) -> &'a mut EvalExpr {
        match place_expr {
            EvalPlaceExpr::Ident(_) => prev_expr,
            EvalPlaceExpr::Field(place_expr, member) => {
                let prev_expr: &mut EvalExpr = ExprVisitor::helper(prev_expr, &place_expr);
                match (prev_expr, member) {
                    (EvalExpr::Tuple(tuple_expr), Member::Unnamed(index)) => {
                        &mut tuple_expr.tuple[*index]
                    }
                    (
                        EvalExpr::Struct(EvalStructExpr::Tuple(tuple_expr)),
                        Member::Unnamed(index),
                    ) => &mut tuple_expr.expr.tuple[*index],
                    (EvalExpr::Struct(EvalStructExpr::Field(field_expr)), Member::Named(name)) => {
                        &mut field_expr.get_mut_field_by_name(name).unwrap().expr
                    }
                    _ => panic!(),
                }
            }
            EvalPlaceExpr::Index(place_expr, index) => {
                let prev_expr = ExprVisitor::helper(prev_expr, &place_expr);
                match prev_expr {
                    EvalExpr::Array(array_expr) => &mut array_expr.array[*index],
                    _ => panic!(),
                }
            }
        }
    }
}

impl Visitor for ExprVisitor {
    fn enter_scope(&mut self) {
        self.prev_symbol_tables.push(self.symbol_table.clone());
    }

    fn exit_scope(&mut self) {
        self.symbol_table = self.prev_symbol_tables.pop().unwrap();
    }

    // TODO: Implement local decl stmt
    fn visit_local_decl_stmt(&mut self, _stmt: &mut DeclLocalStmt) {
        todo!();
    }

    fn visit_local_init_stmt(&mut self, stmt: &mut InitLocalStmt) {
        let res_expr = self.safe_expr_visit(&mut stmt.rhs);
        self.add_expr(&stmt.name, &res_expr, &stmt.ty);
        self.expr = Some(EvalExpr::unit_expr());
    }

    fn visit_semi_stmt(&mut self, stmt: &mut SemiStmt) {
        self.visit_expr(&mut stmt.expr);
        self.expr = Some(EvalExpr::unit_expr());
    }

    fn visit_expr(&mut self, expr: &mut Expr) {
        if let Expr::Unary(_) = expr {
            // visit_unary_expr can modify the expr to some other expr variant
            self.visit_unary_expr(expr);
        } else {
            base_visitor::walk_expr(self, expr);
        }
    }

    fn visit_literal_expr(&mut self, expr: &mut LitExpr) {
        self.expr = Some(EvalExpr::Literal(expr.clone()));
    }

    fn visit_binary_expr(&mut self, expr: &mut BinaryExpr) {
        let mut lhs = self.safe_expr_visit(&mut expr.lhs);
        if expr.op.short_circuit_rhs(&lhs) {
            // Need to visit expression to make sure that there are no errors in the rhs
            self.enter_scope();
            self.safe_expr_visit(&mut expr.rhs);
            self.expr = Some(lhs);
            self.exit_scope();
            return;
        }
        let mut rhs = self.safe_expr_visit(&mut expr.rhs);
        let mut res = expr.op.apply(&lhs, &rhs);
        for _ in 0..self.max_attempt_fix {
            if let Err(err) = &res {
                expr.fix(*err, &mut lhs, &mut rhs);
                res = expr.op.apply(&lhs, &rhs);
            } else {
                break;
            }
        }
        self.expr = Some(res.unwrap());
    }
    fn visit_unary_expr(&mut self, _expr: &mut UnaryExpr) {
        unreachable!()
    }

    fn visit_cast_expr(&mut self, expr: &mut CastExpr) {
        self.expr = Some(self.safe_expr_visit(&mut expr.expr).cast(&expr.ty).unwrap());
    }

    fn visit_if_expr(&mut self, expr: &mut IfExpr) {
        let cond_expr = self.safe_expr_visit(&mut expr.condition);

        let true_sym_table = self.visit_block_internal(&mut expr.then);
        let true_expr: EvalExpr = self.expr.clone().unwrap();

        let (false_expr, false_sym_table) = if let Some(otherwise) = &mut expr.otherwise {
            let false_sym_table = Some(self.visit_block_internal(otherwise));
            (self.expr.clone().unwrap(), false_sym_table)
        } else {
            (EvalExpr::unit_expr(), None)
        };

        self.expr = Some(match &cond_expr {
            EvalExpr::Literal(LitExpr::Bool(true)) => {
                self.update_symbol_table(&true_sym_table);
                true_expr
            }
            EvalExpr::Literal(LitExpr::Bool(false)) => {
                if let Some(false_sym_table) = false_sym_table {
                    self.update_symbol_table(&false_sym_table);
                }
                false_expr
            }
            EvalExpr::Unknown => EvalExpr::Unknown,
            _ => {
                dbg!(cond_expr);
                panic!("unexpected condition value");
            }
        });
    }

    fn visit_block_expr(&mut self, expr: &mut BlockExpr) {
        let scoped_symbol_table = self.visit_block_internal(expr);
        self.update_symbol_table(&scoped_symbol_table);
    }

    // fn visit_block_expr(&mut self, expr: &mut BlockExpr) {
    //     walk_block_expr(self, expr)
    // }
    fn visit_ident_expr(&mut self, expr: &mut IdentExpr) {
        if let Some(expr) = self.symbol_table().get_expr_by_name(&expr.name) {
            self.expr = Some(expr.clone());
            // Should never evaluated to unknown value
            assert_ne!(expr, EvalExpr::Unknown);
        } else {
            panic!("Unexpected ident expression")
        }
    }

    fn visit_tuple_expr(&mut self, expr: &mut TupleExpr) {
        let mut res: Vec<EvalExpr> = vec![];
        let mut _return_none = false;
        for inner_expr in &mut expr.tuple {
            let res_expr = self.safe_expr_visit(inner_expr);
            if let EvalExpr::Unknown = res_expr {
                _return_none = true;
            } else {
                res.push(res_expr);
            }
        }
        let res_expr: EvalExpr = if _return_none {
            drop(EvalExpr::Unknown);
            panic!();
        } else {
            EvalExpr::Tuple(EvalTupleExpr { tuple: res })
        };
        self.expr = Some(res_expr);
    }

    fn visit_assign_expr(&mut self, expr: &mut AssignExpr) {
        let res_expr = self.safe_expr_visit(&mut expr.rhs);
        // let mut block_expr =
        //     BlockExpr {
        //         stmts: vec![Stmt::Expr(ExprStmt {
        //             expr: expr.place.into()
        //         })]
        //     };
        // let sym_table = self.visit_block_internal(&mut block_expr);
        //
        // expr.place = match block_expr.stmts[0].clone() {
        //     Stmt::Expr(ExprStmt {expr: Expr::Ident(expr)}) => PlaceExpr::Ident(expr),
        //     Stmt::Expr(ExprStmt {expr: Expr::Field(expr)}) => PlaceExpr::Field(expr),
        //     Stmt::Expr(ExprStmt {expr: Expr::Index(expr)}) => PlaceExpr::Index(expr),
        //     _ => panic!()
        // };

        let mut place_expr: Expr = expr.place.clone().into();
        let eval_expr = self.eval_place_expr(&mut place_expr);

        expr.place = match place_expr {
            Expr::Ident(expr) => PlaceExpr::Ident(expr),
            Expr::Index(expr) => PlaceExpr::Index(expr),
            Expr::Field(expr) => PlaceExpr::Field(expr),
            _ => panic!("Invalid place expression type"),
        };

        if let Some(eval_expr) = eval_expr {
            // (var2.1)[1] = 5
            // ver2 = ((),[1,2])
            // eval_expr Index(Field(Ident))
            // prev_expr Tuple([Tuple([]), Array([1,2]))
            let name = eval_expr.name();
            let prev_expr = self.mut_symbol_table().get_expr_ref_by_name(&name).unwrap();
            let prev_expr = ExprVisitor::helper(prev_expr, &eval_expr);
            *prev_expr = res_expr;
            // self.update_place_expr(prev_expr, eval_expr, res_expr)
            // let prev_expr = prev_expr.unwrap();

            // self.add_expr(&name, &new_expr, &self.symbol_table().get_ty_by_name(&name).unwrap())
            // match eval_expr {
            //     EvalPlaceExpr::Ident(name) => {
            //         // If eval place is ident expression then add
            //         // otherwise get expression and
            //         self.add_expr(
            //             &name,
            //             &res_expr,
            //             &self.symbol_table().get_ty_by_name(&name).unwrap(),
            //         );
            //     }
            //
            //     EvalPlaceExpr::Field(eval_expr, member) => {
            //         let mut visitor = EmitVisitor::default();
            //         visitor.visit_assign_expr(expr);
            //         println!("{}", visitor.output())
            //     },
            //     EvalPlaceExpr::Index(expr, index) => println!("haha"),
            // }
        }

        self.expr = Some(EvalExpr::unit_expr());
    }

    fn visit_field_expr(&mut self, expr: &mut FieldExpr) {
        let base = self.safe_expr_visit(&mut expr.base);
        match (base, &expr.member) {
            (EvalExpr::Tuple(exprs), Member::Unnamed(index)) => {
                self.expr = Some(exprs.tuple[*index].clone());
            }
            (EvalExpr::Struct(EvalStructExpr::Tuple(struct_expr)), Member::Unnamed(index)) => {
                self.expr = Some(struct_expr.expr.tuple[*index].clone());
            }
            (EvalExpr::Struct(EvalStructExpr::Field(struct_expr)), Member::Named(field_name)) => {
                self.expr = Some(struct_expr.get_field_by_name(field_name).unwrap().expr);
            }
            (_, _) => panic!(),
        }
    }

    fn visit_array_expr(&mut self, expr: &mut ArrayExpr) {
        let mut res: Vec<EvalExpr> = vec![];
        let mut _return_none = false;
        for inner_expr in &mut expr.array {
            let res_expr = self.safe_expr_visit(inner_expr);
            if let EvalExpr::Unknown = res_expr {
                _return_none = true;
            } else {
                res.push(res_expr);
            }
        }
        let res_expr: EvalExpr = if _return_none {
            drop(EvalExpr::Unknown);
            panic!()
        } else {
            EvalExpr::Array(EvalArrayExpr { array: res })
        };
        self.expr = Some(res_expr);
    }

    fn visit_index_expr(&mut self, expr: &mut IndexExpr) {
        let base = self.safe_expr_visit(&mut expr.base);
        let index = self.safe_expr_visit(&mut expr.index);
        match (base, index) {
            (
                EvalExpr::Array(exprs),
                EvalExpr::Literal(LitExpr::Int(LitIntExpr {
                    value: index,
                    ty: LitIntTy::Unsigned(UIntTy::USize),
                })),
            ) => {
                self.expr = Some(exprs.array[index as usize].clone());
            }
            _ => panic!(),
        };
    }

    fn visit_field_struct_expr(&mut self, expr: &mut FieldStructExpr) {
        let mut fields = vec![];
        for field in &mut expr.fields {
            let expr = self.safe_expr_visit(&mut field.expr);
            fields.push(EvalField {
                name: field.name.clone(),
                expr,
            });
        }
        self.expr = Some(EvalExpr::Struct(EvalStructExpr::Field(
            EvalFieldStructExpr { fields },
        )));
    }

    fn visit_tuple_struct_expr(&mut self, expr: &mut TupleStructExpr) {
        self.visit_tuple_expr(&mut expr.fields);
        let tuple_expr = self.expr.clone().unwrap();
        if let EvalExpr::Tuple(expr) = tuple_expr {
            self.expr = Some(EvalExpr::Struct(EvalStructExpr::Tuple(
                EvalTupleStructExpr { expr },
            )));
        } else {
            panic!()
        }
    }

    fn visit_reference_expr(&mut self, expr: &mut ReferenceExpr) {
        let expr = Box::new(self.safe_expr_visit(&mut expr.expr));
        self.expr = Some(EvalExpr::Reference(EvalReferenceExpr { expr }))
    }
}

impl ExprVisitor {
    fn visit_unary_expr(&mut self, expr: &mut Expr) {
        if let Expr::Unary(unary_expr) = expr {
            let eval_expr = self.safe_expr_visit(&mut unary_expr.expr);
            let mut res = unary_expr.op.apply(&eval_expr);
            if res.is_err() {
                res = Ok(eval_expr);
                *expr = *unary_expr.clone().expr;
            }
            self.expr = Some(res.unwrap());
        } else {
            panic!();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::ast::expr::BlockExpr;
    use crate::ast::function::Function;
    use crate::ast::op::{BinaryOp, UnaryOp};
    use crate::ast::stmt::{LocalStmt, Stmt};
    use crate::ast::ty::IntTy;
    use crate::visitor::emit_visitor::EmitVisitor;

    #[test]
    fn unary_expr_ok_not() {
        for b in [true, false] {
            let mut expr = Expr::Unary(UnaryExpr {
                expr: Box::new(Expr::bool(b)),
                op: UnaryOp::Not,
            });
            let expected_expr = expr.clone();
            let mut visitor = ExprVisitor::default();
            visitor.visit_unary_expr(&mut expr);
            // Assert that the tree is the same
            assert_eq!(expr, expected_expr);
            let eval_expr = visitor.expr;
            let expected_eval_expr = Some(EvalExpr::bool(!b));
            // Assert that the evaluated value is correct
            assert_eq!(eval_expr, expected_eval_expr);
        }
    }

    #[test]
    fn unary_expr_ok_neg() {
        let mut expr = Expr::Unary(UnaryExpr {
            expr: Box::new(Expr::i8(32)),
            op: UnaryOp::Neg,
        });
        let expected_expr = expr.clone();
        let mut visitor = ExprVisitor::default();
        visitor.visit_unary_expr(&mut expr);
        assert_eq!(expr, expected_expr);
        let eval_expr = visitor.expr;
        let expected_eval_expr = Some(EvalExpr::i8(-32));
        assert_eq!(eval_expr, expected_eval_expr);
    }

    #[test]
    fn unary_expr_fix_overflow_neg() {
        let mut expr = Expr::Unary(UnaryExpr {
            expr: Box::new(Expr::i8(i8::MIN)),
            op: UnaryOp::Neg,
        });
        let expected_expr = Expr::i8(i8::MIN);
        let mut visitor = ExprVisitor::default();
        visitor.visit_unary_expr(&mut expr);
        assert_eq!(expr, expected_expr);
        let eval_expr = visitor.expr;
        let expected_eval_expr = Some(EvalExpr::i8(i8::MIN));
        assert_eq!(eval_expr, expected_eval_expr);
    }

    #[test]
    fn binary_expr_signed_fix() {
        for op in [
            BinaryOp::Add,
            BinaryOp::Sub,
            BinaryOp::Mul,
            BinaryOp::Div,
            BinaryOp::Rem,
        ] {
            for i in i8::MIN..=i8::MAX {
                for j in i8::MIN..=i8::MAX {
                    let mut expr = BinaryExpr {
                        lhs: Box::new(Expr::i8(i)),
                        rhs: Box::new(Expr::i8(j)),
                        op,
                    };
                    let mut visitor = ExprVisitor::default();
                    visitor.visit_binary_expr(&mut expr);
                }
            }
        }
    }

    #[test]
    fn binary_expr_unsigned_fix() {
        for op in [
            BinaryOp::Add,
            BinaryOp::Sub,
            BinaryOp::Mul,
            BinaryOp::Div,
            BinaryOp::Rem,
        ] {
            for i in u8::MIN..=u8::MAX {
                for j in u8::MIN..=u8::MAX {
                    let mut expr = BinaryExpr {
                        lhs: Box::new(Expr::u8(i)),
                        rhs: Box::new(Expr::u8(j)),
                        op,
                    };
                    let mut visitor = ExprVisitor::default();
                    visitor.visit_binary_expr(&mut expr);
                }
            }
        }
    }

    // fn main() {
    //     let mut var_0 = 0_i8;
    //     {
    //         var_0 = 127_i8;
    //     }
    //     var_0 = var_0 + 1_i8;
    // }

    #[test]
    fn assign_scope_test() {
        let mut func = Function {
            name: "main".to_owned(),
            block: BlockExpr {
                stmts: vec![
                    Stmt::Local(LocalStmt::Init(InitLocalStmt {
                        name: "var_0".to_owned(),
                        ty: IntTy::I8.into(),
                        rhs: Expr::i8(0),
                        mutable: true,
                    })),
                    Stmt::Semi(SemiStmt {
                        expr: Expr::Block(BlockExpr {
                            stmts: vec![Stmt::Semi(SemiStmt {
                                expr: Expr::Assign(AssignExpr {
                                    place: "var_0".to_owned(),
                                    rhs: Box::new(Expr::i8(127)),
                                }),
                            })],
                        }),
                    }),
                    Stmt::Semi(SemiStmt {
                        expr: Expr::Assign(AssignExpr {
                            place: "var_0".to_owned(),
                            rhs: Box::new(Expr::Binary(BinaryExpr {
                                lhs: Box::new(Expr::Ident(IdentExpr {
                                    name: "var_0".to_owned(),
                                    ty: IntTy::I8.into(),
                                })),
                                rhs: Box::new(Expr::i8(1)),
                                op: BinaryOp::Add,
                            })),
                        }),
                    }),
                ],
            },
        };
        let mut expr_visitor = ExprVisitor::default();
        expr_visitor.visit_function(&mut func);
        let mut emit_visitor = EmitVisitor::default();
        emit_visitor.visit_function(&mut func);
        println!("{}", emit_visitor.output())
    }

    // fn main() {
    //     let mut var_0 = 0_i8;
    //     if true {
    //         var_0 = 127_i8;
    //     } else {
    //         var_0 = 0_i8;
    //     }
    //     var_0 = var_0 + 1_i8;
    // }

    #[test]
    fn assign_if_true_test() {
        let mut func = Function {
            name: "main".to_owned(),
            block: BlockExpr {
                stmts: vec![
                    Stmt::Local(LocalStmt::Init(InitLocalStmt {
                        name: "var_0".to_owned(),
                        ty: IntTy::I8.into(),
                        rhs: Expr::i8(0),
                        mutable: true,
                    })),
                    Stmt::Semi(SemiStmt {
                        expr: Expr::If(IfExpr {
                            condition: Box::new(Expr::bool(true)),
                            then: Box::new(BlockExpr {
                                stmts: vec![Stmt::Semi(SemiStmt {
                                    expr: Expr::Assign(AssignExpr {
                                        place: "var_0".to_owned(),
                                        rhs: Box::new(Expr::i8(127)),
                                    }),
                                })],
                            }),
                            otherwise: Some(Box::new(BlockExpr {
                                stmts: vec![Stmt::Semi(SemiStmt {
                                    expr: Expr::Assign(AssignExpr {
                                        place: "var_0".to_owned(),
                                        rhs: Box::new(Expr::i8(0)),
                                    }),
                                })],
                            })),
                        }),
                    }),
                    Stmt::Semi(SemiStmt {
                        expr: Expr::Assign(AssignExpr {
                            place: "var_0".to_owned(),
                            rhs: Box::new(Expr::Binary(BinaryExpr {
                                lhs: Box::new(Expr::Ident(IdentExpr {
                                    name: "var_0".to_owned(),
                                    ty: IntTy::I8.into(),
                                })),
                                rhs: Box::new(Expr::i8(1)),
                                op: BinaryOp::Add,
                            })),
                        }),
                    }),
                ],
            },
        };
        let mut expr_visitor = ExprVisitor::default();
        expr_visitor.visit_function(&mut func);
        let mut emit_visitor = EmitVisitor::default();
        emit_visitor.visit_function(&mut func);
        println!("{}", emit_visitor.output())
    }

    // fn main() {
    //     let mut var_0 = 0_i8;
    //     if false {
    //         var_0 = 127_i8;
    //     } else {
    //         var_0 = 0_i8;
    //     }
    //     var_0 = var_0 + 1_i8;
    // }

    #[test]
    fn assign_if_false_test() {
        let mut func = Function {
            name: "main".to_owned(),
            block: BlockExpr {
                stmts: vec![
                    Stmt::Local(LocalStmt::Init(InitLocalStmt {
                        name: "var_0".to_owned(),
                        ty: IntTy::I8.into(),
                        rhs: Expr::i8(0),
                        mutable: true,
                    })),
                    Stmt::Semi(SemiStmt {
                        expr: Expr::If(IfExpr {
                            condition: Box::new(Expr::bool(false)),
                            then: Box::new(BlockExpr {
                                stmts: vec![Stmt::Semi(SemiStmt {
                                    expr: Expr::Assign(AssignExpr {
                                        place: "var_0".to_owned(),
                                        rhs: Box::new(Expr::i8(127)),
                                    }),
                                })],
                            }),
                            otherwise: Some(Box::new(BlockExpr {
                                stmts: vec![Stmt::Semi(SemiStmt {
                                    expr: Expr::Assign(AssignExpr {
                                        place: "var_0".to_owned(),
                                        rhs: Box::new(Expr::i8(0)),
                                    }),
                                })],
                            })),
                        }),
                    }),
                    Stmt::Semi(SemiStmt {
                        expr: Expr::Assign(AssignExpr {
                            place: "var_0".to_owned(),
                            rhs: Box::new(Expr::Binary(BinaryExpr {
                                lhs: Box::new(Expr::Ident(IdentExpr {
                                    name: "var_0".to_owned(),
                                    ty: IntTy::I8.into(),
                                })),
                                rhs: Box::new(Expr::i8(1)),
                                op: BinaryOp::Add,
                            })),
                        }),
                    }),
                ],
            },
        };
        let mut expr_visitor = ExprVisitor::default();
        expr_visitor.visit_function(&mut func);
        let mut emit_visitor = EmitVisitor::default();
        emit_visitor.visit_function(&mut func);
        println!("{}", emit_visitor.output())
    }

    // fn main() {
    //     let mut var_0 = 0_i8;
    //     if false {
    //         var_0 = 127_i8;
    //     } else {
    //         var_0 = 0_i8;
    //     }
    //     var_0 = var_0 + 1_i8;
    // }
}
