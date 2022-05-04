use crate::ast::expr::LitExprTy::Unsigned;
use crate::ast::expr::{
    AssignExpr, BinaryExpr, BinaryOp, BlockExpr, CastExpr, Expr, IdentExpr,
    LitExpr,
};
use crate::ast::function::Function;
use crate::ast::stmt::{
    CustomStmt, InitLocalStmt, LocalStmt, SemiStmt, Stmt,
};
use crate::ast::ty::{Ty, UIntTy};
use crate::symbol_table::ty::TypeSymbolTable;
use crate::visitor::base_visitor::Visitor;

pub struct ChecksumGenVisitor {
    local_type_symbol_table: TypeSymbolTable,
    prev_local_type_symbol_tables: Vec<TypeSymbolTable>,
    checksum_name: &'static str,
}

impl ChecksumGenVisitor {
    pub fn new() -> ChecksumGenVisitor {
        ChecksumGenVisitor {
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
        self.local_type_symbol_table = self.prev_local_type_symbol_tables.pop().unwrap()
    }

    fn visit_function(&mut self, function: &mut Function) {
        function.block.stmts.insert(
            0,
            Stmt::Local(LocalStmt::Init(InitLocalStmt {
                name: self.checksum_name.to_owned(),
                ty: Ty::UInt(UIntTy::U128),
                rhs: Expr::Literal(LitExpr::Int(0, Unsigned(UIntTy::U128))),
                mutable: true,
            })),
        );
        function.block.stmts.push(Stmt::Custom(CustomStmt {
            stmt: format!("println!(\"{{}}\", {})", self.checksum_name),
        }));
        self.visit_block_expr(&mut function.block);
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
            let ty = ty_mapping.ty.clone();
            let cast_expr = match ty {
                Ty::Int(_) | Ty::UInt(_) => Expr::Cast(CastExpr {
                    expr: {
                        Box::new(Expr::Ident(IdentExpr {
                            name: name.clone(),
                            ty,
                        }))
                    },
                    ty: Ty::UInt(UIntTy::U128),
                }),
                // TODO: Hash other types too
                _ => continue,
            };
            let stmt = Stmt::Semi(SemiStmt {
                expr: Expr::Assign(AssignExpr {
                    name: self.checksum_name.to_owned(),
                    rhs: Box::new(Expr::Binary(BinaryExpr {
                        lhs: Box::new(Expr::Ident(IdentExpr {
                            name: self.checksum_name.to_owned(),
                            ty: Ty::UInt(UIntTy::U128),
                        })),
                        rhs: Box::new(cast_expr),
                        op: BinaryOp::Add,
                    })),
                }),
            });
            expr.stmts.insert(expr.stmts.len() - 1, stmt);
        }
        self.visit_stmt((&mut expr.stmts).last_mut().unwrap());
        self.exit_scope();
    }
}
