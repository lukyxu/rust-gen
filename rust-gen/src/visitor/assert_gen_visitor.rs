//! Visitor for adding assertions into the program.

use crate::ast::expr::BlockExpr;
use crate::ast::stmt::{AssertStmt, CustomStmt, Stmt};
use crate::symbol_table::ty::TypeSymbolTable;
use crate::visitor::checksum_gen_visitor::exprs_from_ident;
use crate::visitor::validation_gen_visitor::{ValidationGen, ValidationGenVisitor};

pub type AssertGenVisitor = ValidationGenVisitor<AssertGen>;

pub struct AssertGen;

impl ValidationGen for AssertGen {
    fn add_validation(
        block_expr: &mut BlockExpr,
        name: &String,
        full_type_symbol_table: &TypeSymbolTable,
        _checksum_name: &'static str,
    ) {
        let ty = full_type_symbol_table.get_var_type(name).unwrap();
        let exprs = exprs_from_ident(name, &ty);
        for expr in exprs {
            let stmt = Stmt::Custom(CustomStmt::Assert(AssertStmt {
                lhs_expr: expr.clone(),
                rhs_expr: None,
            }));
            block_expr.stmts.insert(block_expr.stmts.len() - 1, stmt);
        }
    }
}
