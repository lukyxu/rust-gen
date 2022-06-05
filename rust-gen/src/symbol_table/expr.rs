use crate::generate::eval_expr::EvalExpr;
use crate::ast::ty::Ty;
use std::collections::hash_map::Iter;
use std::collections::HashMap;

#[derive(Debug, Default, Clone)]
pub struct ExprSymbolTable {
    expr_mapping: HashMap<String, EvalExpr>,
    ty_mapping: HashMap<String, Ty>,
}

impl<'a> IntoIterator for &'a ExprSymbolTable {
    type Item = (&'a String, &'a Ty);
    type IntoIter = Iter<'a, String, Ty>;

    fn into_iter(self) -> Self::IntoIter {
        (&self.ty_mapping).iter()
    }
}

impl ExprSymbolTable {
    pub fn get_expr_by_name(&self, name: &str) -> Option<EvalExpr> {
        Some(self.expr_mapping.get(name)?.clone())
    }

    pub fn get_expr_ref_by_name(&mut self, name: &str) -> Option<&mut EvalExpr> {
        self.expr_mapping.get_mut(name)
    }

    pub fn get_ty_by_name(&self, name: &str) -> Option<Ty> {
        Some(self.ty_mapping.get(name)?.clone())
    }

    pub fn add_expr(&mut self, key: &str, value: EvalExpr, ty: Ty) {
        self.expr_mapping.insert(key.to_owned(), value);
        self.ty_mapping.insert(key.to_owned(), ty);
    }

    pub fn update(&mut self, other: &ExprSymbolTable) {
        for (name, expr) in &other.expr_mapping {
            if let Some(ty) = self.get_ty_by_name(name) {
                self.add_expr(name, expr.clone(), ty);
            }
        }
    }
}
