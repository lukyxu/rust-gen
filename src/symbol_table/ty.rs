use crate::ast::expr::IdentExpr;
use crate::ast::ty::Ty;
use std::collections::btree_map::Iter;
use std::collections::{BTreeMap};

#[derive(Debug, Clone)]
pub struct TypeMapping {
    pub ty: Ty,
    mutable: bool,
}

#[derive(Debug, Default, Clone)]
pub struct TypeSymbolTable {
    var_type_mapping: BTreeMap<String, TypeMapping>,
}

// TODO: Change this to a bidirectional map
impl TypeSymbolTable {
    pub fn add_var(&mut self, key: String, ty: Ty, mutable: bool) {
        self.var_type_mapping
            .insert(key, TypeMapping { ty, mutable });
    }

    #[allow(dead_code)]
    pub fn contains(&self, key: &String) -> bool {
        self.var_type_mapping.contains_key(key)
    }

    #[allow(dead_code)]
    pub fn get_ident_expr_by_name(&self, key: &String) -> Option<IdentExpr> {
        self.var_type_mapping.get(key).map(|ty_mapping| IdentExpr {
            name: key.clone(),
            ty: ty_mapping.ty.clone(),
        })
    }

    // TODO: refactor
    pub fn get_ident_exprs_by_type(&self, ty: &Ty) -> Vec<IdentExpr> {
        self.var_type_mapping
            .iter()
            .filter(|&(_k, v)| v.ty == *ty)
            .map(|(name, ty_mapping)| IdentExpr {
                name: name.clone(),
                ty: ty_mapping.ty.clone(),
            })
            .collect()
    }

    pub fn get_mut_ident_exprs_by_type(&self, ty: &Ty) -> Vec<IdentExpr> {
        self.var_type_mapping
            .iter()
            .filter(|&(_k, v)| v.mutable && v.ty == *ty)
            .map(|(name, ty_mapping)| IdentExpr {
                name: name.clone(),
                ty: ty_mapping.ty.clone(),
            })
            .collect()
    }
}

impl<'a> IntoIterator for &'a TypeSymbolTable {
    type Item = (&'a String, &'a TypeMapping);
    type IntoIter = Iter<'a, String, TypeMapping>;

    fn into_iter(self) -> Self::IntoIter {
        (&self.var_type_mapping).iter()
    }
}
