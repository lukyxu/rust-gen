use std::any::Any;
use crate::ast::expr::IdentExpr;
use crate::ast::ty::Ty;
use std::collections::btree_map::Iter;
use std::collections::BTreeMap;

#[derive(Debug, Clone)]
pub struct TypeMapping {
    pub ty: Ty,
    pub moved: bool,
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
            .insert(key, TypeMapping { ty, moved: false, mutable });
    }

    pub fn move_var(&mut self, key: &str) {
        assert!(!self.var_type_mapping.get_mut(key).unwrap().moved);
        (*self.var_type_mapping.get_mut(key).unwrap()).moved = true;
    }

    pub fn contains(&self, key: &String) -> bool {
        self.var_type_mapping.contains_key(key)
    }

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
            .filter(|&(_k, v)| v.ty == *ty && !v.moved)
            .map(|(name, ty_mapping)| IdentExpr {
                name: name.clone(),
                ty: ty_mapping.ty.clone(),
            })
            .collect()
    }

    pub fn get_mut_ident_exprs_by_type(&self, ty: &Ty) -> Vec<IdentExpr> {
        self.var_type_mapping
            .iter()
            .filter(|&(_k, v)| v.mutable && v.ty == *ty && !v.moved)
            .map(|(name, ty_mapping)| IdentExpr {
                name: name.clone(),
                ty: ty_mapping.ty.clone(),
            })
            .collect()
    }

    pub fn merge_inplace(&mut self, other: &TypeSymbolTable) {
        for (k, v) in self.var_type_mapping.iter_mut() {
            v.moved = other.var_type_mapping.get(k).unwrap().moved;
        };
    }

    pub fn merge(mut self, other: &TypeSymbolTable) -> TypeSymbolTable {
        for (k, v) in self.var_type_mapping.iter_mut() {
            v.moved = other.var_type_mapping.get(k).unwrap().moved;
        };
        self
    }
}

impl<'a> IntoIterator for &'a TypeSymbolTable {
    type Item = (&'a String, &'a TypeMapping);
    type IntoIter = Iter<'a, String, TypeMapping>;

    fn into_iter(self) -> Self::IntoIter {
        (&self.var_type_mapping).iter()
    }
}
