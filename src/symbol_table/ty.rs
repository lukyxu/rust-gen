use crate::ast::expr::{Expr, IdentExpr};
use crate::ast::ty::Ty;
use crate::symbol_table::tracked_ty::{OwnershipState, TrackedTy};
use std::collections::btree_map::Iter;
use std::collections::BTreeMap;

#[derive(Debug, Clone)]
pub struct TypeMapping {
    pub ty: TrackedTy,
    mutable: bool,
}

#[derive(Debug, Default, Clone)]
pub struct TypeSymbolTable {
    var_type_mapping: BTreeMap<String, TypeMapping>,
}

impl TypeSymbolTable {
    pub fn add_var(&mut self, key: String, ty: Ty, mutable: bool) {
        self.var_type_mapping.insert(
            key,
            TypeMapping {
                ty: ty.to_tracked(),
                mutable,
            },
        );
    }

    pub fn contains(&self, key: &String) -> bool {
        self.var_type_mapping.contains_key(key)
    }

    // TODO: refactor
    pub fn get_ident_exprs_by_type(&self, ty: &Ty) -> Vec<IdentExpr> {
        self.var_type_mapping
            .iter()
            .filter_map(|(name, mapping)| {
                (Ty::from(&mapping.ty) == *ty && mapping.ty.moveable())
                    .then(|| IdentExpr { name: name.clone() })
            })
            .collect()
    }

    pub fn get_mut_ident_exprs_by_type(&self, ty: &Ty) -> Vec<IdentExpr> {
        self.var_type_mapping
            .iter()
            .filter_map(|(name, mapping)| {
                (mapping.mutable && Ty::from(&mapping.ty) == *ty && mapping.ty.moveable())
                    .then(|| IdentExpr { name: name.clone() })
            })
            .collect()
    }

    pub fn move_expr(&mut self, expr: &Expr, ty: &Ty) -> bool {
        if ty.is_copy() {
            return true
        }
        match expr {
            Expr::Literal(_) | Expr::Binary(_) | Expr::Unary(_) | Expr::Cast(_) | Expr::If(_) | Expr::Block(_) | Expr::Tuple(_) | Expr::Array(_) | Expr::Index(_) | Expr::Struct(_) => {
                true
            }
            Expr::Ident(ident) => {
                let mapping = self.var_type_mapping.get_mut(&ident.name).unwrap();
                match mapping.ty.ownership_state() {
                    OwnershipState::NotApplicable => true,
                    OwnershipState::Owned => {
                        mapping.ty.set_ownership_state(OwnershipState::Moved);
                        true
                    }
                    OwnershipState::PartiallyOwned | OwnershipState::Moved => false
                }
            }
            Expr::Assign(_) => {
                false
            }
            Expr::Field(field_expr) => {
                // Find ident of original
                // recursively update chain
                // field_expr.base
                false
            }
            Expr::Reference(_) => {unimplemented!()}
        }
        // let mapping = self.var_type_mapping.get_mut(key).unwrap();
        // assert!(!mapping.ty.ownership_state());
        // if !mapping.ty.is_copy() {
        //     (*mapping).moved = true;
        // }
    }

    // pub fn merge_inplace(&mut self, other: &TypeSymbolTable) {
    //     for (k, v) in self.var_type_mapping.iter_mut() {
    //         v.moved = other.var_type_mapping.get(k).unwrap().moved;
    //     }
    // }
    //
    // pub fn merge(mut self, other: &TypeSymbolTable) -> TypeSymbolTable {
    //     for (k, v) in self.var_type_mapping.iter_mut() {
    //         v.moved = other.var_type_mapping.get(k).unwrap().moved;
    //     }
    //     self
    // }
}

impl<'a> IntoIterator for &'a TypeSymbolTable {
    type Item = (&'a String, &'a TypeMapping);
    type IntoIter = Iter<'a, String, TypeMapping>;

    fn into_iter(self) -> Self::IntoIter {
        (&self.var_type_mapping).iter()
    }
}
