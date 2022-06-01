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

    pub fn get_var_type(&self, key: &str) -> Option<TrackedTy> {
        Some(self.var_type_mapping.get(key)?.ty.clone())
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
                (mapping.mutable && Ty::from(&mapping.ty) == *ty)
                    .then(|| IdentExpr { name: name.clone() })
            })
            .collect()
    }

    pub fn move_expr(&mut self, expr: &Expr) -> bool {
        match expr {
            Expr::Literal(_)
            | Expr::Binary(_)
            | Expr::Unary(_)
            | Expr::Cast(_)
            | Expr::If(_)
            | Expr::Block(_)
            | Expr::Tuple(_)
            | Expr::Array(_)
            | Expr::Index(_)
            | Expr::Struct(_) => true,
            Expr::Ident(ident) => {
                let mapping = self.var_type_mapping.get_mut(&ident.name).unwrap();
                match mapping.ty.ownership_state() {
                    OwnershipState::NotApplicable => true,
                    OwnershipState::Owned => {
                        mapping.ty.set_ownership_state(OwnershipState::Moved);
                        true
                    }
                    OwnershipState::PartiallyOwned | OwnershipState::Moved => false,
                }
            }
            Expr::Assign(_) => true,
            Expr::Field(_field_expr) => {
                // Find ident of original
                // recursively update chain
                // field_expr.base
                false
            }
            Expr::Reference(_) => {
                unimplemented!()
            }
        }
    }

    pub fn regain_ownership(&mut self, place: &Expr) -> Option<&mut TrackedTy>{
        match place {
            Expr::Field(expr) => {
                let ty = self.regain_ownership(&expr.base)?;
                match ty {
                    TrackedTy::Tuple(_) => {}
                    TrackedTy::Struct(_) => {}
                    _ => {}
                };
                None
            }
            Expr::Ident(expr) => {
                let mapping = self.var_type_mapping.get_mut(&expr.name).unwrap();
                if matches!(mapping.ty.ownership_state(), OwnershipState::NotApplicable) {
                    return None;
                }
                mapping.ty.set_ownership_state(OwnershipState::Owned);
                Some(&mut mapping.ty)
            }
            _ => {
                None
            }
        }
    }

    pub fn update(&mut self, other: &TypeSymbolTable) {
        for (key, v) in self.var_type_mapping.iter_mut() {
            v.ty.set_ownership_state(other.get_var_type(key).unwrap().ownership_state())
        }
    }

    pub fn update_branch(&mut self, branch1: &TypeSymbolTable, branch2: &Option<TypeSymbolTable>) {
        for (key, v) in self.var_type_mapping.iter_mut() {
            let branch1_ownership = branch1.get_var_type(key).unwrap().ownership_state();
            let branch2_ownership = if let Some(branch2) = branch2 {
                branch2.get_var_type(key).unwrap().ownership_state()
            } else {
                v.ty.ownership_state()
            };
            if matches!(branch1_ownership, OwnershipState::Moved)
                || matches!(branch2_ownership, OwnershipState::Moved)
            {
                v.ty.set_ownership_state(OwnershipState::Moved)
            }
        }
    }
}

impl<'a> IntoIterator for &'a TypeSymbolTable {
    type Item = (&'a String, &'a TypeMapping);
    type IntoIter = Iter<'a, String, TypeMapping>;

    fn into_iter(self) -> Self::IntoIter {
        (&self.var_type_mapping).iter()
    }
}
