use crate::ast::expr::{Expr, IdentExpr, Member, PlaceExpr};
use crate::ast::ty::{StructTy, Ty};
use crate::symbol_table::tracked_ty::{OwnershipState, TrackedStructTy, TrackedTy};
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
                (Ty::from(&mapping.ty) == *ty && mapping.ty.movable())
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

    pub fn get_tracked_ty(&mut self, expr: &Expr) -> Option<&mut TrackedTy> {
        match expr {
            Expr::Field(expr) => {
                let ty = self.get_tracked_ty(&expr.base)?;
                return match (ty, &expr.member) {
                    (TrackedTy::Tuple(ty), Member::Unnamed(index)) => {
                        Some(&mut ty.tuple[*index])
                    }
                    (TrackedTy::Struct(TrackedStructTy::Tuple(ty)), Member::Unnamed(index)) => {
                        Some(&mut ty.fields.tuple[*index])
                    }
                    (TrackedTy::Struct(TrackedStructTy::Field(ty)), Member::Named(name)) => {
                        Some(&mut ty.fields.iter_mut().find(|field_def| &field_def.name == name).unwrap().ty)
                    }
                    _ => None
                };
            }
            Expr::Ident(expr) => {
                let mapping = self.var_type_mapping.get_mut(&expr.name).unwrap();
                Some(&mut mapping.ty)
            }
            _ => None,
        }
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
            Expr::Field(field_expr) => {
                let ty = self.get_tracked_ty(&field_expr.clone().into());
                if let Some(ty) = ty {
                    if !ty.movable() {
                        return false;
                    }
                    ty.set_ownership_state(OwnershipState::Owned)
                };
                true
            }
            Expr::Reference(_) => {
                unimplemented!()
            }
        }
    }

    pub fn regain_ownership(&mut self, place: &PlaceExpr) {
        let ty = self.get_tracked_ty(&place.clone().into());
        if let Some(ty) = ty {
            let ownership = ty.ownership_state();
            assert!(ownership == OwnershipState::Moved || ownership == OwnershipState::NotApplicable || ownership == OwnershipState::PartiallyOwned);
            if matches!(ty.ownership_state(), OwnershipState::NotApplicable) {
                return;
            }
            ty.set_ownership_state(OwnershipState::Owned);
        }
        // match place {
        //     Expr::Field(expr) => {
        //         let ty = self.regain_ownership(&expr.base)?;
        //         match ty {
        //             TrackedTy::Tuple(_) => {}
        //             TrackedTy::Struct(_) => {}
        //             _ => {}
        //         };
        //         None
        //     }
        //     Expr::Ident(expr) => {
        //         let mapping = self.var_type_mapping.get_mut(&expr.name).unwrap();
        //         if matches!(mapping.ty.ownership_state(), OwnershipState::NotApplicable) {
        //             return None;
        //         }
        //         mapping.ty.set_ownership_state(OwnershipState::Owned);
        //         Some(&mut mapping.ty)
        //     }
        //     _ => None,
        // }
    }

    pub fn update(&mut self, other: &TypeSymbolTable) {
        for (key, v) in self.var_type_mapping.iter_mut() {
            v.ty.set_ownership_state(other.get_var_type(key).unwrap().ownership_state())
        }
    }

    pub fn update_branch(&mut self, branch1: &TypeSymbolTable, branch2: &Option<TypeSymbolTable>) {
        for (key, v) in self.var_type_mapping.iter_mut() {
            let prev_ownership = v.ty.ownership_state();
            let branch1_ownership = branch1.get_var_type(key).unwrap().ownership_state();
            let branch2_ownership = if let Some(branch2) = branch2 {
                branch2.get_var_type(key).unwrap().ownership_state()
            } else {
                prev_ownership
            };
            if prev_ownership == OwnershipState::NotApplicable {
                assert_eq!(branch1_ownership, OwnershipState::NotApplicable);
                assert_eq!(branch2_ownership, OwnershipState::NotApplicable);
                continue;
            }
            if matches!(branch1_ownership, OwnershipState::Moved)
                || matches!(branch2_ownership, OwnershipState::Moved)
            {
                v.ty.set_ownership_state(OwnershipState::Moved)
            }
            if matches!(
                (branch1_ownership, branch2_ownership),
                (OwnershipState::Owned, OwnershipState::Owned)
            ) {
                v.ty.set_ownership_state(OwnershipState::Owned)
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
