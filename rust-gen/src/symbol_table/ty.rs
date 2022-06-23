use crate::ast::expr::{Expr, IdentExpr, Member, PlaceExpr};
use crate::ast::ty::Ty;
use crate::symbol_table::tracked_ty::{OwnershipState, TrackedStructTy, TrackedTy};
use archery::RcK;
use rpds::map::red_black_tree_map::Iter;
use rpds::RedBlackTreeMap;

#[derive(Debug, Clone)]
pub struct TypeMapping {
    pub ty: TrackedTy,
    mutable: bool,
}

#[derive(Debug, Default, Clone)]
pub struct TypeSymbolTable {
    var_type_mapping: RedBlackTreeMap<String, TypeMapping>,
}

impl TypeSymbolTable {
    pub fn add_var(&mut self, key: String, ty: &Ty, mutable: bool) {
        self.var_type_mapping = self.var_type_mapping.insert(
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
    pub fn get_names_by_type(&self, ty: &Ty) -> Vec<String> {
        self.var_type_mapping
            .iter()
            .filter_map(|(name, mapping)| {
                (Ty::from(&mapping.ty) == *ty && mapping.ty.movable()).then(|| name.clone())
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
                    (TrackedTy::Tuple(ty), Member::Unnamed(index)) => Some(&mut ty.tuple[*index]),
                    (TrackedTy::Struct(TrackedStructTy::Tuple(ty)), Member::Unnamed(index)) => {
                        Some(&mut ty.fields.tuple[*index])
                    }
                    (TrackedTy::Struct(TrackedStructTy::Field(ty)), Member::Named(name)) => Some(
                        &mut ty
                            .fields
                            .iter_mut()
                            .find(|field_def| &field_def.name == name)
                            .unwrap()
                            .ty,
                    ),
                    _ => None,
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
            | Expr::Struct(_)
            | Expr::Assign(_)
            | Expr::FunctionCall(_) => true,
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
            Expr::Field(field_expr) => {
                let ty = self.get_tracked_ty(&field_expr.clone().into());
                if let Some(ty) = ty {
                    if !ty.movable() {
                        return false;
                    }
                    ty.set_ownership_state(OwnershipState::Moved);
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
            if matches!(ty.ownership_state(), OwnershipState::NotApplicable) {
                return;
            }

            ty.set_ownership_state(OwnershipState::Owned);
        }
    }

    pub fn update(&mut self, other: &TypeSymbolTable) {
        let keys: Vec<String> = self.var_type_mapping.keys().cloned().collect();
        for key in &keys {
            self.var_type_mapping.get_mut(key).unwrap().ty.update(
                &other.get_var_type(key).unwrap(),
                None,
                false,
            )
        }
    }

    pub fn update_branch(&mut self, branch1: &TypeSymbolTable, branch2: &Option<TypeSymbolTable>) {
        let keys: Vec<String> = self.var_type_mapping.keys().cloned().collect();
        for key in &keys {
            self.var_type_mapping.get_mut(key).unwrap().ty.update(
                &branch1.get_var_type(key).unwrap(),
                branch2
                    .as_ref()
                    .map(|branch2| branch2.get_var_type(key).unwrap())
                    .as_ref(),
                true,
            )
        }
    }
}

impl<'a> IntoIterator for &'a TypeSymbolTable {
    type Item = (&'a String, &'a TypeMapping);
    type IntoIter = Iter<'a, String, TypeMapping, RcK>;

    fn into_iter(self) -> Self::IntoIter {
        (&self.var_type_mapping).into_iter()
    }
}
