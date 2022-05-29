use crate::ast::ty::{
    GArrayTy, GFieldDef, GFieldStructTy, GReferenceTy, GStructTy, GTupleStructTy, GTupleTy, GTy,
    PrimTy, TupleTy, Ty,
};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum OwnershipState {
    NotApplicable,
    Owned,
    PartiallyOwned,
    Unowned,
}

// #[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
// struct TrackedTy {
//     ty: GTy<TrackedTy>,
//     ownership_state: OwnershipState,
// }

type TrackedTy = GTy<OwnershipState>;

// impl Ty {
//     pub fn to_tracked(&self) -> TrackedTy {
//         let ty: GTy<TrackedTy> = match &self.0 {
//             GTy::Unit => GTy::Unit.clone(),
//             GTy::Prim(ty) => GTy::Prim(ty.clone()),
//             GTy::Tuple(ty) => GTy::Tuple(GTupleTy {
//                 tuple: ty.tuple.iter().map(Ty::to_tracked).collect(),
//             }),
//             GTy::Array(ty) => GTy::Array(GArrayTy {
//                 base_ty: Box::new(ty.base_ty.to_tracked()),
//                 len: ty.len,
//             }),
//             GTy::Struct(ty) => GTy::Struct(match ty {
//                 GStructTy::Field(ty) => GStructTy::Field(GFieldStructTy {
//                     name: ty.name.clone(),
//                     is_copy: ty.is_copy(),
//                     is_clone: ty.is_clone(),
//                     fields: ty
//                         .fields
//                         .iter()
//                         .map(|field| GFieldDef {
//                             name: field.name.clone(),
//                             ty: Box::new(field.ty.to_tracked()),
//                         })
//                         .collect(),
//                     lifetimes: ty.lifetimes.clone(),
//                 }),
//                 GStructTy::Tuple(ty) => GStructTy::Tuple(GTupleStructTy {
//                     name: ty.name.clone(),
//                     is_copy: ty.is_copy(),
//                     is_clone: ty.is_clone(),
//                     fields: ty.fields.tuple.iter().map(Ty::to_tracked).collect(),
//                     lifetimes: ty.lifetimes.clone(),
//                 }),
//             }),
//             GTy::Reference(ty) => GTy::Reference(GReferenceTy {
//                 mutability: ty.mutability,
//                 lifetime: ty.lifetime.clone(),
//                 elem: Box::new(ty.elem.to_tracked()),
//             }),
//         };
//         TrackedTy {
//             ty,
//             ownership_state: if self.is_copy() {
//                 OwnershipState::NotApplicable
//             } else {
//                 OwnershipState::Owned
//             },
//         }
//     }
// }
