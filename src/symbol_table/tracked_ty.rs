use crate::ast::ty::{ArrayTy, FieldStructTy, GArrayTy, GFieldDef, GFieldStructTy, GReferenceTy, GStructTy, GTupleStructTy, GTupleTy, GTy,  ReferenceTy, StructTy, TupleStructTy, TupleTy, Ty};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum OwnershipState {
    NotApplicable,
    Owned,
    PartiallyOwned,
    Moved,
}

macro_rules! ownership {
    ($value: ident) => {
        if $value.is_copy() {
            OwnershipState::NotApplicable
        } else {
            OwnershipState::Owned
        }
    }
}

type TrackedTy = GTy<OwnershipState>;

impl TrackedTy {
    pub fn ownership_state(&self) -> OwnershipState {
        match self {
            TrackedTy::Unit | TrackedTy::Prim(_) => {
                OwnershipState::NotApplicable
            }
            TrackedTy::Tuple(ty) => ty.assoc,
            TrackedTy::Array(ty) => ty.assoc,
            TrackedTy::Struct(ty) => match ty {
                GStructTy::Field(ty) => ty.assoc,
                GStructTy::Tuple(ty) => ty.assoc,
            },
            TrackedTy::Reference(ty) => ty.assoc,
        }
    }
}

impl Ty {
    pub fn to_tracked(&self) -> TrackedTy {
        match self {
            Ty::Unit => GTy::Unit,
            Ty::Prim(ty) => GTy::Prim(ty.clone()),
            Ty::Tuple(ty) => GTy::Tuple(ty.to_tracked()),
            Ty::Array(ty) => GTy::Array(ty.to_tracked()),
            Ty::Struct(ty) => GTy::Struct(ty.to_tracked()),
            Ty::Reference(ty) => GTy::Reference(ty.to_tracked()),
        }
    }
}

type TrackedTupleTy = GTupleTy<OwnershipState>;

impl TupleTy {
    pub fn to_tracked(&self) -> TrackedTupleTy {
        GTupleTy {
            tuple: self.tuple.iter().map(Ty::to_tracked).collect(),
            assoc: ownership!(self)
        }
    }
}

type TrackedArrayTy = GArrayTy<OwnershipState>;

impl ArrayTy {
    pub fn to_tracked(&self) -> TrackedArrayTy {
        GArrayTy {
            base_ty: Box::new(self.base_ty.to_tracked()),
            len: self.len,
            assoc: ownership!(self)
        }
    }
}

type TrackedStructTy = GStructTy<OwnershipState>;

impl StructTy {
    pub fn to_tracked(&self) -> TrackedStructTy {
        match self {
            StructTy::Field(ty) => GStructTy::Field(ty.to_tracked()),
            StructTy::Tuple(ty) => GStructTy::Tuple(ty.to_tracked()),
        }
    }
}

type TrackedFieldStructTy = GFieldStructTy<OwnershipState>;

impl FieldStructTy {
    pub fn to_tracked(&self) -> TrackedFieldStructTy {
        GFieldStructTy {
            name: self.name.clone(),
            is_copy: self.is_copy,
            is_clone: self.is_clone,
            fields: self.fields.iter().map(|field| GFieldDef {
                name: field.name.clone(),
                ty: Box::new(field.ty.to_tracked()),
            }).collect(),
            lifetimes: self.lifetimes.clone(),
            assoc: ownership!(self)
        }
    }
}

type TrackedTupleStructTy = GTupleStructTy<OwnershipState>;

impl TupleStructTy {
    pub fn to_tracked(&self) -> TrackedTupleStructTy {
        GTupleStructTy {
            name: self.name.clone(),
            is_copy: self.is_copy,
            is_clone: self.is_clone,
            fields: self.fields.to_tracked(),
            lifetimes: self.lifetimes.clone(),
            assoc: ownership!(self)
        }
    }
}

type TrackedReferenceTy = GReferenceTy<OwnershipState>;

impl ReferenceTy {
    pub fn to_tracked(&self) -> TrackedReferenceTy {
        GReferenceTy {
            elem: Box::new(self.elem.to_tracked()),
            mutability: self.mutability,
            lifetime: self.lifetime.clone(),
            assoc: ownership!(self)
        }
    }
}