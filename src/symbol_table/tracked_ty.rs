use crate::ast::ty::{
    ArrayTy, FieldStructTy, GArrayTy, GFieldDef, GFieldStructTy, GReferenceTy, GStructTy,
    GTupleStructTy, GTupleTy, GTy, ReferenceTy, StructTy, TupleStructTy, TupleTy, Ty,
};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum OwnershipState {
    NotApplicable,
    Owned,
    PartiallyOwned,
    Moved,
}

impl OwnershipState {
    pub fn moveable(self) -> bool {
        matches!(self, OwnershipState::NotApplicable | OwnershipState::Owned)
    }
}

macro_rules! ownership {
    ($value: ident) => {
        if $value.is_copy() {
            OwnershipState::NotApplicable
        } else {
            OwnershipState::Owned
        }
    };
}

pub type TrackedTy = GTy<OwnershipState>;

impl TrackedTy {
    pub fn ownership_state(&self) -> OwnershipState {
        match self {
            TrackedTy::Unit | TrackedTy::Prim(_) => OwnershipState::NotApplicable,
            TrackedTy::Tuple(ty) => ty.assoc,
            TrackedTy::Array(ty) => ty.assoc,
            TrackedTy::Struct(ty) => match ty {
                GStructTy::Field(ty) => ty.assoc,
                GStructTy::Tuple(ty) => ty.assoc,
            },
            TrackedTy::Reference(ty) => ty.assoc,
        }
    }

    pub fn moveable(&self) -> bool {
        self.ownership_state().moveable()
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

impl From<&TrackedTy> for Ty {
    fn from(ty: &TrackedTy) -> Ty {
        match ty {
            GTy::Unit => Ty::Unit,
            GTy::Prim(ty) => Ty::Prim(ty.clone()),
            GTy::Tuple(ty) => Ty::Tuple(ty.into()),
            GTy::Array(ty) => Ty::Array(ty.into()),
            GTy::Struct(ty) => Ty::Struct(ty.into()),
            GTy::Reference(ty) => Ty::Reference(ty.into()),
        }
    }
}

pub type TrackedTupleTy = GTupleTy<OwnershipState>;

impl TupleTy {
    pub fn to_tracked(&self) -> TrackedTupleTy {
        GTupleTy {
            tuple: self.tuple.iter().map(Ty::to_tracked).collect(),
            assoc: ownership!(self),
        }
    }
}

impl From<&TrackedTupleTy> for TupleTy {
    fn from(ty: &TrackedTupleTy) -> TupleTy {
        TupleTy::new(ty.tuple.iter().map(From::from).collect())
    }
}

impl<'a> IntoIterator for &'a TrackedTupleTy {
    type Item = &'a TrackedTy;
    type IntoIter = std::slice::Iter<'a, TrackedTy>;

    fn into_iter(self) -> Self::IntoIter {
        self.tuple.iter()
    }
}

pub type TrackedArrayTy = GArrayTy<OwnershipState>;

impl ArrayTy {
    pub fn to_tracked(&self) -> TrackedArrayTy {
        GArrayTy {
            base_ty: Box::new(self.base_ty.to_tracked()),
            len: self.len,
            assoc: ownership!(self),
        }
    }
}

impl From<&TrackedArrayTy> for ArrayTy {
    fn from(ty: &TrackedArrayTy) -> ArrayTy {
        ArrayTy::new((&*ty.base_ty).into(), ty.len)
    }
}

impl TrackedArrayTy {
    pub fn iter(&self) -> impl Iterator<Item = TrackedTy> {
        std::iter::repeat(*self.base_ty.clone()).take(self.len)
    }
}

pub type TrackedStructTy = GStructTy<OwnershipState>;

impl StructTy {
    pub fn to_tracked(&self) -> TrackedStructTy {
        match self {
            StructTy::Field(ty) => GStructTy::Field(ty.to_tracked()),
            StructTy::Tuple(ty) => GStructTy::Tuple(ty.to_tracked()),
        }
    }
}

impl From<&TrackedStructTy> for StructTy {
    fn from(ty: &TrackedStructTy) -> StructTy {
        match ty {
            TrackedStructTy::Field(ty) => StructTy::Field(ty.into()),
            TrackedStructTy::Tuple(ty) => StructTy::Tuple(ty.into()),
        }
    }
}

pub type TrackedFieldStructTy = GFieldStructTy<OwnershipState>;

impl FieldStructTy {
    pub fn to_tracked(&self) -> TrackedFieldStructTy {
        GFieldStructTy {
            name: self.name.clone(),
            is_copy: self.is_copy,
            is_clone: self.is_clone,
            fields: self
                .fields
                .iter()
                .map(|field| GFieldDef {
                    name: field.name.clone(),
                    ty: Box::new(field.ty.to_tracked()),
                })
                .collect(),
            lifetimes: self.lifetimes.clone(),
            assoc: ownership!(self),
        }
    }
}

impl From<&TrackedFieldStructTy> for FieldStructTy {
    fn from(ty: &TrackedFieldStructTy) -> FieldStructTy {
        FieldStructTy {
            name: ty.name.clone(),
            is_copy: ty.is_copy,
            is_clone: ty.is_clone,
            fields: ty
                .fields
                .iter()
                .map(|field| GFieldDef {
                    name: field.name.clone(),
                    ty: Box::new((&*field.ty).into()),
                })
                .collect(),
            lifetimes: ty.lifetimes.clone(),
            assoc: (),
        }
    }
}

pub type TrackedTupleStructTy = GTupleStructTy<OwnershipState>;

impl TupleStructTy {
    pub fn to_tracked(&self) -> TrackedTupleStructTy {
        GTupleStructTy {
            name: self.name.clone(),
            is_copy: self.is_copy,
            is_clone: self.is_clone,
            fields: self.fields.to_tracked(),
            lifetimes: self.lifetimes.clone(),
            assoc: ownership!(self),
        }
    }
}

impl From<&TrackedTupleStructTy> for TupleStructTy {
    fn from(ty: &TrackedTupleStructTy) -> TupleStructTy {
        TupleStructTy {
            name: ty.name.clone(),
            is_copy: ty.is_copy,
            is_clone: ty.is_clone,
            fields: (&ty.fields).into(),
            lifetimes: ty.lifetimes.clone(),
            assoc: (),
        }
    }
}

pub type TrackedReferenceTy = GReferenceTy<OwnershipState>;

impl ReferenceTy {
    pub fn to_tracked(&self) -> TrackedReferenceTy {
        GReferenceTy {
            elem: Box::new(self.elem.to_tracked()),
            mutability: self.mutability,
            lifetime: self.lifetime.clone(),
            assoc: ownership!(self),
        }
    }
}

impl From<&TrackedReferenceTy> for ReferenceTy {
    fn from(ty: &TrackedReferenceTy) -> ReferenceTy {
        ReferenceTy {
            elem: Box::new((&*ty.elem).into()),
            mutability: ty.mutability,
            lifetime: ty.lifetime.clone(),
            assoc: (),
        }
    }
}
