//! Tracked type symbol table used for ownership tracking.

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
    pub fn movable(self) -> bool {
        matches!(self, OwnershipState::NotApplicable | OwnershipState::Owned)
    }

    pub fn partially_movable(self) -> bool {
        matches!(self, OwnershipState::NotApplicable | OwnershipState::Owned | OwnershipState::PartiallyOwned)
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
        if self.is_copy() {
            return OwnershipState::NotApplicable;
        }
        match self {
            TrackedTy::Unit | TrackedTy::Prim(_) => OwnershipState::NotApplicable,
            TrackedTy::Tuple(ty) => {
                if ty.assoc == OwnershipState::Moved || ty.assoc == OwnershipState::PartiallyOwned {
                    return ty.assoc;
                }
                if ty
                    .tuple
                    .iter()
                    .map(TrackedTy::ownership_state)
                    .any(|state| {
                        state == OwnershipState::Moved || state == OwnershipState::PartiallyOwned
                    })
                {
                    return OwnershipState::PartiallyOwned;
                }
                OwnershipState::Owned
            }
            TrackedTy::Array(ty) => ty.assoc,
            TrackedTy::Struct(ty) => match ty {
                GStructTy::Field(ty) => {
                    if ty.assoc == OwnershipState::Moved
                        || ty.assoc == OwnershipState::PartiallyOwned
                    {
                        return ty.assoc;
                    }
                    if ty
                        .fields
                        .iter()
                        .map(|field_def| field_def.ty.ownership_state())
                        .any(|state| {
                            state == OwnershipState::Moved
                                || state == OwnershipState::PartiallyOwned
                        })
                    {
                        return OwnershipState::PartiallyOwned;
                    }
                    OwnershipState::Owned
                }
                GStructTy::Tuple(ty) => {
                    if ty.assoc == OwnershipState::Moved
                        || ty.assoc == OwnershipState::PartiallyOwned
                    {
                        return ty.assoc;
                    }
                    if ty
                        .fields
                        .tuple
                        .iter()
                        .map(TrackedTy::ownership_state)
                        .any(|state| {
                            state == OwnershipState::Moved
                                || state == OwnershipState::PartiallyOwned
                        })
                    {
                        return OwnershipState::PartiallyOwned;
                    }
                    OwnershipState::Owned
                }
            },
            TrackedTy::Reference(ty) => ty.assoc,
        }
    }

    pub fn set_ownership_state(&mut self, state: OwnershipState) {
        // TODO: Recursive moves
        match self {
            TrackedTy::Unit | TrackedTy::Prim(_) => {}
            TrackedTy::Tuple(ty) => ty.set_ownership_state(state),
            TrackedTy::Array(ty) => ty.set_ownership_state(state),
            TrackedTy::Struct(ty) => ty.set_ownership_state(state),
            TrackedTy::Reference(_) => {
                unimplemented!()
            }
        }
    }

    pub fn update(&mut self, other1: &TrackedTy, other2: Option<&TrackedTy>, if_stmt: bool) {
        let prev_ownership = self.ownership_state();
        let other1_ownership = other1.ownership_state();
        let other2_ownership = if let Some(other2) = other2 {
            other2.ownership_state()
        } else {
            if if_stmt {
                prev_ownership
            } else {
                other1_ownership
            }
        };
        if prev_ownership == OwnershipState::NotApplicable {
            assert_eq!(other1_ownership, OwnershipState::NotApplicable);
            assert_eq!(other2_ownership, OwnershipState::NotApplicable);
            return;
        }
        if matches!(other1_ownership, OwnershipState::Moved)
            || matches!(other2_ownership, OwnershipState::Moved)
        {
            self.set_ownership_state(OwnershipState::Moved);
            return;
        }
        if matches!(
            (other1_ownership, other2_ownership),
            (OwnershipState::Owned, OwnershipState::Owned)
        ) {
            self.set_ownership_state(OwnershipState::Owned);
            return;
        }
        match self {
            TrackedTy::Tuple(ty_self) => {
                let ty_1 = match other1 {
                    TrackedTy::Tuple(ty_1) => &ty_1.tuple,
                    _ => panic!(),
                };
                let ty_2 = match other2 {
                    Some(TrackedTy::Tuple(ty_2)) => Some(&ty_2.tuple),
                    None => None,
                    _ => panic!(),
                };
                ty_self
                    .tuple
                    .iter_mut()
                    .enumerate()
                    .for_each(|(i, ty)| ty.update(&ty_1[i], ty_2.map(|tys| &tys[i]), if_stmt));
            }
            TrackedTy::Struct(ty_self) => match ty_self {
                TrackedStructTy::Field(ty_self) => {
                    let ty_1: &Vec<GFieldDef<OwnershipState>> = match other1 {
                        TrackedTy::Struct(TrackedStructTy::Field(ty_1)) => &ty_1.fields,
                        _ => panic!(),
                    };
                    let ty_2: Option<&Vec<GFieldDef<OwnershipState>>> = match other2 {
                        Some(TrackedTy::Struct(TrackedStructTy::Field(ty_2))) => Some(&ty_2.fields),
                        None => None,
                        _ => panic!(),
                    };
                    ty_self
                        .fields
                        .iter_mut()
                        .enumerate()
                        .for_each(|(i, field)| {
                            field
                                .ty
                                .update(&ty_1[i].ty, ty_2.map(|fields| &*fields[i].ty), if_stmt)
                        })
                }
                TrackedStructTy::Tuple(ty_self) => {
                    let ty_1 = match other1 {
                        TrackedTy::Struct(TrackedStructTy::Tuple(ty_1)) => &ty_1.fields.tuple,
                        _ => panic!(),
                    };
                    let ty_2 = match other2 {
                        Some(TrackedTy::Struct(TrackedStructTy::Tuple(ty_2))) => {
                            Some(&ty_2.fields.tuple)
                        }
                        None => None,
                        _ => panic!(),
                    };
                    ty_self
                        .fields
                        .tuple
                        .iter_mut()
                        .enumerate()
                        .for_each(|(i, ty)| ty.update(&ty_1[i], ty_2.map(|tys| &tys[i]), if_stmt));
                }
            },
            TrackedTy::Reference(_reference_ty) => {
                panic!()
            }
            _ => {}
        }
    }

    pub fn movable(&self) -> bool {
        self.ownership_state().movable()
    }

    pub fn partially_movable(&self) -> bool {
        self.ownership_state().partially_movable()
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

impl TrackedTupleTy {
    pub fn set_ownership_state(&mut self, state: OwnershipState) {
        if self.is_copy() {
            return
        }
        self.assoc = state;
        self.tuple
            .iter_mut()
            .for_each(|ty| ty.set_ownership_state(state));
    }
}

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

impl TrackedArrayTy {
    pub fn set_ownership_state(&mut self, state: OwnershipState) {
        if self.is_copy() {
            return
        }
        self.assoc = state;
        self.base_ty.set_ownership_state(state);
    }
}

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

pub type TrackedStructTy = GStructTy<OwnershipState>;

impl TrackedStructTy {
    pub fn set_ownership_state(&mut self, state: OwnershipState) {
        match self {
            TrackedStructTy::Field(ty) => ty.set_ownership_state(state),
            TrackedStructTy::Tuple(ty) => ty.set_ownership_state(state),
        }
    }
}

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

impl TrackedFieldStructTy {
    pub fn set_ownership_state(&mut self, state: OwnershipState) {
        self.assoc = state;
        self.fields
            .iter_mut()
            .for_each(|field| field.ty.set_ownership_state(state));
    }
}

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

impl TrackedTupleStructTy {
    pub fn set_ownership_state(&mut self, state: OwnershipState) {
        self.assoc = state;
        self.fields.set_ownership_state(state);
    }
}

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
