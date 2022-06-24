//! Type nodes.

use crate::ast::expr::LitIntTy;
use crate::context::Context;

use rand::Rng;
use serde::{Deserialize, Serialize};

use std::collections::BTreeSet;
use std::fmt::Debug;

pub type Ty = GTy<()>;

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Serialize, Deserialize)]
pub enum GTy<A> {
    Unit,
    Prim(PrimTy),
    Tuple(GTupleTy<A>), // TODO: Add more types such as Arrays, Slices, Ptrs (https://doc.rust-lang.org/nightly/nightly-rustc/rustc_middle/ty/sty/enum.TyKind.html)
    Array(GArrayTy<A>),
    Struct(GStructTy<A>),
    Reference(GReferenceTy<A>),
}

impl<A> GTy<A> {
    /// Returns whether a given type is a primitive integer.
    pub fn is_primitive_number(&self) -> bool {
        // TODO: Add floats
        matches!(self, GTy::Prim(PrimTy::Int(_) | PrimTy::UInt(_)))
    }

    /// Checks to see if a given cast is compatible.
    pub fn compatible_cast(&self, target_type: &Ty) -> bool {
        // TODO: More thorough casting
        self.is_primitive_number() && target_type.is_primitive_number()
    }

    pub fn unit_type() -> Ty {
        GTy::Unit
    }

    /// Returns whether a given type is the unit type.
    /// Both `Ty::Unit` and `Ty::Tuple(vec![])` correspond to the unit type.
    pub fn is_unit(&self) -> bool {
        match &self {
            GTy::Unit => true,
            GTy::Tuple(tuple_ty) => tuple_ty.tuple.is_empty(),
            _ => false,
        }
    }

    /// Returns the composite depth of a type.
    pub fn composite_depth(&self) -> usize {
        match self {
            GTy::Array(array_ty) => array_ty.composite_depth(),
            GTy::Tuple(tuple_ty) => tuple_ty.composite_depth(),
            GTy::Struct(struct_ty) => struct_ty.composite_depth(),
            _ => 0,
        }
    }

    /// Returns the array depth of a type.
    pub fn array_depth(&self) -> usize {
        match self {
            GTy::Array(array_ty) => array_ty.array_depth(),
            _ => 0,
        }
    }

    /// Returns the tuple depth of a type.
    pub fn tuple_depth(&self) -> usize {
        match self {
            GTy::Tuple(tuple_ty) => tuple_ty.tuple_depth(),
            _ => 0,
        }
    }

    /// Returns the struct depth of a type.
    pub fn struct_depth(&self) -> usize {
        match self {
            GTy::Struct(struct_ty) => struct_ty.struct_depth(),
            _ => 0,
        }
    }

    pub fn require_lifetime(&self) -> bool {
        match self {
            GTy::Unit => false,
            GTy::Prim(ty) => ty.require_lifetime(),
            GTy::Tuple(ty) => ty.require_lifetime(),
            GTy::Array(ty) => ty.require_lifetime(),
            GTy::Struct(ty) => ty.require_lifetime(),
            GTy::Reference(ty) => ty.require_lifetime(),
        }
    }

    pub fn is_copy(&self) -> bool {
        let is_copy = match self {
            GTy::Unit => true,
            GTy::Prim(ty) => ty.is_copy(),
            GTy::Tuple(ty) => ty.is_copy(),
            GTy::Array(ty) => ty.is_copy(),
            GTy::Struct(ty) => ty.is_copy(),
            GTy::Reference(ty) => ty.is_copy(),
        };
        if is_copy {
            assert!(self.is_clone());
        }
        is_copy
    }

    pub fn is_clone(&self) -> bool {
        match self {
            GTy::Unit => true,
            GTy::Prim(ty) => ty.is_clone(),
            GTy::Tuple(ty) => ty.is_clone(),
            GTy::Array(ty) => ty.is_clone(),
            GTy::Struct(ty) => ty.is_clone(),
            GTy::Reference(ty) => ty.is_clone(),
        }
    }
}

impl ToString for Ty {
    fn to_string(&self) -> String {
        match self {
            GTy::Unit => "()".to_string(),
            GTy::Prim(prim) => prim.to_string(),
            GTy::Tuple(tuple) => tuple.to_string(),
            GTy::Array(array) => array.to_string(),
            GTy::Struct(struct_ty) => struct_ty.to_string(),
            GTy::Reference(reference) => reference.to_string(),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Serialize, Deserialize)]
pub enum PrimTy {
    Bool,
    #[allow(dead_code)]
    Char,
    Int(IntTy),
    UInt(UIntTy),
    #[allow(dead_code)]
    Float(FloatTy),
    #[allow(dead_code)]
    Str,
}

impl ToString for PrimTy {
    fn to_string(&self) -> String {
        match self {
            PrimTy::Bool => "bool".to_string(),
            PrimTy::Char => "char".to_string(),
            PrimTy::Int(int_ty) => int_ty.to_string(),
            PrimTy::UInt(uint_ty) => uint_ty.to_string(),
            PrimTy::Float(float) => match float {
                FloatTy::F32 => "f32",
                FloatTy::F64 => "f64",
            }
            .to_string(),
            PrimTy::Str => "&str".to_string(),
        }
    }
}

impl From<PrimTy> for Ty {
    fn from(ty: PrimTy) -> Ty {
        GTy::Prim(ty)
    }
}

impl PrimTy {
    pub fn int_types(ctx: &Context) -> Vec<PrimTy> {
        let mut ints: Vec<PrimTy> = ctx
            .policy
            .prim_type_dist
            .iter()
            .filter_map(|(ty, _)| {
                if matches!(ty, PrimTy::Int(_)) || matches!(ty, PrimTy::UInt(_)) {
                    Some(ty)
                } else {
                    None
                }
            })
            .cloned()
            .collect();
        if ints.is_empty() {
            ints.append(&mut vec![
                PrimTy::Int(IntTy::I8),
                PrimTy::Int(IntTy::I16),
                PrimTy::Int(IntTy::I32),
                PrimTy::Int(IntTy::I64),
                PrimTy::Int(IntTy::I128),
                PrimTy::Int(IntTy::ISize),
                PrimTy::UInt(UIntTy::U8),
                PrimTy::UInt(UIntTy::U16),
                PrimTy::UInt(UIntTy::U32),
                PrimTy::UInt(UIntTy::U64),
                PrimTy::UInt(UIntTy::U128),
                PrimTy::UInt(UIntTy::USize),
            ]);
        }
        ints
    }

    pub fn require_lifetime(&self) -> bool {
        false
    }

    pub fn is_copy(&self) -> bool {
        true
    }

    pub fn is_clone(&self) -> bool {
        true
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Serialize, Deserialize)]
#[allow(dead_code)]
pub enum FloatTy {
    F32,
    F64,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Serialize, Deserialize)]
pub enum IntTy {
    ISize,
    I8,
    I16,
    I32,
    I64,
    I128,
}

impl From<IntTy> for Ty {
    fn from(ty: IntTy) -> Ty {
        GTy::Prim(ty.into())
    }
}

impl From<IntTy> for PrimTy {
    fn from(ty: IntTy) -> PrimTy {
        PrimTy::Int(ty)
    }
}

impl From<IntTy> for LitIntTy {
    fn from(ty: IntTy) -> LitIntTy {
        LitIntTy::Signed(ty)
    }
}

impl ToString for IntTy {
    fn to_string(&self) -> String {
        match self {
            IntTy::ISize => "isize",
            IntTy::I8 => "i8",
            IntTy::I16 => "i16",
            IntTy::I32 => "i32",
            IntTy::I64 => "i64",
            IntTy::I128 => "i128",
        }
        .to_owned()
    }
}

impl IntTy {
    pub fn rand_val(self, ctx: &mut Context) -> u128 {
        let rng = &mut ctx.rng;
        match self {
            IntTy::ISize => rng.gen::<isize>() as u128,
            IntTy::I8 => rng.gen::<i8>() as u128,
            IntTy::I16 => rng.gen::<i16>() as u128,
            IntTy::I32 => rng.gen::<i32>() as u128,
            IntTy::I64 => rng.gen::<i64>() as u128,
            IntTy::I128 => rng.gen::<i128>() as u128,
        }
    }

    pub fn cast_value(self, value: u128) -> u128 {
        match self {
            IntTy::ISize => value as isize as u128,
            IntTy::I8 => value as i8 as u128,
            IntTy::I16 => value as i16 as u128,
            IntTy::I32 => value as i32 as u128,
            IntTy::I64 => value as i64 as u128,
            IntTy::I128 => value as i128 as u128,
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Serialize, Deserialize)]
pub enum UIntTy {
    USize,
    U8,
    U16,
    U32,
    U64,
    U128,
}

impl From<UIntTy> for Ty {
    fn from(ty: UIntTy) -> Ty {
        GTy::Prim(ty.into())
    }
}

impl From<UIntTy> for PrimTy {
    fn from(ty: UIntTy) -> PrimTy {
        PrimTy::UInt(ty)
    }
}

impl From<UIntTy> for LitIntTy {
    fn from(ty: UIntTy) -> LitIntTy {
        LitIntTy::Unsigned(ty)
    }
}

impl ToString for UIntTy {
    fn to_string(&self) -> String {
        match self {
            UIntTy::USize => "usize",
            UIntTy::U8 => "u8",
            UIntTy::U16 => "u16",
            UIntTy::U32 => "u32",
            UIntTy::U64 => "u64",
            UIntTy::U128 => "u128",
        }
        .to_owned()
    }
}

impl UIntTy {
    pub fn rand_val(self, ctx: &mut Context) -> u128 {
        let rng = &mut ctx.rng;
        match self {
            UIntTy::USize => rng.gen::<usize>() as u128,
            UIntTy::U8 => rng.gen::<u8>() as u128,
            UIntTy::U16 => rng.gen::<u16>() as u128,
            UIntTy::U32 => rng.gen::<u32>() as u128,
            UIntTy::U64 => rng.gen::<u64>() as u128,
            UIntTy::U128 => rng.gen::<u128>(),
        }
    }

    pub fn cast_value(self, value: u128) -> u128 {
        match self {
            UIntTy::USize => value as usize as u128,
            UIntTy::U8 => value as u8 as u128,
            UIntTy::U16 => value as u16 as u128,
            UIntTy::U32 => value as u32 as u128,
            UIntTy::U64 => value as u64 as u128,
            UIntTy::U128 => value as u128 as u128,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Serialize, Deserialize)]
pub struct GTupleTy<A> {
    pub tuple: Vec<GTy<A>>,
    pub assoc: A,
}

impl<A> GTupleTy<A> {
    pub fn composite_depth(&self) -> usize {
        1 + self
            .tuple
            .iter()
            .map(GTy::composite_depth)
            .max()
            .unwrap_or_default()
    }

    /// Returns the depth of a tuple.
    pub fn tuple_depth(&self) -> usize {
        1 + self
            .tuple
            .iter()
            .map(GTy::tuple_depth)
            .max()
            .unwrap_or_default()
    }

    pub fn require_lifetime(&self) -> bool {
        self.tuple.iter().any(GTy::require_lifetime)
    }

    pub fn is_copy(&self) -> bool {
        self.tuple.iter().all(GTy::is_copy)
    }

    pub fn is_clone(&self) -> bool {
        self.tuple.iter().all(GTy::is_clone)
    }
}

pub type TupleTy = GTupleTy<()>;

impl From<TupleTy> for Ty {
    fn from(ty: TupleTy) -> Ty {
        GTy::Tuple(ty)
    }
}

impl TupleTy {
    pub fn new(tuple: Vec<Ty>) -> TupleTy {
        GTupleTy { tuple, assoc: () }
    }
}

impl<'a> IntoIterator for &'a TupleTy {
    type Item = &'a Ty;
    type IntoIter = std::slice::Iter<'a, Ty>;

    fn into_iter(self) -> Self::IntoIter {
        self.tuple.iter()
    }
}

impl ToString for TupleTy {
    fn to_string(&self) -> String {
        format!(
            "({})",
            self.tuple
                .iter()
                .map(std::string::ToString::to_string)
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

pub type ArrayTy = GArrayTy<()>;

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Serialize, Deserialize)]
pub struct GArrayTy<A> {
    pub base_ty: Box<GTy<A>>,
    pub len: usize,
    pub assoc: A,
}

impl<A: Clone> GArrayTy<A> {
    pub fn iter(&self) -> impl Iterator<Item = GTy<A>> {
        std::iter::repeat(*self.base_ty.clone()).take(self.len)
    }
}

impl<A> GArrayTy<A> {
    pub fn composite_depth(&self) -> usize {
        1 + self.base_ty.composite_depth()
    }

    pub fn array_depth(&self) -> usize {
        1 + self.base_ty.array_depth()
    }

    pub fn require_lifetime(&self) -> bool {
        self.base_ty.require_lifetime()
    }

    pub fn is_copy(&self) -> bool {
        self.base_ty.is_copy()
    }

    pub fn is_clone(&self) -> bool {
        self.base_ty.is_clone()
    }
}

impl From<ArrayTy> for Ty {
    fn from(ty: ArrayTy) -> Ty {
        GTy::Array(ty)
    }
}

impl ToString for ArrayTy {
    fn to_string(&self) -> String {
        format!("[{};{}]", self.base_ty.to_string(), self.len)
    }
}

impl ArrayTy {
    pub fn new(base_ty: Ty, len: usize) -> ArrayTy {
        GArrayTy {
            base_ty: Box::new(base_ty),
            len,
            assoc: (),
        }
    }
}

pub type StructTy = GStructTy<()>;

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Serialize, Deserialize)]
pub enum GStructTy<A> {
    Field(GFieldStructTy<A>),
    Tuple(GTupleStructTy<A>),
}

impl<A> GStructTy<A> {
    pub fn composite_depth(&self) -> usize {
        1 + match self {
            GStructTy::Field(field_struct) => field_struct
                .fields
                .iter()
                .map(|f| f.ty.composite_depth())
                .max()
                .unwrap_or_default(),
            GStructTy::Tuple(tuple_struct) => tuple_struct
                .fields
                .tuple
                .iter()
                .map(GTy::composite_depth)
                .max()
                .unwrap_or_default(),
        }
    }

    pub fn struct_depth(&self) -> usize {
        1 + match self {
            GStructTy::Field(field_struct) => field_struct
                .fields
                .iter()
                .map(|f| f.ty.struct_depth())
                .max()
                .unwrap_or_default(),
            GStructTy::Tuple(tuple_struct) => tuple_struct
                .fields
                .tuple
                .iter()
                .map(GTy::struct_depth)
                .max()
                .unwrap_or_default(),
        }
    }

    pub fn require_lifetime(&self) -> bool {
        match self {
            GStructTy::Field(struct_ty) => struct_ty.fields.iter().any(|x| x.ty.require_lifetime()),
            GStructTy::Tuple(struct_ty) => struct_ty.fields.require_lifetime(),
        }
    }

    pub fn lifetimes(&self) -> &BTreeSet<Lifetime> {
        match self {
            GStructTy::Field(struct_ty) => &struct_ty.lifetimes,
            GStructTy::Tuple(struct_ty) => &struct_ty.lifetimes,
        }
    }

    pub fn is_copy(&self) -> bool {
        match self {
            GStructTy::Field(struct_ty) => struct_ty.is_copy(),
            GStructTy::Tuple(struct_ty) => struct_ty.is_copy(),
        }
    }

    pub fn is_clone(&self) -> bool {
        match self {
            GStructTy::Field(struct_ty) => struct_ty.is_clone(),
            GStructTy::Tuple(struct_ty) => struct_ty.is_clone(),
        }
    }
}

impl From<StructTy> for Ty {
    fn from(ty: StructTy) -> Ty {
        GTy::Struct(ty)
    }
}

impl ToString for StructTy {
    fn to_string(&self) -> String {
        match self {
            StructTy::Field(field) => field.name.clone(),
            StructTy::Tuple(tuple) => tuple.name.clone(),
        }
    }
}

pub type FieldStructTy = GFieldStructTy<()>;

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Serialize, Deserialize)]
pub struct GFieldStructTy<A> {
    pub name: String,
    pub is_copy: bool,
    pub is_clone: bool,
    pub fields: Vec<GFieldDef<A>>,
    pub lifetimes: BTreeSet<Lifetime>,
    pub assoc: A,
}

impl<A> GFieldStructTy<A> {
    pub fn is_copy(&self) -> bool {
        assert!(!self.is_copy || self.fields.iter().all(|f| f.ty.is_copy()));
        self.is_copy
    }

    pub fn is_clone(&self) -> bool {
        assert!(!self.is_clone || self.fields.iter().all(|f| f.ty.is_clone()));
        self.is_clone
    }
}

impl ToString for FieldStructTy {
    fn to_string(&self) -> String {
        self.name.clone()
    }
}

impl From<FieldStructTy> for StructTy {
    fn from(field: FieldStructTy) -> StructTy {
        StructTy::Field(field)
    }
}

pub type FieldDef = GFieldDef<()>;

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Serialize, Deserialize)]
pub struct GFieldDef<A> {
    pub name: String,
    pub ty: Box<GTy<A>>,
}

impl ToString for FieldDef {
    fn to_string(&self) -> String {
        format!("{}: {}", self.name, self.ty.to_string())
    }
}

pub type TupleStructTy = GTupleStructTy<()>;

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Serialize, Deserialize)]
pub struct GTupleStructTy<A> {
    pub name: String,
    pub is_copy: bool,
    pub is_clone: bool,
    pub fields: GTupleTy<A>,
    pub lifetimes: BTreeSet<Lifetime>,
    pub assoc: A,
}

impl<A> GTupleStructTy<A> {
    pub fn is_copy(&self) -> bool {
        assert!(!self.is_copy || self.fields.is_copy());
        self.is_copy
    }

    pub fn is_clone(&self) -> bool {
        assert!(!self.is_clone || self.fields.is_clone());
        self.is_clone
    }
}

impl ToString for TupleStructTy {
    fn to_string(&self) -> String {
        self.name.clone()
    }
}

impl From<TupleStructTy> for StructTy {
    fn from(field: TupleStructTy) -> StructTy {
        StructTy::Tuple(field)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Serialize, Deserialize)]
pub struct Lifetime(pub String);

impl ToString for Lifetime {
    fn to_string(&self) -> String {
        format!("'{}", self.0)
    }
}

pub type ReferenceTy = GReferenceTy<()>;

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Serialize, Deserialize)]
pub struct GReferenceTy<A> {
    pub elem: Box<GTy<A>>,
    pub mutability: bool,
    pub lifetime: Option<Lifetime>,
    pub assoc: A,
}

impl<A> GReferenceTy<A> {
    pub fn require_lifetime(&self) -> bool {
        true
    }

    pub fn is_copy(&self) -> bool {
        !self.mutability
    }

    pub fn is_clone(&self) -> bool {
        !self.mutability
    }
}

impl ToString for ReferenceTy {
    fn to_string(&self) -> String {
        format!(
            "&{}{}{}",
            self.lifetime
                .as_ref()
                .map(|x| format!("'{} ", x.0))
                .unwrap_or_default(),
            self.mutability.then(|| "mut ").unwrap_or_default(),
            self.elem.to_string()
        )
    }
}

impl From<ReferenceTy> for Ty {
    fn from(ty: ReferenceTy) -> Ty {
        GTy::Reference(ty)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
pub enum TyKind {
    Unit,
    Prim,
    Tuple,
    Array,
    Struct,
    Reference,
}
