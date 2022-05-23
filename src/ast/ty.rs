use crate::ast::expr::LitIntTy;
use crate::ast::utils::{increment_counter, track_type};
use crate::context::Context;
use rand::Rng;
use serde::{Deserialize, Serialize};
use std::cmp::max;

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub enum Ty {
    Unit,
    Prim(PrimTy),
    Tuple(TupleTy), // TODO: Add more types such as Arrays, Slices, Ptrs (https://doc.rust-lang.org/nightly/nightly-rustc/rustc_middle/ty/sty/enum.TyKind.html)
    Array(ArrayTy),
    Struct(StructTy),
}

impl Ty {
    /// Attempts multiple times given by `ctx.policy.max_ty_attempts` to generate a valid type.
    pub fn fuzz_type(ctx: &mut Context) -> Option<Ty> {
        let mut res: Option<Ty> = None;
        let mut num_failed_attempts = 0;
        while res.is_none() && num_failed_attempts < ctx.policy.max_ty_attempts {
            res = Ty::generate_type(ctx);
            if res.is_none() {
                num_failed_attempts += 1;
                ctx.statistics.max_failed_ty_depth =
                    max(ctx.statistics.max_failed_ty_depth, num_failed_attempts);
            }
        }
        res
    }
    /// Attempts a single attempt to generate a valid type.
    pub fn generate_type(ctx: &mut Context) -> Option<Ty> {
        let ty_kind = ctx.choose_base_expr_kind();
        match ty_kind {
            TyKind::Unit => track_type(TyKind::Unit, Box::new(Ty::generate_unit_internal))(ctx),
            TyKind::Prim => PrimTy::generate_type(ctx).map(From::from),
            TyKind::Tuple => TupleTy::generate_type(ctx, &None).map(From::from),
            TyKind::Array => ArrayTy::generate_type(ctx, &None).map(From::from),
            TyKind::Struct => StructTy::generate_type(ctx, &None).map(From::from),
        }
    }

    fn generate_unit_internal(_ctx: &mut Context) -> Option<Ty> {
        Some(Ty::Unit)
    }

    /// Returns whether a given type is the unit type.
    /// Both `Ty::Unit` and `Ty::Tuple(vec![])` correspond to the unit type.
    pub fn is_unit(&self) -> bool {
        match self {
            Ty::Unit => true,
            Ty::Tuple(tuple_ty) => tuple_ty.tuple.is_empty(),
            _ => false,
        }
    }

    pub fn unit_type() -> Ty {
        Ty::Unit
    }

    /// Returns whether a given type is a primitive integer.
    pub fn is_primitive_number(&self) -> bool {
        // TODO: Add floats
        matches!(self, Ty::Prim(PrimTy::Int(_) | PrimTy::UInt(_)))
    }

    /// Checks to see if a given cast is compatible.
    pub fn compatible_cast(&self, target_type: &Ty) -> bool {
        // TODO: More thorough casting
        self.is_primitive_number() && target_type.is_primitive_number()
    }

    /// Returns the array depth of a type.
    pub fn array_depth(&self) -> usize {
        match self {
            Ty::Array(array_ty) => 1 + array_ty.base_ty.array_depth(),
            _ => 0,
        }
    }

    /// Returns the tuple depth of a type.
    pub fn tuple_depth(&self) -> usize {
        match self {
            Ty::Tuple(tuple_ty) => {
                1 + tuple_ty
                    .tuple
                    .iter()
                    .map(Ty::tuple_depth)
                    .max()
                    .unwrap_or_default()
            }
            _ => 0,
        }
    }

    /// Returns the struct depth of a type.
    pub fn struct_depth(&self) -> usize {
        match self {
            Ty::Struct(struct_ty) => {
                1 + match struct_ty {
                    StructTy::Field(field_struct) => field_struct
                        .fields
                        .iter()
                        .map(|f| f.ty.struct_depth())
                        .max()
                        .unwrap_or_default(),
                    StructTy::Tuple(tuple_struct) => tuple_struct
                        .fields
                        .tuple
                        .iter()
                        .map(Ty::struct_depth)
                        .max()
                        .unwrap_or_default(),
                }
            }
            _ => 0,
        }
    }
}

impl ToString for Ty {
    fn to_string(&self) -> String {
        match self {
            Ty::Unit => "()".to_string(),
            Ty::Prim(prim) => prim.to_string(),
            Ty::Tuple(tuple) => tuple.to_string(),
            Ty::Array(array) => array.to_string(),
            Ty::Struct(struct_ty) => struct_ty.to_string(),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
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
        Ty::Prim(ty)
    }
}

impl PrimTy {
    pub fn generate_type(ctx: &mut Context) -> Option<PrimTy> {
        track_type(TyKind::Prim, Box::new(PrimTy::generate_type_internal))(ctx)
    }

    fn generate_type_internal(ctx: &mut Context) -> Option<PrimTy> {
        ctx.choose_prim_type()
    }

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
        return ints;
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
#[allow(dead_code)]
pub enum FloatTy {
    F32,
    F64,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Serialize, Deserialize)]
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
        Ty::Prim(ty.into())
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

#[derive(Debug, Copy, Clone, Eq, PartialEq, Serialize, Deserialize)]
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
        Ty::Prim(ty.into())
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

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct TupleTy {
    pub tuple: Vec<Ty>,
}

impl From<TupleTy> for Ty {
    fn from(ty: TupleTy) -> Ty {
        Ty::Tuple(ty)
    }
}

impl TupleTy {
    /// Returns the depth of a tuple.
    pub fn tuple_depth(&self) -> usize {
        1 + self
            .tuple
            .iter()
            .map(Ty::tuple_depth)
            .max()
            .unwrap_or_default()
    }
}

impl<'a> IntoIterator for &'a TupleTy {
    type Item = &'a Ty;
    type IntoIter = std::slice::Iter<'a, Ty>;

    fn into_iter(self) -> Self::IntoIter {
        (&self.tuple).iter()
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

impl TupleTy {
    pub fn generate_type(ctx: &mut Context, ty: &Option<Ty>) -> Option<TupleTy> {
        let res = TupleTy::generate_type_internal(ctx, ty);
        increment_counter(
            &res,
            TyKind::Tuple,
            &mut ctx.statistics.successful_ty_counter,
            &mut ctx.statistics.failed_ty_counter,
        );
        res
    }

    fn generate_type_internal(ctx: &mut Context, ty: &Option<Ty>) -> Option<TupleTy> {
        if let Some(ty) = ty {
            if ty.tuple_depth() + 1 > ctx.policy.max_tuple_depth {
                return None;
            }
        }
        let mut res: Option<TupleTy> = None;
        if !ctx.choose_new_tuple_type() {
            res = ctx.choose_tuple_type(ty);
        }
        if res.is_none() && ctx.gen_new_tuple_types {
            res = TupleTy::generate_new_type(ctx, ty);
        }
        res
    }

    pub fn generate_new_type(ctx: &mut Context, ty: &Option<Ty>) -> Option<TupleTy> {
        let prev_max_tuple_depth = ctx.policy.max_tuple_depth;
        let prev_gen_new_tuple_types = ctx.gen_new_tuple_types;
        ctx.policy.max_tuple_depth = ctx.policy.max_tuple_depth.saturating_sub(1);
        ctx.gen_new_tuple_types = false;
        let res: Option<TupleTy> = (|| {
            let len = ctx.choose_tuple_length();
            let mut types: Vec<Ty> = vec![];
            for _ in 0..len {
                types.push(Ty::fuzz_type(ctx)?)
            }
            if let Some(ty) = &ty {
                let index = ctx.rng.gen_range(0..len);
                types[index] = ty.clone();
            }
            let tuple_type = TupleTy { tuple: types };
            if !ctx.tuple_type_dist.iter().any(|(t, _)| t == &tuple_type) {
                let weight = 1.0;
                ctx.tuple_type_dist.push((tuple_type.clone(), weight));
            }
            Some(tuple_type)
        })();
        ctx.policy.max_tuple_depth = prev_max_tuple_depth;
        ctx.gen_new_tuple_types = prev_gen_new_tuple_types;
        res
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct ArrayTy {
    pub base_ty: Box<Ty>,
    pub len: usize,
}

impl From<ArrayTy> for Ty {
    fn from(ty: ArrayTy) -> Ty {
        Ty::Array(ty)
    }
}

impl ToString for ArrayTy {
    fn to_string(&self) -> String {
        format!("[{};{}]", self.base_ty.to_string(), self.len)
    }
}

impl ArrayTy {
    pub fn iter(&self) -> impl Iterator<Item = Ty> {
        std::iter::repeat(*self.base_ty.clone()).take(self.len)
    }

    pub fn generate_type(ctx: &mut Context, ty: &Option<Ty>) -> Option<ArrayTy> {
        let res = ArrayTy::generate_type_internal(ctx, ty);
        increment_counter(
            &res,
            TyKind::Array,
            &mut ctx.statistics.successful_ty_counter,
            &mut ctx.statistics.failed_ty_counter,
        );
        res
    }

    fn generate_type_internal(ctx: &mut Context, ty: &Option<Ty>) -> Option<ArrayTy> {
        if let Some(ty) = ty {
            if ty.array_depth() + 1 > ctx.policy.max_array_depth {
                return None;
            }
        }
        let mut res: Option<ArrayTy> = None;
        if !ctx.choose_new_array_type() {
            res = ctx.choose_array_type(ty);
        }
        if res.is_none() && ctx.gen_new_array_types {
            res = ArrayTy::generate_new_type(ctx, ty);
        }
        res
    }

    pub fn generate_new_type(ctx: &mut Context, ty: &Option<Ty>) -> Option<ArrayTy> {
        let prev_max_array_depth = ctx.policy.max_array_depth;
        let prev_gen_new_array_types = ctx.gen_new_array_types;
        ctx.policy.max_array_depth = ctx.policy.max_array_depth.saturating_sub(1);
        ctx.gen_new_array_types = false;
        let len = ctx.choose_array_length();
        let base_ty = ty.clone().or_else(|| Ty::fuzz_type(ctx));
        let res = if let Some(base_ty) = base_ty {
            let array_type = ArrayTy {
                base_ty: Box::new(base_ty),
                len,
            };
            if !ctx.array_type_dist.iter().any(|(t, _)| t == &array_type) {
                let weight = 1.0;
                ctx.array_type_dist.push((array_type.clone(), weight));
            }
            Some(array_type)
        } else {
            None
        };
        ctx.policy.max_array_depth = prev_max_array_depth;
        ctx.gen_new_array_types = prev_gen_new_array_types;
        res
    }

    pub fn array_depth(&self) -> usize {
        return 1 + self.base_ty.array_depth();
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub enum StructTy {
    Field(FieldStructTy),
    Tuple(TupleStructTy),
}

impl From<StructTy> for Ty {
    fn from(ty: StructTy) -> Ty {
        Ty::Struct(ty)
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

impl StructTy {
    pub fn is_field_struct(&self) -> bool {
        matches!(self, StructTy::Field(_))
    }

    pub fn is_tuple_struct(&self) -> bool {
        matches!(self, StructTy::Tuple(_))
    }

    pub fn generate_type(ctx: &mut Context, ty: &Option<Ty>) -> Option<StructTy> {
        let res = StructTy::generate_type_internal(ctx, ty);
        increment_counter(
            &res,
            TyKind::Struct,
            &mut ctx.statistics.successful_ty_counter,
            &mut ctx.statistics.failed_ty_counter,
        );
        res
    }

    fn generate_type_internal(ctx: &mut Context, ty: &Option<Ty>) -> Option<StructTy> {
        ctx.choose_struct_type(ty)
    }

    pub fn generate_new_type(ctx: &mut Context) -> Option<StructTy> {
        if ctx.choose_field_struct() {
            FieldStructTy::generate_new_type(ctx, &None).map(From::from)
        } else {
            TupleStructTy::generate_new_type(ctx, &None).map(From::from)
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct FieldStructTy {
    pub name: String,
    pub fields: Vec<FieldDef>,
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

impl FieldStructTy {
    pub fn generate_new_type(ctx: &mut Context, ty: &Option<Ty>) -> Option<FieldStructTy> {
        'outer: for _ in 0..10 {
            let len = ctx.choose_struct_length();
            let mut fields: Vec<FieldDef> = vec![];
            for i in 0..len {
                if let Some(field_def) = FieldDef::generate_field_def(ctx, i) {
                    fields.push(field_def);
                } else {
                    continue 'outer;
                }
            }
            if let Some(ty) = &ty {
                let index = ctx.rng.gen_range(0..len);
                fields[index].ty = Box::new(ty.clone());
            }
            let struct_ty = FieldStructTy {
                name: ctx.create_struct_name(),
                fields,
            };
            let weight = 1.0;
            ctx.struct_type_dist
                .push((struct_ty.clone().into(), weight));
            return Some(struct_ty);
        }
        None
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct FieldDef {
    pub name: String,
    pub ty: Box<Ty>,
}

impl ToString for FieldDef {
    fn to_string(&self) -> String {
        format!("{}: {}", self.name, self.ty.to_string())
    }
}

impl FieldDef {
    pub fn generate_field_def(ctx: &mut Context, i: usize) -> Option<FieldDef> {
        let base_type = Ty::generate_type(ctx)?;
        if base_type.struct_depth() + 1 > ctx.policy.max_struct_depth {
            return None;
        }
        let name = ctx.create_field_name(i);
        Some(FieldDef {
            name,
            ty: Box::new(base_type),
        })
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct TupleStructTy {
    pub name: String,
    pub fields: TupleTy,
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

impl TupleStructTy {
    pub fn generate_new_type(ctx: &mut Context, ty: &Option<Ty>) -> Option<TupleStructTy> {
        let fields = TupleTy::generate_type(ctx, ty)?;
        let struct_ty = TupleStructTy {
            name: ctx.create_struct_name(),
            fields,
        };
        let weight = 1.0;
        ctx.struct_type_dist
            .push((struct_ty.clone().into(), weight));
        Some(struct_ty)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum TyKind {
    Unit,
    Prim,
    Tuple,
    Array,
    Struct,
}
