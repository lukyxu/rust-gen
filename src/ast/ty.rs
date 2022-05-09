use crate::context::Context;
use rand::Rng;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Ty {
    Unit,
    Prim(PrimTy),
    Tuple(TupleTy), // TODO: Add more types such as Arrays, Slices, Ptrs (https://doc.rust-lang.org/nightly/nightly-rustc/rustc_middle/ty/sty/enum.TyKind.html)
    Array(ArrayTy),
}

impl Ty {
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

    pub fn is_primitive_number(&self) -> bool {
        // TODO: Add floats
        matches!(self, Ty::Prim(PrimTy::Int(_)) | Ty::Prim(PrimTy::UInt(_)))
    }

    pub fn compatible_cast(&self, target_type: &Ty) -> bool {
        // TODO: More thorough casting
        self.is_primitive_number() && target_type.is_primitive_number()
    }

    pub fn array_depth(&self) -> usize {
        match self {
            Ty::Array(array_ty) => 1 + array_ty.base_ty.array_depth(),
            _ => 0,
        }
    }

    pub fn tuple_depth(&self) -> usize {
        match self {
            Ty::Tuple(tuple_ty) => {
                1 + tuple_ty
                    .tuple
                    .iter()
                    .map(|ty| ty.tuple_depth())
                    .max()
                    .unwrap_or_default()
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
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
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
            }.to_string(),
            PrimTy::Str => "&str".to_string(),
        }
        .to_owned()
    }
}

impl From<PrimTy> for Ty {
    fn from(ty: PrimTy) -> Ty {
        Ty::Prim(ty)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
#[allow(dead_code)]
pub enum FloatTy {
    F32,
    F64,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
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

    pub fn recast(self, value: u128) -> u128 {
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

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
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

    pub fn recast(self, value: u128) -> u128 {
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

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TupleTy {
    pub tuple: Vec<Ty>
}

impl From<TupleTy> for Ty {
    fn from(ty: TupleTy) -> Ty {
        Ty::Tuple(ty)
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
                .join(",")
        )
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
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
}