use crate::Context;

use rand::Rng;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Ty {
    Bool,
    #[allow(dead_code)]
    Char,
    Int(IntTy),
    UInt(UIntTy),
    #[allow(dead_code)]
    Float(FloatTy),
    #[allow(dead_code)]
    Str,
    Tuple(Vec<Ty>), // TODO: Add more types such as Arrays, Slices, Ptrs (https://doc.rust-lang.org/nightly/nightly-rustc/rustc_middle/ty/sty/enum.TyKind.html)
}

impl Ty {
    pub fn is_unit(&self) -> bool {
        match self {
            Ty::Tuple(types) => types.is_empty(),
            _ => false,
        }
    }

    pub fn unit_type() -> Ty {
        Ty::Tuple(Vec::new())
    }

    pub fn is_primitive_number(&self) -> bool {
        // TODO: Add floats
        match self {
            Ty::Int(_) | Ty::UInt(_) => true,
            _ => false,
        }
    }

    pub fn compatible_cast(&self, target_type: &Ty) -> bool {
        // TODO: More thorough casting
        self.is_primitive_number() && target_type.is_primitive_number()
    }
}

impl ToString for Ty {
    fn to_string(&self) -> String {
        let tmp;
        match self {
            Ty::Bool => "bool",
            Ty::Char => "char",
            Ty::Int(int) => match int {
                IntTy::ISize => "isize",
                IntTy::I8 => "i8",
                IntTy::I16 => "i16",
                IntTy::I32 => "i32",
                IntTy::I64 => "i64",
                IntTy::I128 => "i128",
            },
            Ty::UInt(uint) => match uint {
                UIntTy::USize => "usize",
                UIntTy::U8 => "u8",
                UIntTy::U16 => "u16",
                UIntTy::U32 => "u32",
                UIntTy::U64 => "u64",
                UIntTy::U128 => "u128",
            },
            Ty::Float(float) => match float {
                FloatTy::F32 => "f32",
                FloatTy::F64 => "f64",
            },
            Ty::Str => "&str",
            Ty::Tuple(tuple) => {
                tmp = format!(
                    "({})",
                    tuple
                        .iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<String>>()
                        .join(",")
                );
                &tmp
            }
        }
        .to_owned()
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

impl IntTy {
    pub fn rand_val(&self, ctx: &mut Context) -> u128 {
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

    pub fn recast(&self, value: u128) -> u128 {
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
    pub fn rand_val(&self, ctx: &mut Context) -> u128 {
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

    pub fn recast(&self, value: u128) -> u128 {
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
