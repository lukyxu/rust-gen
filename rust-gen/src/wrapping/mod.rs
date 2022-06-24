//! Extention of num traits crate to support wrapping div and wrapping rem traits.

use std::ops::{Div, Rem};
macro_rules! wrapping_impl {
    ($trait_name:ident, $method:ident, $t:ty) => {
        impl $trait_name for $t {
            #[inline]
            fn $method(&self, v: &Self) -> Self {
                <$t>::$method(*self, *v)
            }
        }
    };
    ($trait_name:ident, $method:ident, $t:ty, $rhs:ty) => {
        impl $trait_name<$rhs> for $t {
            #[inline]
            fn $method(&self, v: &$rhs) -> Self {
                <$t>::$method(*self, *v)
            }
        }
    };
}

/// Performs division that wraps around on overflow.
pub trait WrappingDiv: Sized + Div<Self, Output = Self> {
    /// Wrapping (modular) division. Computes `self / other`, wrapping around at the boundary of
    /// the type.
    fn wrapping_div(&self, v: &Self) -> Self;
}

wrapping_impl!(WrappingDiv, wrapping_div, u8);
wrapping_impl!(WrappingDiv, wrapping_div, u16);
wrapping_impl!(WrappingDiv, wrapping_div, u32);
wrapping_impl!(WrappingDiv, wrapping_div, u64);
wrapping_impl!(WrappingDiv, wrapping_div, usize);
wrapping_impl!(WrappingDiv, wrapping_div, u128);

wrapping_impl!(WrappingDiv, wrapping_div, i8);
wrapping_impl!(WrappingDiv, wrapping_div, i16);
wrapping_impl!(WrappingDiv, wrapping_div, i32);
wrapping_impl!(WrappingDiv, wrapping_div, i64);
wrapping_impl!(WrappingDiv, wrapping_div, isize);
wrapping_impl!(WrappingDiv, wrapping_div, i128);

/// Performs division that wraps around on overflow.
pub trait WrappingRem: Sized + Rem<Self, Output = Self> {
    /// Wrapping (modular) division. Computes `self / other`, wrapping around at the boundary of
    /// the type.
    fn wrapping_rem(&self, v: &Self) -> Self;
}

wrapping_impl!(WrappingRem, wrapping_rem, u8);
wrapping_impl!(WrappingRem, wrapping_rem, u16);
wrapping_impl!(WrappingRem, wrapping_rem, u32);
wrapping_impl!(WrappingRem, wrapping_rem, u64);
wrapping_impl!(WrappingRem, wrapping_rem, usize);
wrapping_impl!(WrappingRem, wrapping_rem, u128);

wrapping_impl!(WrappingRem, wrapping_rem, i8);
wrapping_impl!(WrappingRem, wrapping_rem, i16);
wrapping_impl!(WrappingRem, wrapping_rem, i32);
wrapping_impl!(WrappingRem, wrapping_rem, i64);
wrapping_impl!(WrappingRem, wrapping_rem, isize);
wrapping_impl!(WrappingRem, wrapping_rem, i128);
