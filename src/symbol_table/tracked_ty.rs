use crate::ast::ty::{GTupleTy, GTy, Ty};

pub enum BorrowState {

}

struct TrackedTy {
    ty: GTy<TrackedTy>,
    borrow_state: BorrowState,
}
// pub enum TrackedTy {
//     Unit,
//     Prim(TrackedPrimTy),
//     Tuple(TupleTy),
//     Array(TrackedPrimTy),
//     Struct(TrackedPrimTy),
//     Reference(TrackedPrimTy),
// }