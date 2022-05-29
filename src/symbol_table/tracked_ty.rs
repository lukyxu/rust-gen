use crate::ast::ty::GTy;

pub enum OwnershipState {
    Owned,
    PartiallyOwned,
    Unowned,
}

#[allow(dead_code)]
struct TrackedTy {
    ty: GTy<TrackedTy>,
    ownership_state: OwnershipState,
}
