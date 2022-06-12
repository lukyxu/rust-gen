use crate::ast::ty::{
    ArrayTy, FieldDef, FieldStructTy, GReferenceTy, GTy, Lifetime, PrimTy, ReferenceTy, StructTy,
    TupleStructTy, TupleTy, Ty, TyKind,
};
use crate::context::{Context, StructContext};
use crate::generate::utils::{
    apply_limit_array_ty, apply_limit_tuple_ty, increment_counter, track_type,
};
use rand::prelude::IteratorRandom;
use rand::Rng;
use std::cmp::max;

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
        let ty_kind = ctx.choose_ty_kind();
        match ty_kind {
            TyKind::Unit => track_type(TyKind::Unit, Box::new(Ty::generate_unit_internal))(ctx),
            TyKind::Prim => PrimTy::generate_type(ctx).map(From::from),
            TyKind::Tuple => TupleTy::generate_type(ctx, &None).map(From::from),
            TyKind::Array => ArrayTy::generate_type(ctx, &None).map(From::from),
            TyKind::Struct => StructTy::generate_type(ctx, &None).map(From::from),
            TyKind::Reference => ReferenceTy::generate_type(ctx).map(From::from),
        }
    }
    pub fn generate_copy_type(ctx: &mut Context) -> Option<Ty> {
        let prev_gen_only_copy_type = ctx.gen_only_copy_type;
        ctx.gen_only_copy_type = true;
        let res_type = Ty::generate_type(ctx);
        ctx.gen_only_copy_type = prev_gen_only_copy_type;
        res_type
    }

    fn generate_unit_internal(_ctx: &mut Context) -> Option<Ty> {
        Some(GTy::Unit)
    }
}

impl PrimTy {
    pub fn generate_type(ctx: &mut Context) -> Option<PrimTy> {
        track_type(TyKind::Prim, Box::new(PrimTy::generate_type_internal))(ctx)
    }

    fn generate_type_internal(ctx: &mut Context) -> Option<PrimTy> {
        ctx.choose_prim_type()
    }
}

impl TupleTy {
    pub fn generate_type(ctx: &mut Context, ty: &Option<Ty>) -> Option<TupleTy> {
        let res = TupleTy::generate_type_internal(ctx, ty);
        increment_counter(
            &res,
            TyKind::Tuple,
            &mut ctx.statistics.successful_mapping.ty_counter,
            &mut ctx.statistics.failed_mapping.ty_counter,
        );
        res
    }

    fn generate_type_internal(ctx: &mut Context, ty: &Option<Ty>) -> Option<TupleTy> {
        if let Some(ty) = ty {
            if ty.tuple_depth() + 1 > ctx.policy.max_tuple_depth
                || ty.composite_depth() + 1 > ctx.policy.max_composite_depth
            {
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
        apply_limit_tuple_ty(TupleTy::generate_new_type_internal, ctx, ty)
    }

    fn generate_new_type_internal(ctx: &mut Context, ty: &Option<Ty>) -> Option<TupleTy> {
        let len = ctx.choose_tuple_length();
        let mut types: Vec<Ty> = vec![];
        for _ in 0..len {
            types.push(Ty::fuzz_type(ctx)?);
        }
        if let Some(ty) = &ty {
            let index = ctx.rng.gen_range(0..len);
            types[index] = ty.clone();
        }
        let tuple_type = TupleTy::new(types);
        if !ctx.tuple_type_dist.iter().any(|(t, _)| t == &tuple_type) {
            let weight = 1.0;
            ctx.tuple_type_dist.push((tuple_type.clone(), weight));
        }
        Some(tuple_type)
    }
}

impl ArrayTy {
    pub fn generate_type(ctx: &mut Context, ty: &Option<Ty>) -> Option<ArrayTy> {
        let res = ArrayTy::generate_type_internal(ctx, ty);
        increment_counter(
            &res,
            TyKind::Array,
            &mut ctx.statistics.successful_mapping.ty_counter,
            &mut ctx.statistics.failed_mapping.ty_counter,
        );
        res
    }

    fn generate_type_internal(ctx: &mut Context, ty: &Option<Ty>) -> Option<ArrayTy> {
        if let Some(ty) = ty {
            if ty.array_depth() + 1 > ctx.policy.max_array_depth
                || ty.composite_depth() + 1 > ctx.policy.max_composite_depth
            {
                return None;
            }
        }
        if ctx.policy.max_array_depth == 0 || ctx.policy.max_composite_depth == 0 {
            return None;
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
        apply_limit_array_ty(ArrayTy::generate_new_type_internal, ctx, ty)
    }

    fn generate_new_type_internal(ctx: &mut Context, ty: &Option<Ty>) -> Option<ArrayTy> {
        let len = ctx.choose_array_length();
        let base_ty = ty.clone().or_else(|| Ty::fuzz_type(ctx))?;
        let array_type = ArrayTy::new(base_ty, len);
        if !ctx.array_type_dist.iter().any(|(t, _)| t == &array_type) {
            let weight = 1.0;
            ctx.array_type_dist.push((array_type.clone(), weight));
        }
        Some(array_type)
    }
}

impl StructTy {
    pub fn generate_type(ctx: &mut Context, ty: &Option<Ty>) -> Option<StructTy> {
        let res = StructTy::generate_type_internal(ctx, ty);
        increment_counter(
            &res,
            TyKind::Struct,
            &mut ctx.statistics.successful_mapping.ty_counter,
            &mut ctx.statistics.failed_mapping.ty_counter,
        );
        res
    }

    fn generate_type_internal(ctx: &mut Context, ty: &Option<Ty>) -> Option<StructTy> {
        if let Some(ty) = ty {
            if ty.struct_depth() + 1 > ctx.policy.max_struct_depth
                || ty.composite_depth() + 1 > ctx.policy.max_composite_depth
            {
                return None;
            }
        };
        if ctx.policy.max_struct_depth == 0 || ctx.policy.max_composite_depth == 0 {
            return None;
        }
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

impl FieldStructTy {
    pub fn generate_new_type(ctx: &mut Context, ty: &Option<Ty>) -> Option<FieldStructTy> {
        let prev_max_struct_depth = ctx.policy.max_struct_depth;
        let prev_max_composite_depth = ctx.policy.max_composite_depth;
        ctx.policy.max_struct_depth = ctx.policy.max_struct_depth.saturating_sub(1);
        ctx.policy.max_composite_depth = ctx.policy.max_composite_depth.saturating_sub(1);
        ctx.struct_ctx = Some(StructContext::new(ctx.choose_copy_field_struct()));
        let res = FieldStructTy::generate_new_type_internal(ctx, ty);
        ctx.struct_ctx = None;
        ctx.policy.max_struct_depth = prev_max_struct_depth;
        ctx.policy.max_composite_depth = prev_max_composite_depth;
        res
    }

    pub fn generate_new_type_internal(ctx: &mut Context, ty: &Option<Ty>) -> Option<FieldStructTy> {
        let len = ctx.choose_struct_length();
        let mut fields: Vec<FieldDef> = vec![];
        for i in 0..len {
            fields.push(FieldDef::generate_field_def(ctx, i)?);
        }
        if let Some(ty) = &ty {
            let index = ctx.rng.gen_range(0..len);
            fields[index].ty = Box::new(ty.clone());
        }
        let struct_ty = FieldStructTy {
            name: ctx.create_struct_name(),
            is_copy: ctx.struct_ctx.as_ref().unwrap().generate_copy_struct,
            is_clone: fields.iter().all(|f| f.ty.is_clone()),
            fields,
            lifetimes: ctx
                .struct_ctx
                .as_ref()
                .map(|x| x.lifetimes.clone())
                .unwrap(),
            assoc: (),
        };
        let weight = 1.0;
        ctx.struct_type_dist
            .push((struct_ty.clone().into(), weight));
        Some(struct_ty)
    }
}

impl FieldDef {
    pub fn generate_field_def(ctx: &mut Context, i: usize) -> Option<FieldDef> {
        let base_type = Ty::fuzz_type(ctx)?;
        let name = ctx.create_field_name(i);
        Some(FieldDef {
            name,
            ty: Box::new(base_type),
        })
    }
}

impl TupleStructTy {
    pub fn generate_new_type(ctx: &mut Context, ty: &Option<Ty>) -> Option<TupleStructTy> {
        let prev_max_struct_depth = ctx.policy.max_struct_depth;
        let prev_max_composite_depth = ctx.policy.max_composite_depth;
        ctx.policy.max_struct_depth = ctx.policy.max_struct_depth.saturating_sub(1);
        ctx.policy.max_composite_depth = ctx.policy.max_composite_depth.saturating_sub(1);
        ctx.struct_ctx = Some(StructContext::new(ctx.choose_copy_tuple_struct()));
        let res = TupleStructTy::generate_new_type_internal(ctx, ty);
        ctx.struct_ctx = None;
        ctx.policy.max_struct_depth = prev_max_struct_depth;
        ctx.policy.max_composite_depth = prev_max_composite_depth;
        res
    }

    fn generate_new_type_internal(ctx: &mut Context, ty: &Option<Ty>) -> Option<TupleStructTy> {
        let fields = TupleTy::generate_type(ctx, ty)?;
        let struct_ty = TupleStructTy {
            name: ctx.create_struct_name(),
            is_copy: ctx.struct_ctx.as_ref().unwrap().generate_copy_struct,
            is_clone: fields.is_clone(),
            fields,
            lifetimes: ctx
                .struct_ctx
                .as_ref()
                .map(|x| x.lifetimes.clone())
                .unwrap(),
            assoc: (),
        };
        let weight = 1.0;
        ctx.struct_type_dist
            .push((struct_ty.clone().into(), weight));
        Some(struct_ty)
    }
}

impl ReferenceTy {
    pub fn new(elem: Ty, mutability: bool, lifetime: Option<Lifetime>) -> ReferenceTy {
        GReferenceTy {
            elem: Box::new(elem),
            mutability,
            lifetime,
            assoc: (),
        }
    }

    pub fn generate_type(ctx: &mut Context) -> Option<ReferenceTy> {
        let res = ReferenceTy::generate_type_internal(ctx);
        increment_counter(
            &res,
            TyKind::Reference,
            &mut ctx.statistics.successful_mapping.ty_counter,
            &mut ctx.statistics.failed_mapping.ty_counter,
        );
        res
    }

    pub fn generate_type_internal(ctx: &mut Context) -> Option<ReferenceTy> {
        let lifetime = Lifetime::generate_lifetime(ctx);
        let elem = Ty::fuzz_type(ctx)?;
        Some(ReferenceTy::new(elem, false, lifetime))
    }
}

impl Lifetime {
    fn generate_lifetime(ctx: &mut Context) -> Option<Lifetime> {
        ctx.struct_ctx.as_ref()?;
        let mut lifetime: Option<Lifetime> = None;
        if !ctx.choose_new_lifetime() {
            lifetime = ctx
                .struct_ctx
                .as_ref()
                .unwrap()
                .lifetimes
                .iter()
                .choose(&mut ctx.rng)
                .cloned();
        }
        if lifetime.is_none() {
            lifetime = ctx.create_lifetime_name().map(Lifetime);
            ctx.struct_ctx
                .as_mut()
                .unwrap()
                .lifetimes
                .insert(lifetime.clone().unwrap());
        }
        lifetime
    }
}
