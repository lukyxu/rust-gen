use lazy_static::lazy_static;
use rand::prelude::SliceRandom;
use std::cmp::max;
use std::collections::HashMap;

use crate::ast::expr::{
    ArrayExpr, AssignExpr, BinaryExpr, BlockExpr, CastExpr, Expr, ExprKind, Field, FieldExpr,
    FieldStructExpr, IdentExpr, IfExpr, IndexExpr, LitExpr, LitIntExpr, Member, PlaceExpr,
    ReferenceExpr, StructExpr, TupleExpr, TupleStructExpr, UnaryExpr,
};
use crate::ast::op::{BinaryOp, UnaryOp};
use crate::ast::stmt::Stmt;
use crate::ast::ty::{
    ArrayTy, FieldDef, FieldStructTy, GTy, PrimTy, ReferenceTy, StructTy, TupleStructTy, TupleTy,
    Ty, UIntTy,
};
use crate::context::Context;
use crate::generate::utils::{
    apply_limit_expr_depth_in_array, apply_limit_expr_depth_in_struct,
    apply_limit_expr_depth_in_tuple, increment_counter, limit_arith_depth, limit_block_depth,
    limit_expr_depth, limit_if_else_depth, revert_ctx_on_failure, track_expr,
};
use crate::symbol_table::ty::TypeSymbolTable;

impl Expr {
    /// Attempts multiple times given by `ctx.policy.max_expr_attempts` to generate a valid expression.
    pub fn fuzz_expr(ctx: &mut Context, res_type: &Ty) -> Option<Expr> {
        let mut res: Option<Expr> = None;
        let mut num_failed_attempts = 0;
        while res.is_none() && num_failed_attempts < ctx.policy.max_expr_attempts {
            res = Expr::generate_expr(ctx, res_type);
            if res.is_none() {
                num_failed_attempts += 1;
                ctx.statistics.max_failed_expr_depth =
                    max(ctx.statistics.max_failed_expr_depth, num_failed_attempts);
            }
        }
        res
    }

    /// Attempts a single attempt to generate a valid expression.
    pub fn generate_expr(ctx: &mut Context, res_type: &Ty) -> Option<Expr> {
        let expr_kind = ctx.choose_expr_kind(res_type);
        match expr_kind {
            ExprKind::Literal => LitExpr::generate_expr(ctx, res_type),
            ExprKind::If => IfExpr::generate_expr(ctx, res_type).map(From::from),
            ExprKind::Binary => BinaryExpr::generate_expr(ctx, res_type).map(From::from),
            ExprKind::Ident => IdentExpr::generate_expr(ctx, res_type).map(From::from),
            ExprKind::Block => BlockExpr::generate_expr(ctx, res_type).map(From::from),
            ExprKind::Assign => AssignExpr::generate_expr(ctx, res_type).map(From::from),
            ExprKind::Unary => UnaryExpr::generate_expr(ctx, res_type).map(From::from),
            ExprKind::Cast => CastExpr::generate_expr(ctx, res_type).map(From::from),
            ExprKind::Index => IndexExpr::generate_expr(ctx, res_type).map(From::from),
            ExprKind::Field => FieldExpr::generate_expr(ctx, res_type).map(From::from),
            _ => panic!("ExprKind {:?} not supported yet", expr_kind),
        }
    }

    pub fn fuzz_move_expr(ctx: &mut Context, res_type: &Ty) -> Option<Expr> {
        let mut res: Option<Expr> = None;
        let mut num_failed_attempts = 0;
        while res.is_none() && num_failed_attempts < ctx.policy.max_expr_attempts {
            res = Expr::generate_move_expr(ctx, res_type);
            if res.is_none() {
                num_failed_attempts += 1;
                ctx.statistics.max_failed_expr_depth =
                    max(ctx.statistics.max_failed_expr_depth, num_failed_attempts);
            }
        }
        res
    }

    pub fn generate_move_expr(ctx: &mut Context, res_type: &Ty) -> Option<Expr> {
        let expr = Expr::generate_expr(ctx, res_type)?;
        let moved = ctx.type_symbol_table.move_expr(&expr);
        if moved {
            Some(expr)
        } else {
            None
        }
    }
}

impl LitExpr {
    pub fn generate_expr(ctx: &mut Context, res_type: &Ty) -> Option<Expr> {
        track_expr(
            ExprKind::Literal,
            revert_ctx_on_failure(Box::new(LitExpr::generate_expr_internal)),
        )(ctx, res_type)
    }

    /// Attempts to generate a base literal type (not necessarily `LitExpr` but also includes arrays and structs).
    pub fn generate_expr_internal(ctx: &mut Context, res_type: &Ty) -> Option<Expr> {
        match res_type {
            GTy::Unit => Some(TupleExpr::empty_tuple()),
            GTy::Prim(PrimTy::Bool) => Some(LitExpr::Bool(ctx.choose_boolean_true()).into()),
            GTy::Prim(PrimTy::Int(t)) => {
                let value = t.rand_val(ctx);
                Some(LitIntExpr::new(value, (*t).into()).into())
            }
            GTy::Prim(PrimTy::UInt(t)) => {
                let value = t.rand_val(ctx);
                Some(LitIntExpr::new(value, (*t).into()).into())
            }
            GTy::Tuple(tuple_ty) => TupleExpr::generate_expr(ctx, tuple_ty).map(From::from),
            GTy::Array(array_ty) => ArrayExpr::generate_expr(ctx, array_ty).map(From::from),
            GTy::Struct(struct_ty) => StructExpr::generate_expr(ctx, struct_ty).map(From::from),
            GTy::Reference(reference_ty) => {
                ReferenceExpr::generate_expr(ctx, reference_ty).map(From::from)
            }
            GTy::Prim(_) => panic!(
                "Literal type for {} not supported yet",
                res_type.to_string()
            ),
        }
    }

    pub fn can_generate(ctx: &mut Context, res_type: &Ty) -> bool {
        match res_type {
            Ty::Unit | Ty::Prim(_) | Ty::Reference(_) => true,
            Ty::Tuple(_) => ctx.policy.new_tuple_prob > 0.0 || !ctx.tuple_type_dist.is_empty(),
            Ty::Array(_) => ctx.policy.new_array_prob > 0.0 || !ctx.array_type_dist.is_empty(),
            Ty::Struct(_) => !ctx.struct_type_dist.is_empty(),
        }
    }
}

impl BinaryExpr {
    pub fn generate_expr(ctx: &mut Context, res_type: &Ty) -> Option<BinaryExpr> {
        track_expr(
            ExprKind::Binary,
            limit_expr_depth(limit_arith_depth(revert_ctx_on_failure(Box::new(
                BinaryExpr::generate_expr_internal,
            )))),
        )(ctx, res_type)
    }

    fn generate_expr_internal(ctx: &mut Context, res_type: &Ty) -> Option<BinaryExpr> {
        let op = ctx.choose_binary_op(res_type)?;
        let expr = BinaryExpr::generate_expr_for_op(ctx, res_type, op);
        increment_counter(
            &expr,
            op,
            &mut ctx.statistics.successful_mapping.bin_op_counter,
            &mut ctx.statistics.failed_mapping.bin_op_counter,
        );
        expr
    }

    fn generate_expr_for_op(ctx: &mut Context, res_type: &Ty, op: BinaryOp) -> Option<BinaryExpr> {
        let lhs_arg_ty = op
            .get_compatible_lhs_arg_types(res_type, ctx)
            .choose(&mut ctx.rng)
            .cloned()
            .unwrap();
        let rhs_arg_ty = op
            .get_compatible_rhs_arg_types(&lhs_arg_ty)
            .choose(&mut ctx.rng)
            .cloned()
            .unwrap();
        let lhs = Box::new(Expr::fuzz_move_expr(ctx, &lhs_arg_ty)?);
        let rhs = if op.can_short_circuit() {
            // Statements that can short circuit might not evaluate rhs moves
            let mut symbol_table = ctx.type_symbol_table.clone();
            let expr = Box::new(Expr::fuzz_move_expr(ctx, &rhs_arg_ty)?);
            std::mem::swap(&mut symbol_table, &mut ctx.type_symbol_table);
            ctx.type_symbol_table.update_branch(&symbol_table, &None);
            expr
        } else {
            Box::new(Expr::fuzz_move_expr(ctx, &rhs_arg_ty)?)
        };
        Some(BinaryExpr { lhs, rhs, op })
    }

    pub fn can_generate(ctx: &mut Context, res_type: &Ty) -> bool {
        ctx.expr_depth + 1 <= ctx.policy.max_expr_depth && ctx.arith_depth + 1 <= ctx.policy.max_arith_depth && matches!(res_type, Ty::Prim(_))
    }
}

impl UnaryExpr {
    pub fn generate_expr(ctx: &mut Context, res_type: &Ty) -> Option<UnaryExpr> {
        track_expr(
            ExprKind::Unary,
            limit_expr_depth(limit_arith_depth(revert_ctx_on_failure(Box::new(
                UnaryExpr::generate_expr_internal,
            )))),
        )(ctx, res_type)
    }

    pub fn generate_expr_internal(ctx: &mut Context, res_type: &Ty) -> Option<UnaryExpr> {
        let op = ctx.choose_unary_op(res_type)?;
        let expr = UnaryExpr::generate_expr_for_op(ctx, res_type, op);
        increment_counter(
            &expr,
            op,
            &mut ctx.statistics.successful_mapping.un_op_counter,
            &mut ctx.statistics.failed_mapping.un_op_counter,
        );
        expr
    }

    fn generate_expr_for_op(ctx: &mut Context, res_type: &Ty, op: UnaryOp) -> Option<UnaryExpr> {
        let args_type = op
            .get_compatible_arg_types(res_type)
            .choose(&mut ctx.rng)
            .cloned()
            .unwrap();
        let expr = Box::new(Expr::fuzz_move_expr(ctx, &args_type)?);
        Some(UnaryExpr { expr, op })
    }

    pub fn can_generate(ctx: &mut Context, res_type: &Ty) -> bool {
        ctx.expr_depth + 1 <= ctx.policy.max_expr_depth
            && ctx.arith_depth + 1 <= ctx.policy.max_arith_depth
            && matches!(res_type, Ty::Prim(PrimTy::Bool | PrimTy::Int(_)))
    }
}

impl CastExpr {
    pub fn generate_expr(ctx: &mut Context, res_type: &Ty) -> Option<CastExpr> {
        track_expr(
            ExprKind::Cast,
            limit_expr_depth(limit_arith_depth(revert_ctx_on_failure(Box::new(
                CastExpr::generate_expr_internal,
            )))),
        )(ctx, res_type)
    }

    pub fn generate_expr_internal(ctx: &mut Context, res_type: &Ty) -> Option<CastExpr> {
        let source_type: Ty = PrimTy::generate_type(ctx)?.into();
        if !source_type.compatible_cast(res_type) {
            return None;
        }
        let expr = Box::new(Expr::fuzz_move_expr(ctx, &source_type)?);
        Some(CastExpr {
            expr,
            ty: res_type.clone(),
        })
    }

    pub fn can_generate(ctx: &mut Context, res_type: &Ty) -> bool {
        ctx.expr_depth + 1 <= ctx.policy.max_expr_depth
            && ctx.arith_depth + 1 <= ctx.policy.max_arith_depth
            && matches!(res_type, Ty::Prim(PrimTy::Int(_) | PrimTy::UInt(_)))
    }
}

impl IfExpr {
    pub fn generate_expr(ctx: &mut Context, res_type: &Ty) -> Option<IfExpr> {
        track_expr(
            ExprKind::Cast,
            limit_expr_depth(limit_if_else_depth(revert_ctx_on_failure(Box::new(
                IfExpr::generate_expr_internal,
            )))),
        )(ctx, res_type)
    }

    pub fn generate_expr_internal(ctx: &mut Context, res_type: &Ty) -> Option<IfExpr> {
        let cond = Expr::fuzz_expr(ctx, &PrimTy::Bool.into());
        (|| match cond {
            None => None,
            Some(cond) => {
                let (then, then_sym_t) = BlockExpr::generate_block_expr_internal(ctx, res_type)?;
                let otherwise = (!res_type.is_unit() || ctx.choose_otherwise_if_stmt())
                    .then(|| BlockExpr::generate_block_expr_internal(ctx, res_type));
                let (otherwise, otherwise_sym_t) = if let Some(otherwise) = otherwise {
                    let (otherwise, otherwise_sym_t) = otherwise?;
                    (Some(Box::new(otherwise)), Some(otherwise_sym_t))
                } else {
                    (None, None)
                };
                ctx.type_symbol_table
                    .update_branch(&then_sym_t, &otherwise_sym_t);
                Some(IfExpr {
                    condition: Box::new(cond),
                    then: Box::new(then),
                    otherwise,
                })
            }
        })()
    }

    pub fn can_generate(ctx: &mut Context, _res_type: &Ty) -> bool {
        ctx.expr_depth + 1 <= ctx.policy.max_expr_depth && ctx.if_else_depth + 1 <= ctx.policy.max_if_else_depth && ctx.block_depth <= ctx.policy.max_block_depth
    }
}

impl BlockExpr {
    pub fn generate_expr(ctx: &mut Context, res_type: &Ty) -> Option<BlockExpr> {
        track_expr(
            ExprKind::Block,
            limit_expr_depth(limit_block_depth(revert_ctx_on_failure(Box::new(
                BlockExpr::generate_expr_internal,
            )))),
        )(ctx, res_type)
    }

    pub fn generate_expr_internal(ctx: &mut Context, res_type: &Ty) -> Option<BlockExpr> {
        let (block, sym_table) = BlockExpr::generate_block_expr_internal(ctx, res_type)?;
        ctx.type_symbol_table.update(&sym_table);
        Some(block)
    }

    fn generate_block_expr_internal(
        ctx: &mut Context,
        res_type: &Ty,
    ) -> Option<(BlockExpr, TypeSymbolTable)> {
        let mut stmts: Vec<Stmt> = Vec::new();
        let mut outer_symbol_table = ctx.type_symbol_table.clone();
        let mut generable_ident_type_map = ctx.generable_ident_type_map.clone();
        let mut num_stmts = ctx.choose_num_stmts();
        if !res_type.is_unit() {
            num_stmts -= 1;
        }
        let block_expr = (|| {
            for _ in 0..num_stmts {
                // TODO: Make sure these statements are not expression statements.
                stmts.push(Stmt::fuzz_non_expr_stmt(ctx)?);
            }
            if !res_type.is_unit() {
                stmts.push(Stmt::fuzz_expr_stmt(ctx, res_type)?);
            }
            Some(BlockExpr { stmts })
        })();
        std::mem::swap(&mut outer_symbol_table, &mut ctx.type_symbol_table);
        std::mem::swap(&mut generable_ident_type_map, &mut ctx.generable_ident_type_map);
        block_expr.map(|block_expr| (block_expr, outer_symbol_table))
    }

    pub fn can_generate(ctx: &mut Context, _res_type: &Ty) -> bool {
        ctx.expr_depth + 1 <= ctx.policy.max_expr_depth && ctx.block_depth + 1 <= ctx.policy.max_block_depth && ctx.block_depth <= ctx.policy.max_block_depth
    }
}

impl IdentExpr {
    pub fn generate_expr(ctx: &mut Context, res_type: &Ty) -> Option<IdentExpr> {
        track_expr(
            ExprKind::Ident,
            revert_ctx_on_failure(Box::new(IdentExpr::generate_expr_internal)),
        )(ctx, res_type)
    }

    fn generate_expr_internal(ctx: &mut Context, res_type: &Ty) -> Option<IdentExpr> {
        let ident = ctx.choose_ident_expr_by_type(res_type)?;
        Some(ident)
    }

    pub fn generate_place_expr(ctx: &mut Context, res_type: &Ty) -> Option<IdentExpr> {
        track_expr(
            ExprKind::Ident,
            Box::new(IdentExpr::generate_place_expr_internal),
        )(ctx, res_type)
    }

    fn generate_place_expr_internal(ctx: &mut Context, res_type: &Ty) -> Option<IdentExpr> {
        let mut_ident_exprs = ctx.type_symbol_table.get_mut_ident_exprs_by_type(res_type);
        mut_ident_exprs.choose(&mut ctx.rng).cloned()
    }

    pub fn can_generate(ctx: &mut Context, res_type: &Ty) -> bool {
        ctx.generable_ident_type_map.contains(res_type)
    }
}

impl PlaceExpr {
    pub fn generate_expr(ctx: &mut Context, res_type: &Ty) -> Option<PlaceExpr> {
        let expr_kind = ctx.choose_place_expr_kind();
        match expr_kind {
            ExprKind::Ident => IdentExpr::generate_place_expr(ctx, res_type).map(From::from),
            ExprKind::Index => IndexExpr::generate_expr(ctx, res_type).map(From::from),
            ExprKind::Field => FieldExpr::generate_expr(ctx, res_type).map(From::from),
            _ => panic!("Invalid expr_kind ({:?}) for PlaceExpr ", expr_kind),
        }
    }
}

impl AssignExpr {
    pub fn generate_expr(ctx: &mut Context, res_type: &Ty) -> Option<AssignExpr> {
        track_expr(
            ExprKind::Assign,
            limit_expr_depth(revert_ctx_on_failure(Box::new(
                AssignExpr::generate_expr_internal,
            ))),
        )(ctx, res_type)
    }

    fn generate_expr_internal(ctx: &mut Context, res_type: &Ty) -> Option<AssignExpr> {
        if !res_type.is_unit() {
            return None;
        };
        let ty = Ty::generate_type(ctx)?;

        // Order is important here
        let rhs = Box::new(Expr::fuzz_move_expr(ctx, &ty)?);
        let place = PlaceExpr::generate_expr(ctx, &ty)?;
        ctx.type_symbol_table.regain_ownership(&place);
        Some(AssignExpr { place, rhs })
    }

    pub fn can_generate(ctx: &mut Context, _res_type: &Ty) -> bool {
        ctx.expr_depth + 1 <= ctx.policy.max_expr_depth
    }
}

impl ArrayExpr {
    pub fn generate_expr(ctx: &mut Context, res_type: &ArrayTy) -> Option<ArrayExpr> {
        let mut res = vec![];
        for ty in res_type.iter() {
            res.push(apply_limit_expr_depth_in_array(
                Expr::fuzz_move_expr,
                ctx,
                &ty,
            )?);
        }
        Some(ArrayExpr { array: res })
    }
}

impl TupleExpr {
    pub fn empty_tuple() -> Expr {
        Expr::Tuple(TupleExpr { tuple: vec![] })
    }

    pub fn generate_expr(ctx: &mut Context, res_type: &TupleTy) -> Option<TupleExpr> {
        let mut res = vec![];
        for ty in &res_type.tuple {
            res.push(apply_limit_expr_depth_in_tuple(
                Expr::fuzz_move_expr,
                ctx,
                ty,
            )?);
        }
        Some(TupleExpr { tuple: res })
    }
}

impl FieldExpr {
    pub fn generate_expr(ctx: &mut Context, res_type: &Ty) -> Option<FieldExpr> {
        track_expr(
            ExprKind::Field,
            limit_expr_depth(limit_arith_depth(revert_ctx_on_failure(Box::new(
                FieldExpr::generate_expr_internal,
            )))),
        )(ctx, res_type)
    }

    fn generate_expr_internal(ctx: &mut Context, res_type: &Ty) -> Option<FieldExpr> {
        if ctx.choose_field_struct() {
            FieldExpr::generate_struct_field_expr(ctx, res_type)
        } else {
            FieldExpr::generate_tuple_field_expr(ctx, res_type)
        }
    }

    pub fn generate_tuple_field_expr(ctx: &mut Context, res_type: &Ty) -> Option<FieldExpr> {
        let tuple = TupleTy::generate_type(ctx, &Some(res_type.clone()))?;

        let base = Box::new(Expr::fuzz_expr(ctx, &tuple.clone().into())?);
        let indexes: Vec<usize> = (&tuple)
            .into_iter()
            .enumerate()
            .filter_map(|(i, ty)| if ty == res_type { Some(i) } else { None })
            .collect();

        let member = Member::Unnamed(*indexes.choose(&mut ctx.rng).unwrap());
        Some(FieldExpr { base, member })
    }

    pub fn generate_struct_field_expr(ctx: &mut Context, res_type: &Ty) -> Option<FieldExpr> {
        let struct_ty = StructTy::generate_type(ctx, &Some(res_type.clone()))?;
        let base = Box::new(Expr::fuzz_expr(ctx, &struct_ty.clone().into())?);
        let member = match struct_ty {
            StructTy::Field(field_struct) => Member::Named(
                field_struct
                    .fields
                    .iter()
                    .filter(|f| &*f.ty == res_type)
                    .collect::<Vec<_>>()
                    .choose(&mut ctx.rng)
                    .unwrap()
                    .name
                    .clone(),
            ),
            StructTy::Tuple(tuple_struct) => Member::Unnamed(
                *(&tuple_struct.fields)
                    .into_iter()
                    .enumerate()
                    .filter_map(|(i, ty)| if ty == res_type { Some(i) } else { None })
                    .collect::<Vec<_>>()
                    .choose(&mut ctx.rng)
                    .unwrap(),
            ),
        };
        Some(FieldExpr { base, member })
    }

    pub fn can_generate(ctx: &mut Context, _res_type: &Ty) -> bool {
        // TODO: Can improve this
        ctx.expr_depth + 1 <= ctx.policy.max_expr_depth && ctx.arith_depth + 1 <= ctx.policy.max_arith_depth
    }
}

impl IndexExpr {
    pub fn generate_expr(ctx: &mut Context, res_type: &Ty) -> Option<IndexExpr> {
        track_expr(
            ExprKind::Index,
            limit_expr_depth(limit_arith_depth(revert_ctx_on_failure(Box::new(
                IndexExpr::generate_expr_internal,
            )))),
        )(ctx, res_type)
    }

    fn generate_expr_internal(ctx: &mut Context, res_type: &Ty) -> Option<IndexExpr> {
        if res_type.array_depth() + 1 > ctx.policy.max_array_depth {
            return None;
        }
        // [Struct1(5), Struct1(5), Struct1(5)][0] is invalid if Struct1 is not copy
        // However [Struct1(5), Struct1(5), Struct1(5)][0].0 should work
        // if !res_type.is_copy() {
        //     return None;
        // }
        // TODO: See if we can make this work
        if !res_type.is_copy() {
            return None;
        }

        let array_type: ArrayTy = ArrayTy::generate_type(ctx, &Some(res_type.clone()))?;
        let base = Box::new(Expr::fuzz_expr(ctx, &array_type.clone().into())?);
        let index = Box::new(Expr::fuzz_expr(ctx, &PrimTy::UInt(UIntTy::USize).into())?);
        let inbound_index = Box::new(Expr::Binary(BinaryExpr {
            lhs: index,
            rhs: Box::new(LitIntExpr::new(array_type.len as u128, UIntTy::USize.into()).into()),
            op: BinaryOp::Rem,
        }));

        Some(IndexExpr {
            base,
            index: inbound_index,
        })
    }

    pub fn can_generate(ctx: &mut Context, _res_type: &Ty) -> bool {
        // TODO: Can improve this
        ctx.expr_depth + 1 <= ctx.policy.max_expr_depth && ctx.arith_depth + 1 <= ctx.policy.max_arith_depth
    }
}

impl StructExpr {
    pub fn generate_expr(ctx: &mut Context, res_type: &StructTy) -> Option<StructExpr> {
        apply_limit_expr_depth_in_struct(StructExpr::generate_expr_internal, ctx, res_type)
    }

    fn generate_expr_internal(ctx: &mut Context, res_type: &StructTy) -> Option<StructExpr> {
        match res_type {
            StructTy::Field(res_type) => {
                FieldStructExpr::generate_expr(ctx, res_type).map(From::from)
            }
            StructTy::Tuple(res_type) => {
                TupleStructExpr::generate_expr(ctx, res_type).map(From::from)
            }
        }
    }
}

impl TupleStructExpr {
    pub fn generate_expr(ctx: &mut Context, res_type: &TupleStructTy) -> Option<TupleStructExpr> {
        Some(TupleStructExpr {
            struct_name: res_type.name.clone(),
            fields: TupleExpr::generate_expr(ctx, &res_type.fields)?,
        })
    }
}

impl FieldStructExpr {
    pub fn generate_expr(ctx: &mut Context, res_type: &FieldStructTy) -> Option<FieldStructExpr> {
        let mut fields: Vec<Field> = vec![];
        for field_def in &res_type.fields {
            fields.push(Field::generate_field(ctx, field_def)?);
        }
        Some(FieldStructExpr {
            struct_name: res_type.name.clone(),
            fields,
        })
    }
}

impl Field {
    pub fn generate_field(ctx: &mut Context, field_def: &FieldDef) -> Option<Field> {
        Some(Field {
            name: field_def.name.clone(),
            expr: Expr::fuzz_move_expr(ctx, &*field_def.ty)?,
        })
    }
}

impl ReferenceExpr {
    pub fn generate_expr(ctx: &mut Context, res_type: &ReferenceTy) -> Option<ReferenceExpr> {
        Some(ReferenceExpr {
            mutability: res_type.mutability,
            expr: Box::new(Expr::fuzz_expr(ctx, &res_type.elem)?),
        })
    }
}

lazy_static! {
    pub static ref GENERABLE_EXPR_FNS: HashMap<ExprKind, fn(&mut Context, &Ty) -> bool> = {
        let mut map: HashMap<ExprKind, fn(&mut Context, &Ty) -> bool> = HashMap::new();
        map.insert(ExprKind::Literal, LitExpr::can_generate);
        map.insert(ExprKind::Binary, BinaryExpr::can_generate);
        map.insert(ExprKind::Unary, UnaryExpr::can_generate);
        map.insert(ExprKind::Cast, CastExpr::can_generate);
        map.insert(ExprKind::If, IfExpr::can_generate);
        map.insert(ExprKind::Block, BlockExpr::can_generate);
        map.insert(ExprKind::Ident, IdentExpr::can_generate);
        map.insert(ExprKind::Assign, AssignExpr::can_generate);
        map.insert(ExprKind::Index, IndexExpr::can_generate);
        map.insert(ExprKind::Field, FieldExpr::can_generate);
        map
    };
}
