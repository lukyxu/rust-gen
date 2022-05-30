use crate::ast::stmt::Stmt;
use crate::ast::ty::{
    ArrayTy, FieldDef, FieldStructTy, FloatTy, GTy, IntTy, PrimTy, ReferenceTy, StructTy,
    TupleStructTy, TupleTy, Ty, UIntTy,
};
use rand::prelude::SliceRandom;

use crate::ast::op::{BinaryOp, UnaryOp};
use crate::ast::utils::{
    apply_limit_expr_depth_in_array, apply_limit_expr_depth_in_struct,
    apply_limit_expr_depth_in_tuple, limit_arith_depth, limit_block_depth, limit_expr_depth,
    limit_if_else_depth, track_expr,
};
use crate::context::Context;
use lazy_static::lazy_static;
use serde::{Deserialize, Serialize};
use std::cmp::max;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
#[non_exhaustive]
/// Rust expression
pub enum Expr {
    /// Literal such as `1_u32`, `"foo"`
    Literal(LitExpr),
    /// Binary operation such as `a + b`, `a * b`
    Binary(BinaryExpr),
    /// Unary operation such as `!x`
    Unary(UnaryExpr),
    /// Cast expression such as `x as u64`
    Cast(CastExpr),
    /// If expression with optional `else` block
    /// `if expr { block } else { expr }`
    If(IfExpr),
    /// Block expression
    Block(BlockExpr),
    /// A variable access such as `x` (Similar to Rust Path in Rust compiler).
    Ident(IdentExpr),
    /// Tuple literal expression such as `(1_u32, "hello")`.
    Tuple(TupleExpr),
    /// Assignment expression such as `(1_u32, "hello")`.
    Assign(AssignExpr),
    /// Array literal expression such as `[1_u32, 2_u32, 3_u32]`.
    Array(ArrayExpr),
    /// Index expression with squared brackets such as `array[5]`.
    Index(IndexExpr),
    /// Field expression representing access to structs and tuples such as `struct.field`.
    Field(FieldExpr),
    /// Struct literal expression such as `S { field1: value1, field2: value2 }` and `S(5_u32, "hello")`.
    Struct(StructExpr),
    /// Reference expression such as `&a` or `&mut a`.
    Reference(ReferenceExpr), // TODO: Path, Box
}

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
}

#[cfg(test)]
impl Expr {
    pub fn bool(b: bool) -> Expr {
        Expr::Literal(LitExpr::Bool(b))
    }

    pub fn i8(i: i8) -> Expr {
        LitIntExpr::new(i as u128, IntTy::I8.into()).into()
    }

    pub fn u8(u: u8) -> Expr {
        LitIntExpr::new(u as u128, UIntTy::U8.into()).into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LitExpr {
    // TODO: Support different styles of Strings such as raw strings `r##"foo"##`
    #[allow(dead_code)]
    /// String literal.
    Str(String),
    #[allow(dead_code)]
    /// Byte literal.
    Byte(u8),
    #[allow(dead_code)]
    /// Char literal.
    Char(char),
    /// Integer literal.
    Int(LitIntExpr),
    #[allow(dead_code)]
    /// Float literal.
    Float(String, LitFloatTy),
    /// Boolean literal.
    Bool(bool),
}

impl From<LitExpr> for Expr {
    fn from(expr: LitExpr) -> Expr {
        Expr::Literal(expr)
    }
}

impl LitExpr {
    pub fn generate_expr(ctx: &mut Context, res_type: &Ty) -> Option<Expr> {
        track_expr(ExprKind::Literal, Box::new(LitExpr::generate_expr_internal))(ctx, res_type)
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
            _ => panic!(
                "Literal type for {} not supported yet",
                res_type.to_string()
            ),
        }
    }

    pub fn can_generate(ctx: &mut Context, res_type: &Ty) -> bool {
        match res_type {
            Ty::Unit => true,
            Ty::Prim(_) => true,
            Ty::Tuple(_) => ctx.policy.new_tuple_prob > 0.0 || !ctx.tuple_type_dist.is_empty(),
            Ty::Array(_) => ctx.policy.new_array_prob > 0.0 || !ctx.array_type_dist.is_empty(),
            Ty::Struct(_) => !ctx.struct_type_dist.is_empty(),
            Ty::Reference(_) => true,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum LitIntTy {
    Signed(IntTy),
    Unsigned(UIntTy),
}

#[derive(Debug, Copy, Clone, PartialEq)]
/// Literal integer expression of a `LitIntTy` type.
/// The actual value of the integer is represented as the u128 value casted with respect to the type.
pub struct LitIntExpr {
    pub value: u128,
    pub ty: LitIntTy,
}

impl From<LitIntExpr> for Expr {
    fn from(expr: LitIntExpr) -> Expr {
        Expr::Literal(expr.into())
    }
}

impl From<LitIntExpr> for LitExpr {
    fn from(expr: LitIntExpr) -> LitExpr {
        LitExpr::Int(expr)
    }
}

impl LitIntExpr {
    pub fn new(value: u128, ty: LitIntTy) -> LitIntExpr {
        LitIntExpr { value, ty }
    }

    pub fn cast(self, ty: LitIntTy) -> LitIntExpr {
        match ty {
            LitIntTy::Signed(ty) => LitIntExpr::new(ty.cast_value(self.value), ty.into()),
            LitIntTy::Unsigned(ty) => LitIntExpr::new(ty.cast_value(self.value), ty.into()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
#[allow(dead_code)]
pub enum LitFloatTy {
    /// Float literal with suffix such as `1f32`, `1E10f32`.
    Suffixed(FloatTy),
    /// Float literal without suffix such as `1.0`, `1.0E10` (To remove once floats are implemented).
    Unsuffixed,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpr {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub op: BinaryOp,
}

impl BinaryExpr {
    pub fn generate_expr(ctx: &mut Context, res_type: &Ty) -> Option<BinaryExpr> {
        track_expr(
            ExprKind::Binary,
            limit_expr_depth(limit_arith_depth(Box::new(
                BinaryExpr::generate_expr_internal,
            ))),
        )(ctx, res_type)
    }

    fn generate_expr_internal(ctx: &mut Context, res_type: &Ty) -> Option<BinaryExpr> {
        let op = ctx.choose_binary_op(res_type)?;
        let args_type = op
            .get_compatible_arg_types(res_type, ctx)
            .choose(&mut ctx.rng)
            .cloned()
            .unwrap();
        let lhs = Box::new(Expr::fuzz_expr(ctx, &args_type)?);
        let rhs = Box::new(Expr::fuzz_expr(ctx, &args_type)?);
        *ctx.statistics.bin_op_counter.entry(op).or_insert(0) += 1;
        Some(BinaryExpr { lhs, rhs, op })
    }

    pub fn can_generate(ctx: &mut Context, res_type: &Ty) -> bool {
        ctx.expr_depth <= ctx.policy.max_expr_depth
            && match res_type {
                Ty::Prim(_) => true,
                _ => false,
            }
    }
}

impl From<BinaryExpr> for Expr {
    fn from(expr: BinaryExpr) -> Expr {
        Expr::Binary(expr)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpr {
    pub expr: Box<Expr>,
    pub op: UnaryOp,
}

impl From<UnaryExpr> for Expr {
    fn from(expr: UnaryExpr) -> Expr {
        Expr::Unary(expr)
    }
}

impl UnaryExpr {
    pub fn generate_expr(ctx: &mut Context, res_type: &Ty) -> Option<UnaryExpr> {
        track_expr(
            ExprKind::Unary,
            limit_expr_depth(limit_arith_depth(Box::new(
                UnaryExpr::generate_expr_internal,
            ))),
        )(ctx, res_type)
    }

    pub fn generate_expr_internal(ctx: &mut Context, res_type: &Ty) -> Option<UnaryExpr> {
        let op = ctx.choose_unary_op(res_type)?;
        let args_type = op
            .get_compatible_arg_types(res_type)
            .choose(&mut ctx.rng)
            .cloned()
            .unwrap();
        let expr = Box::new(Expr::fuzz_expr(ctx, &args_type)?);
        *ctx.statistics.un_op_counter.entry(op).or_insert(0) += 1;
        Some(UnaryExpr { expr, op })
    }

    pub fn can_generate(ctx: &mut Context, res_type: &Ty) -> bool {
        ctx.expr_depth <= ctx.policy.max_expr_depth
            && match res_type {
                Ty::Prim(PrimTy::Bool | PrimTy::Int(_)) => true,
                _ => false,
            }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CastExpr {
    pub expr: Box<Expr>,
    pub ty: Ty,
}

impl From<CastExpr> for Expr {
    fn from(expr: CastExpr) -> Expr {
        Expr::Cast(expr)
    }
}

impl CastExpr {
    pub fn generate_expr(ctx: &mut Context, res_type: &Ty) -> Option<CastExpr> {
        track_expr(
            ExprKind::Cast,
            limit_expr_depth(limit_arith_depth(Box::new(
                CastExpr::generate_expr_internal,
            ))),
        )(ctx, res_type)
    }

    pub fn generate_expr_internal(ctx: &mut Context, res_type: &Ty) -> Option<CastExpr> {
        let source_type: Ty = PrimTy::generate_type(ctx)?.into();
        if !source_type.compatible_cast(res_type) {
            return None;
        }
        let expr = Box::new(Expr::fuzz_expr(ctx, &source_type)?);
        Some(CastExpr {
            expr,
            ty: res_type.clone(),
        })
    }

    pub fn can_generate(ctx: &mut Context, res_type: &Ty) -> bool {
        ctx.expr_depth <= ctx.policy.max_expr_depth
            && match res_type {
                Ty::Prim(PrimTy::Int(_) | PrimTy::UInt(_)) => true,
                _ => false,
            }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfExpr {
    pub condition: Box<Expr>,
    pub then: Box<BlockExpr>,
    pub otherwise: Option<Box<BlockExpr>>,
}

impl From<IfExpr> for Expr {
    fn from(expr: IfExpr) -> Expr {
        Expr::If(expr)
    }
}

impl IfExpr {
    pub fn generate_expr(ctx: &mut Context, res_type: &Ty) -> Option<IfExpr> {
        track_expr(
            ExprKind::Cast,
            limit_expr_depth(limit_if_else_depth(Box::new(
                IfExpr::generate_expr_internal,
            ))),
        )(ctx, res_type)
    }

    pub fn generate_expr_internal(ctx: &mut Context, res_type: &Ty) -> Option<IfExpr> {
        // let outer_symbol_table = ctx.type_symbol_table.clone();
        let cond = Expr::fuzz_expr(ctx, &PrimTy::Bool.into());
        let if_expr = (|| match cond {
            None => None,
            Some(cond) => {
                let then = BlockExpr::generate_expr(ctx, res_type);
                let then = if let Some(then) = then {
                    Box::new(then)
                } else {
                    return None;
                };
                let otherwise = if !res_type.is_unit() || ctx.choose_otherwise_if_stmt() {
                    if let Some(otherwise) = BlockExpr::generate_expr(ctx, res_type) {
                        Some(Box::new(otherwise))
                    } else {
                        return None;
                    }
                } else {
                    None
                };
                Some(IfExpr {
                    condition: Box::new(cond),
                    then,
                    otherwise,
                })
            }
        })();
        // ctx.type_symbol_table = outer_symbol_table;
        if_expr
    }

    pub fn can_generate(ctx: &mut Context, _res_type: &Ty) -> bool {
        ctx.expr_depth <= ctx.policy.max_expr_depth && ctx.block_depth <= ctx.policy.max_block_depth
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlockExpr {
    pub stmts: Vec<Stmt>,
}

impl From<BlockExpr> for Expr {
    fn from(expr: BlockExpr) -> Expr {
        Expr::Block(expr)
    }
}

impl BlockExpr {
    pub fn generate_expr(ctx: &mut Context, res_type: &Ty) -> Option<BlockExpr> {
        track_expr(
            ExprKind::Block,
            limit_expr_depth(limit_block_depth(Box::new(
                BlockExpr::generate_expr_internal,
            ))),
        )(ctx, res_type)
    }
    fn generate_expr_internal(ctx: &mut Context, res_type: &Ty) -> Option<BlockExpr> {
        let mut stmts: Vec<Stmt> = Vec::new();
        let outer_symbol_table = ctx.type_symbol_table.clone();
        let block_expr = (|| {
            let mut num_stmts = ctx.choose_num_stmts();
            if !res_type.is_unit() {
                num_stmts -= 1;
            }
            for _ in 0..num_stmts {
                // TODO: Make sure these statements are not expression statements.
                stmts.push(Stmt::fuzz_non_expr_stmt(ctx)?);
            }
            if !res_type.is_unit() {
                stmts.push(Stmt::fuzz_expr_stmt(ctx, res_type)?);
            }
            Some(BlockExpr { stmts })
        })();
        ctx.type_symbol_table = outer_symbol_table;
        // ctx.type_symbol_table = outer_symbol_table.merge(&ctx.type_symbol_table);
        block_expr
    }

    pub fn can_generate(ctx: &mut Context, _res_type: &Ty) -> bool {
        ctx.expr_depth <= ctx.policy.max_expr_depth && ctx.block_depth <= ctx.policy.max_block_depth
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IdentExpr {
    pub name: String,
}

impl From<IdentExpr> for Expr {
    fn from(expr: IdentExpr) -> Expr {
        Expr::Ident(expr)
    }
}

impl From<IdentExpr> for PlaceExpr {
    fn from(expr: IdentExpr) -> PlaceExpr {
        PlaceExpr::Ident(expr)
    }
}

impl IdentExpr {
    pub fn generate_expr(ctx: &mut Context, res_type: &Ty) -> Option<IdentExpr> {
        track_expr(ExprKind::Ident, Box::new(IdentExpr::generate_expr_internal))(ctx, res_type)
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
        !ctx.type_symbol_table
            .get_ident_exprs_by_type(res_type)
            .is_empty()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TupleExpr {
    pub tuple: Vec<Expr>,
}

impl From<TupleExpr> for Expr {
    fn from(expr: TupleExpr) -> Expr {
        Expr::Tuple(expr)
    }
}

impl TupleExpr {
    pub fn empty_tuple() -> Expr {
        Expr::Tuple(TupleExpr { tuple: vec![] })
    }

    pub fn generate_expr(ctx: &mut Context, res_type: &TupleTy) -> Option<TupleExpr> {
        let mut res = vec![];
        for ty in &res_type.tuple {
            res.push(apply_limit_expr_depth_in_tuple(Expr::fuzz_expr, ctx, ty)?);
        }
        Some(TupleExpr { tuple: res })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum PlaceExpr {
    Field(FieldExpr),
    Index(IndexExpr),
    Ident(IdentExpr),
}

impl From<PlaceExpr> for Expr {
    fn from(place: PlaceExpr) -> Expr {
        match place {
            PlaceExpr::Field(expr) => expr.into(),
            PlaceExpr::Index(expr) => expr.into(),
            PlaceExpr::Ident(expr) => expr.into(),
        }
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

#[derive(Debug, Clone, PartialEq)]
pub struct AssignExpr {
    pub place: PlaceExpr,
    pub rhs: Box<Expr>,
}

impl From<AssignExpr> for Expr {
    fn from(expr: AssignExpr) -> Expr {
        Expr::Assign(expr)
    }
}

impl AssignExpr {
    pub fn generate_expr(ctx: &mut Context, res_type: &Ty) -> Option<AssignExpr> {
        track_expr(
            ExprKind::Assign,
            limit_expr_depth(Box::new(AssignExpr::generate_expr_internal)),
        )(ctx, res_type)
    }

    fn generate_expr_internal(ctx: &mut Context, res_type: &Ty) -> Option<AssignExpr> {
        if !res_type.is_unit() {
            return None;
        };
        let ty = Ty::generate_type(ctx)?;

        let place: PlaceExpr = PlaceExpr::generate_expr(ctx, &ty)?;

        Some(AssignExpr {
            place,
            rhs: Box::new(Expr::fuzz_expr(ctx, &ty)?),
        })
    }

    pub fn can_generate(ctx: &mut Context, _res_type: &Ty) -> bool {
        ctx.expr_depth <= ctx.policy.max_expr_depth
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayExpr {
    pub array: Vec<Expr>,
}

impl From<ArrayExpr> for Expr {
    fn from(expr: ArrayExpr) -> Expr {
        Expr::Array(expr)
    }
}

impl ArrayExpr {
    pub fn generate_expr(ctx: &mut Context, res_type: &ArrayTy) -> Option<ArrayExpr> {
        let mut res = vec![];
        for ty in res_type.iter() {
            res.push(apply_limit_expr_depth_in_array(Expr::fuzz_expr, ctx, &ty)?);
        }
        Some(ArrayExpr { array: res })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Member {
    Named(String),
    Unnamed(usize),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldExpr {
    pub base: Box<Expr>,
    pub member: Member,
}

impl From<FieldExpr> for Expr {
    fn from(expr: FieldExpr) -> Expr {
        Expr::Field(expr)
    }
}

impl From<FieldExpr> for PlaceExpr {
    fn from(expr: FieldExpr) -> PlaceExpr {
        PlaceExpr::Field(expr)
    }
}

impl FieldExpr {
    pub fn generate_expr(ctx: &mut Context, res_type: &Ty) -> Option<FieldExpr> {
        track_expr(
            ExprKind::Field,
            limit_expr_depth(limit_arith_depth(Box::new(
                FieldExpr::generate_expr_internal,
            ))),
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
        ctx.expr_depth <= ctx.policy.max_expr_depth && ctx.arith_depth <= ctx.policy.max_arith_depth
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IndexExpr {
    pub base: Box<Expr>,
    pub index: Box<Expr>,
}

impl From<IndexExpr> for Expr {
    fn from(expr: IndexExpr) -> Expr {
        Expr::Index(expr)
    }
}

impl From<IndexExpr> for PlaceExpr {
    fn from(expr: IndexExpr) -> PlaceExpr {
        PlaceExpr::Index(expr)
    }
}

impl IndexExpr {
    pub fn generate_expr(ctx: &mut Context, res_type: &Ty) -> Option<IndexExpr> {
        track_expr(
            ExprKind::Index,
            limit_expr_depth(limit_arith_depth(Box::new(
                IndexExpr::generate_expr_internal,
            ))),
        )(ctx, res_type)
    }

    fn generate_expr_internal(ctx: &mut Context, res_type: &Ty) -> Option<IndexExpr> {
        if res_type.array_depth() + 1 > ctx.policy.max_array_depth {
            return None;
        }
        // [Struct1(5), Struct1(5), Struct1(5)][0] is invalid if Struct1 is not copy
        // However [Struct1(5), Struct1(5), Struct1(5)][0].0 should work
        if !res_type.is_copy() {
            return None;
        }
        // TODO: See if we can make this work
        // if !res_type.is_copy() {
        //     return None;
        // }

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
        ctx.expr_depth <= ctx.policy.max_expr_depth && ctx.arith_depth <= ctx.policy.max_arith_depth
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum StructExpr {
    /// Tuple struct such as `S { field1: value1, field2: value2 }`.
    Tuple(TupleStructExpr),
    /// Field struct such as `S(5_u32, "hello")`.
    Field(FieldStructExpr),
}

impl From<StructExpr> for Expr {
    fn from(expr: StructExpr) -> Expr {
        Expr::Struct(expr)
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

#[derive(Debug, Clone, PartialEq)]
pub struct TupleStructExpr {
    pub struct_name: String,
    pub fields: TupleExpr,
}

impl From<TupleStructExpr> for StructExpr {
    fn from(expr: TupleStructExpr) -> StructExpr {
        StructExpr::Tuple(expr)
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

#[derive(Debug, Clone, PartialEq)]
pub struct FieldStructExpr {
    pub struct_name: String,
    pub fields: Vec<Field>,
}

impl From<FieldStructExpr> for StructExpr {
    fn from(expr: FieldStructExpr) -> StructExpr {
        StructExpr::Field(expr)
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

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub name: String,
    pub expr: Expr,
}

impl Field {
    pub fn generate_field(ctx: &mut Context, field_def: &FieldDef) -> Option<Field> {
        Some(Field {
            name: field_def.name.clone(),
            expr: Expr::fuzz_expr(ctx, &*field_def.ty)?,
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReferenceExpr {
    pub mutability: bool,
    pub expr: Box<Expr>,
}

impl From<ReferenceExpr> for Expr {
    fn from(expr: ReferenceExpr) -> Expr {
        Expr::Reference(expr)
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[non_exhaustive]
pub enum ExprKind {
    Literal,
    Binary,
    Unary,
    Cast,
    If,
    Block,
    Ident,
    Assign,
    Index,
    Field,
    __Nonexhaustive,
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
