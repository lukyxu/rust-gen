use crate::ast::stmt::Stmt;
use crate::ast::ty::{
    ArrayTy, FieldDef, FieldStructTy, FloatTy, IntTy, PrimTy, StructTy, TupleStructTy, TupleTy, Ty,
    UIntTy,
};
use rand::prelude::SliceRandom;

use crate::ast::op::{BinaryOp, UnaryOp};
use crate::ast::utils::{
    limit_arith_depth, limit_block_depth, limit_expr_depth, limit_if_else_depth, track_expr,
};
use crate::context::Context;
use rand::Rng;
use serde::{Deserialize, Serialize};
use std::cmp::min;

#[derive(Debug, Clone, PartialEq)]
#[non_exhaustive]
pub enum Expr {
    /// Literal such as `1`, `"foo"`
    Literal(LitExpr),
    /// Binary operation such as `a + b`, `a * b`
    Binary(BinaryExpr),
    /// Unary operation such as `!x`
    Unary(UnaryExpr),
    /// Cast expression such as `x as u64`
    #[allow(dead_code)]
    Cast(CastExpr),
    /// If expression with optional `else` block
    /// `if expr { block } else { expr }`
    If(IfExpr),
    /// Block expression
    Block(BlockExpr), // TODO: Path, Assign, Arrays, Box, Tuples
    /// A variable access such as `x` (Equivalent to Rust Path in Rust compiler)
    Ident(IdentExpr),
    Tuple(TupleExpr),
    Assign(AssignExpr),
    Array(ArrayExpr),
    Index(IndexExpr),
    Field(FieldExpr),
    Struct(StructExpr),
}

impl Expr {
    pub fn fuzz_expr(ctx: &mut Context, res_type: &Ty) -> Option<Expr> {
        let mut res: Option<Expr> = None;
        let mut num_failed_attempts = 0;
        while res.is_none() && num_failed_attempts < ctx.policy.max_expr_attempts {
            let expr_kind = ctx.choose_expr_kind();
            res = match expr_kind {
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
            };
            if res.is_none() {
                num_failed_attempts += 1;
            }
        }
        res
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
    Str(String),
    #[allow(dead_code)]
    Byte(u8),
    #[allow(dead_code)]
    Char(char),
    Int(LitIntExpr),
    #[allow(dead_code)]
    Float(String, LitFloatTy),
    Bool(bool),
}

impl From<LitExpr> for Expr {
    fn from(expr: LitExpr) -> Self {
        Expr::Literal(expr)
    }
}

impl LitExpr {
    pub fn generate_expr(ctx: &mut Context, res_type: &Ty) -> Option<Expr> {
        track_expr(ExprKind::Literal, Box::new(LitExpr::generate_expr_internal))(ctx, res_type)
    }

    pub fn generate_expr_internal(ctx: &mut Context, res_type: &Ty) -> Option<Expr> {
        match res_type {
            Ty::Unit => Some(TupleExpr::empty_tuple()),
            Ty::Prim(PrimTy::Bool) => Some(LitExpr::Bool(ctx.choose_boolean_true()).into()),
            Ty::Prim(PrimTy::Int(t)) => {
                let value = t.rand_val(ctx);
                Some(LitIntExpr::new(value, (*t).into()).into())
            }
            Ty::Prim(PrimTy::UInt(t)) => {
                let value = t.rand_val(ctx);
                Some(LitIntExpr::new(value, (*t).into()).into())
            }
            Ty::Tuple(tuple_ty) => TupleExpr::generate_expr(ctx, tuple_ty).map(From::from),
            Ty::Array(array_ty) => ArrayExpr::generate_expr(ctx, array_ty).map(From::from),
            Ty::Struct(struct_ty) => StructExpr::generate_expr(ctx, struct_ty).map(From::from),
            _ => panic!(
                "Literal type for {} not supported yet",
                res_type.to_string()
            ),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum LitIntTy {
    Signed(IntTy),
    Unsigned(UIntTy),
}

#[derive(Debug, Copy, Clone, PartialEq)]
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
    /// Float literal with suffix such as `1f32`, `1E10f32`
    Suffixed(FloatTy),
    /// Float literal without suffix such as `1.0`, `1.0E10`
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
        let op = match res_type {
            Ty::Prim(PrimTy::Bool) => ctx.choose_binary_bool_op(),
            Ty::Prim(PrimTy::Int(_) | PrimTy::UInt(_)) => ctx.choose_binary_int_op(),
            _ => return None,
        };
        let args_type = op
            .get_compatible_arg_type(&res_type)
            .choose(&mut ctx.rng)
            .cloned()
            .unwrap();
        let lhs = Box::new(Expr::fuzz_expr(ctx, &args_type)?);
        let rhs = Box::new(Expr::fuzz_expr(ctx, &args_type)?);
        *ctx.statistics.bin_op_counter.entry(op).or_insert(0) += 1;
        Some(BinaryExpr { lhs, rhs, op })
    }
}

impl From<BinaryExpr> for Expr {
    fn from(expr: BinaryExpr) -> Self {
        Expr::Binary(expr)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpr {
    pub expr: Box<Expr>,
    pub op: UnaryOp,
}

impl From<UnaryExpr> for Expr {
    fn from(expr: UnaryExpr) -> Self {
        Expr::Unary(expr)
    }
}

impl UnaryExpr {
    pub fn generate_expr(ctx: &mut Context, res_type: &Ty) -> Option<Expr> {
        track_expr(
            ExprKind::Unary,
            limit_expr_depth(limit_arith_depth(Box::new(
                UnaryExpr::generate_expr_internal,
            ))),
        )(ctx, res_type)
    }

    pub fn generate_expr_internal(ctx: &mut Context, res_type: &Ty) -> Option<Expr> {
        let op = match res_type {
            Ty::Prim(PrimTy::Bool) => UnaryOp::Not,
            Ty::Prim(PrimTy::Int(_)) => UnaryOp::Neg,
            _ => return None,
        };
        let expr = Box::new(Expr::fuzz_expr(ctx, res_type)?);
        *ctx.statistics.un_op_counter.entry(op).or_insert(0) += 1;
        Some(Expr::Unary(UnaryExpr { expr, op }))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CastExpr {
    pub expr: Box<Expr>,
    pub ty: Ty,
}

impl From<CastExpr> for Expr {
    fn from(expr: CastExpr) -> Self {
        Expr::Cast(expr)
    }
}

impl CastExpr {
    pub fn generate_expr(ctx: &mut Context, res_type: &Ty) -> Option<Expr> {
        track_expr(
            ExprKind::Cast,
            limit_expr_depth(limit_arith_depth(Box::new(
                CastExpr::generate_expr_internal,
            ))),
        )(ctx, res_type)
    }

    pub fn generate_expr_internal(ctx: &mut Context, res_type: &Ty) -> Option<Expr> {
        let source_type: Ty = PrimTy::generate_type(ctx)?.into();
        if !source_type.compatible_cast(res_type) {
            return None;
        }
        let expr = Box::new(Expr::fuzz_expr(ctx, &source_type)?);
        Some(Expr::Cast(CastExpr {
            expr,
            ty: res_type.clone(),
        }))
    }
}

// TODO: Improve IfExpr formatting in printing
#[derive(Debug, Clone, PartialEq)]
pub struct IfExpr {
    pub condition: Box<Expr>,
    pub then: Box<BlockExpr>,
    pub otherwise: Option<Box<BlockExpr>>,
}

impl From<IfExpr> for Expr {
    fn from(expr: IfExpr) -> Self {
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
        let outer_symbol_table = ctx.type_symbol_table.clone();
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
        ctx.type_symbol_table = outer_symbol_table;
        if_expr
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlockExpr {
    pub stmts: Vec<Stmt>,
}

impl From<BlockExpr> for Expr {
    fn from(expr: BlockExpr) -> Self {
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
        let mut num_stmts = ctx.choose_num_stmts();
        if !res_type.is_unit() {
            num_stmts -= 1;
        }
        for _ in 0..num_stmts {
            // TODO: Make sure these statements are not expression statements
            stmts.push(Stmt::generate_non_expr_stmt(ctx)?);
        }
        if !res_type.is_unit() {
            stmts.push(Stmt::generate_expr_stmt(ctx, res_type)?);
        }
        ctx.type_symbol_table = outer_symbol_table;
        Some(BlockExpr { stmts })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IdentExpr {
    pub name: String,
    // TODO: remove type
    pub ty: Ty,
}

impl From<IdentExpr> for Expr {
    fn from(expr: IdentExpr) -> Self {
        Expr::Ident(expr)
    }
}

impl IdentExpr {
    pub fn generate_expr(ctx: &mut Context, res_type: &Ty) -> Option<IdentExpr> {
        track_expr(ExprKind::Ident, Box::new(IdentExpr::generate_expr_internal))(ctx, res_type)
    }

    fn generate_expr_internal(ctx: &mut Context, res_type: &Ty) -> Option<IdentExpr> {
        ctx.choose_ident_expr_by_type(res_type)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TupleExpr {
    pub tuple: Vec<Expr>,
}

impl From<TupleExpr> for Expr {
    fn from(expr: TupleExpr) -> Self {
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
            let prev_max_expr_depth = ctx.policy.max_expr_depth;
            ctx.policy.max_expr_depth = min(
                ctx.policy.max_expr_depth,
                ctx.policy.max_expr_depth_in_tuple,
            );
            let expr = Expr::fuzz_expr(ctx, ty);
            ctx.policy.max_expr_depth = prev_max_expr_depth;
            res.push(expr?);
        }
        Some(TupleExpr { tuple: res })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssignExpr {
    pub name: String,
    pub rhs: Box<Expr>,
}

impl From<AssignExpr> for Expr {
    fn from(expr: AssignExpr) -> Self {
        Expr::Assign(expr)
    }
}

impl AssignExpr {
    pub fn generate_expr(ctx: &mut Context, res_type: &Ty) -> Option<AssignExpr> {
        track_expr(
            ExprKind::Assign,
            Box::new(AssignExpr::generate_expr_internal),
        )(ctx, res_type)
    }

    fn generate_expr_internal(ctx: &mut Context, res_type: &Ty) -> Option<AssignExpr> {
        if !res_type.is_unit() {
            return None;
        };
        let ty = Ty::generate_type(ctx)?;
        let mut_ident_exprs = ctx.type_symbol_table.get_mut_ident_exprs_by_type(&ty);
        let ident_expr = mut_ident_exprs.choose(&mut ctx.rng)?.clone();

        Some(AssignExpr {
            name: ident_expr.name,
            rhs: Box::new(Expr::fuzz_expr(ctx, &ident_expr.ty)?),
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayExpr {
    pub array: Vec<Expr>,
}

impl From<ArrayExpr> for Expr {
    fn from(expr: ArrayExpr) -> Self {
        Expr::Array(expr)
    }
}

impl ArrayExpr {
    pub fn generate_expr(ctx: &mut Context, res_type: &ArrayTy) -> Option<ArrayExpr> {
        let mut res = vec![];
        for ty in res_type.iter() {
            let prev_max_expr_depth = ctx.policy.max_expr_depth;
            ctx.policy.max_expr_depth = min(
                ctx.policy.max_expr_depth,
                ctx.policy.max_expr_depth_in_array,
            );
            let expr = Expr::fuzz_expr(ctx, &ty);
            ctx.policy.max_expr_depth = prev_max_expr_depth;
            res.push(expr?);
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
    fn from(expr: FieldExpr) -> Self {
        Expr::Field(expr)
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
        if ctx.rng.gen_bool(0.5) {
            FieldExpr::generate_tuple_field_expr(ctx, res_type)
        } else {
            FieldExpr::generate_struct_field_expr(ctx, res_type)
        }
    }

    pub fn generate_tuple_field_expr(ctx: &mut Context, res_type: &Ty) -> Option<FieldExpr> {
        let tuple = TupleTy::generate_type(ctx, Some(res_type.clone()))?;

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
        let struct_ty = StructTy::generate_type(ctx, Some(res_type.clone()))?;
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
}

#[derive(Debug, Clone, PartialEq)]
pub struct IndexExpr {
    pub base: Box<Expr>,
    pub index: Box<Expr>,
}

impl From<IndexExpr> for Expr {
    fn from(expr: IndexExpr) -> Self {
        Expr::Index(expr)
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
        let array_type: ArrayTy = ArrayTy::generate_type(ctx, Some(res_type.clone()))?;
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
}

#[derive(Debug, Clone, PartialEq)]
pub enum StructExpr {
    Tuple(TupleStructExpr),
    Field(FieldStructExpr),
}

impl From<StructExpr> for Expr {
    fn from(expr: StructExpr) -> Expr {
        Expr::Struct(expr)
    }
}

impl StructExpr {
    pub fn generate_expr(ctx: &mut Context, res_type: &StructTy) -> Option<StructExpr> {
        let prev_max_expr_depth = ctx.policy.max_expr_depth;
        ctx.policy.max_expr_depth = min(
            ctx.policy.max_expr_depth,
            ctx.policy.max_expr_depth_in_struct,
        );
        let res = match res_type {
            StructTy::Field(res_type) => {
                FieldStructExpr::generate_expr(ctx, res_type).map(From::from)
            }
            StructTy::Tuple(res_type) => {
                TupleStructExpr::generate_expr(ctx, res_type).map(From::from)
            }
        };
        ctx.policy.max_expr_depth = prev_max_expr_depth;
        res
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TupleStructExpr {
    pub struct_name: String,
    pub fields: TupleExpr,
}

impl From<TupleStructExpr> for StructExpr {
    fn from(expr: TupleStructExpr) -> Self {
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
    fn from(expr: FieldStructExpr) -> Self {
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
