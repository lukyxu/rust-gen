use crate::ast::stmt::Stmt;
use crate::ast::ty::{
    ArrayTy, FieldDef, FieldStructTy, FloatTy, IntTy, PrimTy, StructTy, TupleStructTy, TupleTy, Ty,
    UIntTy,
};
use rand::prelude::SliceRandom;

use crate::ast::op::{BinaryOp, UnaryOp};
use crate::context::Context;
use serde::{Deserialize, Serialize};
use std::cmp::{max, min};
use rand::Rng;

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
    Field(FieldExpr),
    Index(IndexExpr),
    Struct(StructExpr),
}

impl Expr {
    pub fn generate_expr(ctx: &mut Context, res_type: &Ty) -> Option<Expr> {
        if ctx.expr_depth > ctx.policy.max_expr_depth {
            return None;
        }

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
                ExprKind::Field => FieldExpr::generate_expr(ctx, res_type).map(From::from),
                ExprKind::Index => IndexExpr::generate_expr(ctx, res_type).map(From::from),
                _ => panic!("ExprKind {:?} not supported yet", expr_kind),
            };
            if res.is_none() {
                num_failed_attempts += 1;
                ctx.statistics.failed_expr_generations += 1;
                ctx.statistics.max_failed_generation_depth = max(
                    num_failed_attempts,
                    ctx.statistics.max_failed_generation_depth,
                );
            } else {
                *ctx.statistics.expr_counter.entry(expr_kind).or_insert(0) += 1;
            }
        }
        res
    }

    fn generate_arith_expr<T>(
        ctx: &mut Context,
        res_type: &Ty,
        f: fn(&mut Context, &Ty) -> Option<T>,
    ) -> Option<T> {
        if ctx.arith_depth > ctx.policy.max_arith_depth {
            return None;
        }
        ctx.arith_depth += 1;
        let res = f(ctx, res_type);
        ctx.arith_depth -= 1;
        res
    }
}

#[cfg(test)]
impl Expr {
    pub fn bool(b: bool) -> Expr {
        Expr::Literal(LitExpr::Bool(b))
    }

    pub fn i8(i: i8) -> Expr {
        Expr::Literal(LitExpr::Int(i as u128, LitIntTy::Signed(IntTy::I8)))
    }

    pub fn u8(u: u8) -> Expr {
        Expr::Literal(LitExpr::Int(u as u128, LitIntTy::Unsigned(UIntTy::U8)))
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
    Int(u128, LitIntTy),
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
        match res_type {
            Ty::Unit => Some(TupleExpr::empty_tuple()),
            Ty::Prim(PrimTy::Bool) => Some(LitExpr::Bool(ctx.choose_boolean_true()).into()),
            Ty::Prim(PrimTy::Int(t)) => {
                let val = t.rand_val(ctx);
                let expr_type = if matches!(t, IntTy::I32) && ctx.choose_unsuffixed_int() {
                    LitIntTy::Unsuffixed
                } else {
                    LitIntTy::Signed(*t)
                };
                Some(LitExpr::Int(val, expr_type).into())
            }
            Ty::Prim(PrimTy::UInt(t)) => {
                let val = t.rand_val(ctx);
                Some(LitExpr::Int(val, LitIntTy::Unsigned(*t)).into())
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
    /// `64_i32`
    Signed(IntTy),
    /// `64_u32`
    Unsigned(UIntTy),
    /// `64`
    /// Defaults to i32
    Unsuffixed,
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
        Expr::generate_arith_expr(ctx, res_type, BinaryExpr::generate_expr_internal)
    }

    fn generate_expr_internal(ctx: &mut Context, res_type: &Ty) -> Option<BinaryExpr> {
        let op = match res_type {
            Ty::Prim(PrimTy::Bool) => ctx.choose_binary_bool_op(),
            Ty::Prim(PrimTy::Int(_) | PrimTy::UInt(_)) => ctx.choose_binary_int_op(),
            Ty::Tuple(_) | Ty::Array(..) | Ty::Unit | Ty::Struct(..) => return None,
            _ => panic!(
                "Binary operations for {} not supported",
                res_type.to_string()
            ),
        };
        let lhs = Box::new(Expr::generate_expr(ctx, res_type)?);
        let rhs = Box::new(Expr::generate_expr(ctx, res_type)?);
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
    #[allow(dead_code)]
    pub fn generate_expr(ctx: &mut Context, res_type: &Ty) -> Option<Expr> {
        Expr::generate_arith_expr(ctx, res_type, UnaryExpr::generate_expr_internal)
    }

    #[allow(dead_code)]
    pub fn generate_expr_internal(ctx: &mut Context, res_type: &Ty) -> Option<Expr> {
        let op = match res_type {
            Ty::Prim(PrimTy::Bool) => UnaryOp::Not,
            Ty::Prim(PrimTy::Int(_)) => UnaryOp::Neg,
            _ => return None,
        };
        let expr = Box::new(Expr::generate_expr(ctx, res_type)?);
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
        Expr::generate_arith_expr(ctx, res_type, CastExpr::generate_expr_internal)
    }

    pub fn generate_expr_internal(ctx: &mut Context, res_type: &Ty) -> Option<Expr> {
        let source_type: Ty = PrimTy::generate_type(ctx)?.into();
        if !source_type.compatible_cast(res_type) {
            return None;
        }
        let expr = Box::new(Expr::generate_expr(ctx, &source_type)?);
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
        if ctx.if_else_depth > ctx.policy.max_if_else_depth {
            return None;
        }
        let outer_symbol_table = ctx.type_symbol_table.clone();
        ctx.if_else_depth += 1;
        let cond = Expr::generate_expr(ctx, &PrimTy::Bool.into());
        let if_expr = match cond {
            None => None,
            Some(cond) => {
                let then = Box::new(BlockExpr::generate_block_expr(ctx, res_type)?);
                let otherwise = if !res_type.is_unit() || ctx.choose_otherwise_if_stmt() {
                    Some(Box::new(
                        BlockExpr::generate_block_expr(ctx, res_type).unwrap(),
                    ))
                } else {
                    None
                };
                Some(IfExpr {
                    condition: Box::new(cond),
                    then,
                    otherwise,
                })
            }
        };
        ctx.type_symbol_table = outer_symbol_table;
        ctx.if_else_depth -= 1;
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
        BlockExpr::generate_block_expr(ctx, res_type)
    }
    pub fn generate_block_expr(ctx: &mut Context, res_type: &Ty) -> Option<BlockExpr> {
        if ctx.block_depth > ctx.policy.max_block_depth {
            return None;
        }
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
    fn generate_expr(ctx: &mut Context, res_type: &Ty) -> Option<IdentExpr> {
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
    fn empty_tuple() -> Expr {
        Expr::Tuple(TupleExpr { tuple: vec![] })
    }

    fn generate_expr(ctx: &mut Context, res_type: &TupleTy) -> Option<TupleExpr> {
        let mut res = vec![];
        for ty in &res_type.tuple {
            let prev_max_expr_depth = ctx.policy.max_expr_depth;
            ctx.policy.max_expr_depth = min(
                ctx.policy.max_expr_depth,
                ctx.policy.max_expr_depth_in_tuple,
            );
            let expr = Expr::generate_expr(ctx, ty);
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
    fn generate_expr(ctx: &mut Context, res_type: &Ty) -> Option<AssignExpr> {
        if !res_type.is_unit() {
            return None;
        };
        let ty = Ty::generate_type(ctx)?;
        let mut_ident_exprs = ctx.type_symbol_table.get_mut_ident_exprs_by_type(&ty);
        let ident_expr = mut_ident_exprs.choose(&mut ctx.rng)?.clone();

        Some(AssignExpr {
            name: ident_expr.name,
            rhs: Box::new(Expr::generate_expr(ctx, &ident_expr.ty)?),
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
    fn generate_expr(ctx: &mut Context, res_type: &ArrayTy) -> Option<ArrayExpr> {
        let mut res = vec![];
        for ty in res_type.iter() {
            let prev_max_expr_depth = ctx.policy.max_expr_depth;
            ctx.policy.max_expr_depth = min(
                ctx.policy.max_expr_depth,
                ctx.policy.max_expr_depth_in_array,
            );
            let expr = Expr::generate_expr(ctx, &ty);
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
    fn generate_expr(ctx: &mut Context, res_type: &Ty) -> Option<FieldExpr> {
        Expr::generate_arith_expr(ctx, res_type, FieldExpr::generate_expr_internal)
    }

    fn generate_expr_internal(ctx: &mut Context, res_type: &Ty) -> Option<FieldExpr> {
        if ctx.rng.gen_bool(0.5) {
            FieldExpr::generate_tuple_field_expr(ctx, res_type)
        } else {
            FieldExpr::generate_struct_field_expr(ctx, res_type)
        }
    }

    fn generate_tuple_field_expr(ctx: &mut Context, res_type: &Ty) -> Option<FieldExpr> {
        let tuple = TupleTy::generate_type(ctx, Some(res_type.clone()))?;

        let base = Box::new(Expr::generate_expr(ctx, &tuple.clone().into())?);
        let indexes: Vec<usize> = (&tuple)
            .into_iter()
            .enumerate()
            .filter_map(|(i, ty)| if ty == res_type { Some(i) } else { None })
            .collect();

        let member = Member::Unnamed(*indexes.choose(&mut ctx.rng).unwrap());
        Some(FieldExpr { base, member })
    }

    fn generate_struct_field_expr(ctx: &mut Context, res_type: &Ty) -> Option<FieldExpr> {
        let struct_ty = StructTy::generate_type(ctx, Some(res_type.clone()))?;
        let base = Box::new(Expr::generate_expr(ctx, &struct_ty.clone().into())?);
        let member = match struct_ty {
            StructTy::Field(field_struct) => {
                Member::Named(
                    field_struct
                        .fields
                        .iter()
                        .filter(|f| &*f.ty == res_type)
                        .collect::<Vec<_>>()
                        .choose(&mut ctx.rng)
                        .unwrap()
                        .name
                        .clone(),
                )
            }
            StructTy::Tuple(tuple_struct) => {
                Member::Unnamed(
                    *(&tuple_struct.fields)
                        .into_iter()
                        .enumerate()
                        .filter_map(|(i, ty)| if ty == res_type { Some(i) } else { None })
                        .collect::<Vec<_>>()
                        .choose(&mut ctx.rng)
                        .unwrap(),
                )
            }
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
    fn generate_expr(ctx: &mut Context, res_type: &Ty) -> Option<IndexExpr> {
        Expr::generate_arith_expr(ctx, res_type, IndexExpr::generate_expr_internal)
    }

    fn generate_expr_internal(ctx: &mut Context, res_type: &Ty) -> Option<IndexExpr> {
        let array_type: ArrayTy = ArrayTy::generate_type(ctx, Some(res_type.clone()))?;
        let base = Box::new(Expr::generate_expr(ctx, &array_type.clone().into())?);
        let index = Box::new(Expr::generate_expr(
            ctx,
            &PrimTy::UInt(UIntTy::USize).into(),
        )?);
        let inbound_index = Box::new(Expr::Binary(BinaryExpr {
            lhs: index,
            rhs: Box::new(Expr::Literal(LitExpr::Int(
                array_type.len as u128,
                LitIntTy::Unsigned(UIntTy::USize),
            ))),
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
    fn generate_expr(ctx: &mut Context, res_type: &StructTy) -> Option<StructExpr> {
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
    fn generate_expr(ctx: &mut Context, res_type: &TupleStructTy) -> Option<TupleStructExpr> {
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
    fn generate_expr(ctx: &mut Context, res_type: &FieldStructTy) -> Option<FieldStructExpr> {
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
    fn generate_field(ctx: &mut Context, field_def: &FieldDef) -> Option<Field> {
        Some(Field {
            name: field_def.name.clone(),
            expr: Expr::generate_expr(ctx, &*field_def.ty)?,
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
