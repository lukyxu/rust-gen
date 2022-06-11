use crate::ast::expr::{ExprKind, IdentExpr};
use crate::ast::item::ItemKind;
use crate::ast::op::{BinaryOp, UnaryOp};
use crate::ast::stmt::StmtKind;
use crate::ast::ty::{ArrayTy, Lifetime, PrimTy, StructTy, TupleTy, Ty, TyKind};
use crate::policy::Policy;
use crate::symbol_table::ty::TypeSymbolTable;

use crate::generate::expr::GENERABLE_EXPR_FNS;
use crate::statistics::generation::GenerationStatistics;
use rand::prelude::{SliceRandom, StdRng};
use rand::{thread_rng, Rng, SeedableRng};
use std::collections::BTreeSet;

pub struct Context {
    pub policy: Policy,
    pub name_handler: NameHandler,
    pub type_symbol_table: TypeSymbolTable,
    pub statistics: GenerationStatistics,
    pub rng: StdRng,
    pub gen_new_array_types: bool,
    pub gen_new_tuple_types: bool,
    pub gen_new_struct_types: bool,
    pub expr_depth: usize,
    pub if_else_depth: usize,
    pub block_depth: usize,
    pub arith_depth: usize,

    pub struct_ctx: Option<StructContext>,
    pub gen_only_copy_type: bool,

    pub array_type_dist: Vec<(ArrayTy, f64)>,
    pub tuple_type_dist: Vec<(TupleTy, f64)>,
    pub struct_type_dist: Vec<(StructTy, f64)>,
}

impl Context {
    pub fn new(seed: Option<u64>) -> Context {
        Context::with_policy(seed, &Policy::default())
    }

    pub fn with_policy(seed: Option<u64>, policy: &Policy) -> Context {
        let rng = if let Some(seed) = seed {
            StdRng::seed_from_u64(seed)
        } else {
            StdRng::seed_from_u64(thread_rng().gen())
        };
        Context {
            policy: policy.clone(),
            name_handler: NameHandler::default(),
            type_symbol_table: TypeSymbolTable::default(),
            statistics: GenerationStatistics::default(),
            rng,
            gen_new_array_types: true,
            gen_new_tuple_types: true,
            gen_new_struct_types: true,
            expr_depth: 0,
            if_else_depth: 0,
            block_depth: 0,
            arith_depth: 0,

            struct_ctx: None,
            gen_only_copy_type: false,

            array_type_dist: policy.default_array_type_dist.clone(),
            tuple_type_dist: policy.default_tuple_type_dist.clone(),
            struct_type_dist: policy.default_struct_type_dist.clone(),
        }
    }
}

pub fn choose<T: Clone>(dist: &Vec<(T, f64)>, rng: &mut StdRng) -> Option<T> {
    if dist.is_empty() {
        return None;
    };
    Some(dist.choose_weighted(rng, |item| item.1).unwrap().0.clone())
}

impl Context {
    pub fn generate_only_copy_type(&mut self) -> bool {
        if let Some(struct_ctx) = &self.struct_ctx {
            struct_ctx.generate_copy_struct
        } else {
            self.gen_only_copy_type
        }
    }

    pub fn choose_item_kind(&mut self) -> ItemKind {
        choose(&self.policy.item_dist, &mut self.rng).unwrap()
    }

    pub fn choose_stmt_kind(&mut self) -> StmtKind {
        choose(&self.policy.stmt_dist, &mut self.rng).unwrap()
    }

    pub fn choose_prim_type(&mut self) -> Option<PrimTy> {
        choose(&self.policy.prim_type_dist, &mut self.rng)
    }

    pub fn choose_array_type(&mut self, elem_ty: &Option<Ty>) -> Option<ArrayTy> {
        let mut dist: Vec<(ArrayTy, f64)> = self.array_type_dist.clone();
        if let Some(elem_ty) = elem_ty {
            dist.retain(|(array_ty, _)| &*array_ty.base_ty == elem_ty);
        }
        dist.retain(|(array_ty, _)| array_ty.array_depth() <= self.policy.max_array_depth);
        if self.policy.disable_lifetime && self.struct_ctx.is_some() {
            dist.retain(|(array_ty, _)| !array_ty.require_lifetime());
        }
        if self.generate_only_copy_type() {
            dist.retain(|(array_ty, _)| array_ty.is_copy());
        }
        choose(&dist, &mut self.rng)
    }

    pub fn choose_array_length(&mut self) -> usize {
        self.policy.array_length_dist.sample(&mut self.rng)
    }

    pub fn choose_tuple_type(&mut self, elem_ty: &Option<Ty>) -> Option<TupleTy> {
        let mut dist: Vec<(TupleTy, f64)> = self.tuple_type_dist.clone();
        if let Some(elem_ty) = elem_ty {
            dist.retain(|(tuple_ty, _)| tuple_ty.tuple.contains(elem_ty));
        }
        dist.retain(|(tuple_ty, _)| tuple_ty.tuple_depth() <= self.policy.max_tuple_depth);
        if self.policy.disable_lifetime && self.struct_ctx.is_some() {
            dist.retain(|(tuple_ty, _)| !tuple_ty.require_lifetime());
        }
        if self.generate_only_copy_type() {
            dist.retain(|(tuple_ty, _)| tuple_ty.is_copy());
        }
        choose(&dist, &mut self.rng)
    }

    pub fn choose_tuple_length(&mut self) -> usize {
        self.policy.tuple_length_dist.sample(&mut self.rng)
    }

    pub fn choose_struct_type(&mut self, elem_ty: &Option<Ty>) -> Option<StructTy> {
        let mut dist: Vec<(StructTy, f64)> = self.struct_type_dist.clone();
        if let Some(elem_ty) = elem_ty {
            dist.retain(|(struct_ty, _)| match struct_ty {
                StructTy::Field(field) => field
                    .fields
                    .iter()
                    .any(|field_def| &*field_def.ty == elem_ty),
                StructTy::Tuple(tuple) => (&tuple.fields).into_iter().any(|ty| ty == elem_ty),
            });
        }
        dist.retain(|(struct_ty, _)| struct_ty.struct_depth() <= self.policy.max_tuple_depth);
        if self.policy.disable_lifetime && self.struct_ctx.is_some() {
            dist.retain(|(struct_ty, _)| !struct_ty.require_lifetime());
        }
        if self.generate_only_copy_type() {
            dist.retain(|(struct_ty, _)| struct_ty.is_copy());
        }
        choose(&dist, &mut self.rng)
    }

    pub fn choose_struct_length(&mut self) -> usize {
        self.policy.struct_length_dist.sample(&mut self.rng)
    }

    pub fn choose_field_struct(&mut self) -> bool {
        self.rng.gen_bool(self.policy.field_struct_prob)
    }

    pub fn choose_ty_kind(&mut self) -> TyKind {
        let mut dist = self.policy.type_dist.clone();
        if self.policy.disable_lifetime && self.struct_ctx.is_some() {
            dist.retain(|(ty_kind, _)| !matches!(ty_kind, TyKind::Reference));
        }
        choose(&dist, &mut self.rng).unwrap()
    }

    pub fn choose_expr_kind(&mut self, res_type: &Ty) -> ExprKind {
        let mut dist: Vec<(ExprKind, f64)> = self.policy.expr_dist.clone();
        dist.retain(|(expr_kind, _w)| {
            GENERABLE_EXPR_FNS
                .get(expr_kind)
                .map_or(true, |f| f(self, res_type))
        });
        choose(&dist, &mut self.rng).unwrap()
    }

    pub fn choose_place_expr_kind(&mut self) -> ExprKind {
        let place_exprs = [ExprKind::Ident, ExprKind::Field, ExprKind::Index];
        let place_exprs_dist: Vec<(ExprKind, f64)> = self
            .policy
            .expr_dist
            .iter()
            .filter(|x| place_exprs.contains(&x.0))
            .copied()
            .collect();
        choose(&place_exprs_dist, &mut self.rng).unwrap()
    }

    pub fn choose_binary_op(&mut self, res_type: &Ty) -> Option<BinaryOp> {
        let filtered_binary_ops: Vec<(BinaryOp, f64)> = self
            .policy
            .binary_op_dist
            .iter()
            .filter(|x| x.0.get_compatible_return_types(self).contains(res_type))
            .copied()
            .collect();
        choose(&filtered_binary_ops, &mut self.rng)
    }

    pub fn choose_unary_op(&mut self, res_type: &Ty) -> Option<UnaryOp> {
        let filtered_unary_ops: Vec<(UnaryOp, f64)> = self
            .policy
            .unary_op_dist
            .iter()
            .filter(|x| x.0.get_compatible_return_types(self).contains(res_type))
            .copied()
            .collect();
        choose(&filtered_unary_ops, &mut self.rng)
    }

    pub fn choose_num_items(&mut self) -> usize {
        self.policy.num_item_dist.sample(&mut self.rng)
    }

    pub fn choose_num_stmts(&mut self) -> usize {
        self.policy.num_stmt_dist.sample(&mut self.rng)
    }

    pub fn create_var_name(&mut self) -> String {
        self.name_handler.create_var_name()
    }

    pub fn create_struct_name(&mut self) -> String {
        self.name_handler.create_struct_name()
    }

    pub fn create_field_name(&mut self, index: usize) -> String {
        NameHandler::create_field_name(index)
    }

    pub fn create_function_name(&mut self) -> String {
        self.name_handler.create_function_name()
    }

    pub fn create_lifetime_name(&mut self) -> Option<String> {
        Some(
            self.name_handler
                .create_lifetime_name(self.struct_ctx.as_ref()?.lifetimes.len()),
        )
    }

    pub fn choose_new_array_type(&mut self) -> bool {
        self.gen_new_array_types && self.rng.gen_bool(self.policy.new_array_prob)
    }

    pub fn choose_new_tuple_type(&mut self) -> bool {
        self.gen_new_tuple_types && self.rng.gen_bool(self.policy.new_tuple_prob)
    }

    pub fn choose_otherwise_if_stmt(&mut self) -> bool {
        self.rng.gen_bool(self.policy.otherwise_if_stmt_prob)
    }

    pub fn choose_boolean_true(&mut self) -> bool {
        self.rng.gen_bool(self.policy.bool_true_prob)
    }

    pub fn choose_mutability(&mut self) -> bool {
        self.rng.gen_bool(self.policy.mutability_prob)
    }

    pub fn choose_new_lifetime(&mut self) -> bool {
        self.rng.gen_bool(self.policy.new_lifetime_prob)
    }

    pub fn choose_ident_expr_by_type(&mut self, ty: &Ty) -> Option<IdentExpr> {
        let ident_exprs = self.type_symbol_table.get_ident_exprs_by_type(ty);
        ident_exprs.choose(&mut self.rng).cloned()
    }

    pub fn choose_copy_tuple_struct(&mut self) -> bool {
        self.rng.gen_bool(self.policy.tuple_struct_copy_prob)
    }

    pub fn choose_copy_field_struct(&mut self) -> bool {
        self.rng.gen_bool(self.policy.field_struct_copy_prob)
    }
}

#[derive(Clone)]
pub struct ContextSnapshot {
    name_handler: NameHandler,
    type_symbol_table: TypeSymbolTable,
}

impl Context {
    pub fn snapshot(&self) -> ContextSnapshot {
        ContextSnapshot {
            name_handler: self.name_handler.clone(),
            type_symbol_table: self.type_symbol_table.clone(),
        }
    }

    pub fn restore_snapshot(&mut self, snapshot: ContextSnapshot) {
        self.name_handler = snapshot.name_handler;
        self.type_symbol_table = snapshot.type_symbol_table;
    }
}

#[derive(Default, Clone)]
pub struct NameHandler {
    var_counter: i32,
    struct_counter: i32,
    function_counter: i32,
}

impl NameHandler {
    fn create_var_name(&mut self) -> String {
        self.var_counter += 1;
        format!("var_{}", self.var_counter)
    }

    fn create_struct_name(&mut self) -> String {
        self.struct_counter += 1;
        format!("Struct{}", self.struct_counter)
    }

    fn create_field_name(index: usize) -> String {
        format!("field_{}", index)
    }

    fn create_function_name(&mut self) -> String {
        self.function_counter += 1;
        format!("function_{}", self.function_counter)
    }

    fn create_lifetime_name(&mut self, index: usize) -> String {
        let possible_values = b'a'..=b'z';
        let possible_value_len = possible_values.len();
        (possible_values.collect::<Vec<u8>>()[index % possible_value_len] as char).to_string()
    }
}

pub struct StructContext {
    pub generate_copy_struct: bool,
    pub lifetimes: BTreeSet<Lifetime>,
}

impl StructContext {
    pub fn new(generate_copy_struct: bool) -> StructContext {
        StructContext {
            generate_copy_struct,
            lifetimes: BTreeSet::new(),
        }
    }
}
