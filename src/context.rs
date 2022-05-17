use crate::ast::expr::{ExprKind, IdentExpr};
use crate::ast::item::ItemKind;
use crate::ast::op::BinaryOp;
use crate::ast::stmt::StmtKind;
use crate::ast::ty::{ArrayTy, PrimTy, StructTy, TupleTy, Ty, TyKind};
use crate::policy::Policy;
use crate::statistics::Statistics;
use crate::symbol_table::ty::TypeSymbolTable;
use rand::prelude::{Distribution, SliceRandom, StdRng};
use rand::{thread_rng, Rng, SeedableRng};

pub struct Context {
    pub policy: Policy,
    pub name_handler: NameHandler,
    pub type_symbol_table: TypeSymbolTable,
    pub statistics: Statistics,
    pub rng: StdRng,
    pub gen_new_array_types: bool,
    pub gen_new_tuple_types: bool,
    pub gen_new_struct_types: bool,
    pub expr_depth: usize,
    pub if_else_depth: usize,
    pub block_depth: usize,
    pub arith_depth: usize,

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
            statistics: Statistics::default(),
            rng,
            gen_new_array_types: true,
            gen_new_tuple_types: true,
            gen_new_struct_types: true,
            expr_depth: 0,
            if_else_depth: 0,
            block_depth: 0,
            arith_depth: 0,

            array_type_dist: policy.default_array_type_dist.clone(),
            tuple_type_dist: policy.default_tuple_type_dist.clone(),
            struct_type_dist: policy.default_struct_type_dist.clone(),
        }
    }
}

// TODO: Check where this is used
fn choose<T: Clone>(dist: &Vec<(T, f64)>, rng: &mut StdRng) -> Option<T> {
    if dist.is_empty() {
        return None;
    };
    Some(dist.choose_weighted(rng, |item| item.1).unwrap().0.clone())
}

impl Context {
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
        let dist: Vec<(ArrayTy, f64)> = if let Some(elem_ty) = elem_ty {
            self.array_type_dist
                .iter()
                .filter(|(array_ty, _)| &*array_ty.base_ty == elem_ty)
                .cloned()
                .collect()
        } else {
            self.array_type_dist.clone()
        };
        choose(&dist, &mut self.rng)
    }

    pub fn choose_array_length(&mut self) -> usize {
        self.policy.array_length_dist.sample(&mut self.rng)
    }

    pub fn choose_tuple_type(&mut self, elem_ty: &Option<Ty>) -> Option<TupleTy> {
        let dist: Vec<(TupleTy, f64)> = if let Some(elem_ty) = elem_ty {
            self.tuple_type_dist
                .iter()
                .filter(|(tuple_ty, _)| tuple_ty.tuple.contains(elem_ty))
                .cloned()
                .collect()
        } else {
            self.tuple_type_dist.clone()
        };
        choose(&dist, &mut self.rng)
    }

    pub fn choose_tuple_length(&mut self) -> usize {
        self.policy.tuple_length_dist.sample(&mut self.rng)
    }

    pub fn choose_struct_type(&mut self, elem_ty: &Option<Ty>) -> Option<StructTy> {
        let dist: Vec<(StructTy, f64)> = if let Some(elem_ty) = elem_ty {
            self.struct_type_dist
                .iter()
                .filter(|(struct_ty, _)| match struct_ty {
                    StructTy::Field(field) => field
                        .fields
                        .iter()
                        .any(|field_def| &*field_def.ty == elem_ty),
                    StructTy::Tuple(tuple) => (&tuple.fields).into_iter().any(|ty| ty == elem_ty),
                })
                .cloned()
                .collect()
        } else {
            self.struct_type_dist.clone()
        };
        choose(&dist, &mut self.rng)
    }

    pub fn choose_struct_length(&mut self) -> usize {
        self.policy.struct_length_dist.sample(&mut self.rng)
    }

    pub fn choose_field_struct(&mut self) -> bool {
        self.rng.gen_bool(self.policy.field_struct_prob)
    }

    pub fn choose_base_expr_kind(&mut self) -> TyKind {
        choose(&self.policy.type_dist, &mut self.rng).unwrap()
    }

    pub fn choose_expr_kind(&mut self) -> ExprKind {
        choose(&self.policy.expr_dist, &mut self.rng).unwrap()
    }

    pub fn choose_binary_int_op(&mut self) -> BinaryOp {
        choose(&self.policy.binary_int_op_dist, &mut self.rng).unwrap()
    }

    pub fn choose_binary_bool_op(&mut self) -> BinaryOp {
        choose(&self.policy.binary_bool_op_dist, &mut self.rng).unwrap()
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

    pub fn choose_ident_expr_by_type(&mut self, ty: &Ty) -> Option<IdentExpr> {
        let ident_exprs = self.type_symbol_table.get_ident_exprs_by_type(ty);
        ident_exprs.choose(&mut self.rng).cloned()
    }
}

#[derive(Default)]
pub struct NameHandler {
    var_counter: i32,
    struct_counter: i32,
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
}
