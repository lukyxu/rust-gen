use crate::ast::expr::{BinaryOp, ExprKind, IdentExpr};
use crate::ast::stmt::StmtKind;
use crate::ast::ty::{ArrayTy, PrimTy, TupleTy, Ty, TyKind};
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
    pub expr_depth: usize,
    pub if_else_depth: usize,
    pub block_depth: usize,
    pub arith_depth: usize,
    pub array_depth: usize,
    pub tuple_depth: usize,
    pub array_type_dist: Vec<(ArrayTy, f64)>,
    pub tuple_type_dist: Vec<(TupleTy, f64)>,
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
            expr_depth: 0,
            if_else_depth: 0,
            block_depth: 0,
            arith_depth: 0,
            array_depth: 0,
            tuple_depth: 0,
            array_type_dist: policy.default_array_type_dist.clone(),
            tuple_type_dist: policy.default_tuple_type_dist.clone(),
        }
    }
}

// TODO: Check where this is used
fn choose<T: Clone>(dist: &Vec<(T, f64)>, rng: &mut StdRng) -> Option<T> {
    if dist.is_empty() {
        return None
    };
    Some(dist.choose_weighted(rng, |item| item.1).unwrap().0.clone())
}

impl Context {
    pub fn choose_stmt_kind(&mut self) -> StmtKind {
        choose(&self.policy.stmt_dist, &mut self.rng).unwrap()
    }

    pub fn choose_prim_type(&mut self) -> Option<PrimTy> {
        choose(&self.policy.prim_type_dist, &mut self.rng)
    }

    pub fn choose_array_type(&mut self, elem_ty: Option<Ty>) -> Option<ArrayTy> {
        let dist: Vec<(ArrayTy, f64)> = if let Some(elem_ty) = elem_ty {
            self
                .array_type_dist
                .iter()
                .filter(|(array_ty, _)| {
                    &*array_ty.base_ty == &elem_ty
                })
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

    pub fn choose_tuple_type(&mut self, elem_ty: Option<Ty>) -> Option<TupleTy> {
        let dist: Vec<(TupleTy, f64)> = if let Some(elem_ty) = elem_ty {
            self
                .tuple_type_dist
                .iter()
                .filter(|(tuple_ty, _)| {
                    tuple_ty.tuple.contains(&elem_ty)
                })
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

    pub fn choose_num_stmts(&mut self) -> usize {
        self.policy.num_stmt_dist.sample(&mut self.rng)
    }

    pub fn create_var_name(&mut self) -> String {
        self.name_handler.create_var_name()
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

    pub fn choose_unsuffixed_int(&mut self) -> bool {
        self.rng.gen_bool(self.policy.unsuffixed_int_prob)
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
}

impl NameHandler {
    fn create_var_name(&mut self) -> String {
        let res = format!("var_{}", self.var_counter);
        self.var_counter += 1;
        res
    }
}

// impl SymbolTable {
//     fn add_var(&mut self, key: String, value: Variable) {
//         self.var_mapping.insert(key, value);
//     }
//
//     fn contains(&self, key: &String) -> bool {
//         self.var_mapping.contains_key(key)
//     }
// }
//
// #[derive(Debug, Clone)]
// pub struct Variable {
//     name: String,
//     ty: Ty,
//     initial_value: LitExpr,
//     current_value: LitExpr
// }
