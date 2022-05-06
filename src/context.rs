use crate::ast::expr::{BinaryOp, ExprKind, IdentExpr};
use crate::ast::stmt::StmtKind;
use crate::ast::ty::Ty;
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
    pub disable_new_array_gen: bool,
    pub disable_new_tuple_gen: bool,
    pub if_else_depth: usize,
    pub block_depth: usize,
    pub arith_depth: usize,
    pub array_depth: usize,
    pub tuple_depth: usize,
    pub array_type_dist: Vec<(Ty, f64)>,
    pub tuple_type_dist: Vec<(Ty, f64)>,
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
            disable_new_array_gen: false,
            disable_new_tuple_gen: false,
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

fn choose<T: Clone>(dist: &Vec<(T, f64)>, rng: &mut StdRng) -> T {
    dist.choose_weighted(rng, |item| item.1).unwrap().0.clone()
}

impl Context {
    pub fn choose_stmt_kind(&mut self) -> StmtKind {
        choose(&self.policy.stmt_dist, &mut self.rng)
    }

    pub fn choose_type(&mut self) -> Ty {
        let base_expr = self.choose_base_expr_kind();
        match base_expr {
            ExprKind::Literal => self.choose_prim_type(),
            ExprKind::Array => self.choose_array_type(),
            ExprKind::Tuple => self.choose_tuple_type(),
            _ => unreachable!(),
        }
    }

    pub fn choose_prim_type(&mut self) -> Ty {
        choose(&self.policy.prim_type_dist, &mut self.rng)
    }

    pub fn choose_array_type(&mut self) -> Ty {
        if self.choose_new_array_type() {
            if let Some(ty) = self.add_new_array_type() {
                return ty;
            }
        }
        choose(&self.array_type_dist, &mut self.rng)
    }

    pub fn choose_array_type_with_elem_type(&mut self, ty: &Ty) -> Ty {
        let filtered_dist: Vec<(Ty, f64)> = self
            .array_type_dist
            .iter()
            .filter(|(t, _)| {
                if let Ty::Array(t, _) = t {
                    &**t == ty
                } else {
                    panic!()
                }
            })
            .cloned()
            .collect();
        if !self.choose_new_array_type() && !filtered_dist.is_empty() {
            return choose(&filtered_dist, &mut self.rng);
        }
        if let Some(ty) = self.add_new_array_type_with_type(ty) {
            return ty;
        }
        let len = self.choose_array_length();
        let base_type = Box::new(ty.clone());
        Ty::Array(base_type, len)
    }

    pub fn choose_array_length(&mut self) -> usize {
        self.policy.array_length_dist.sample(&mut self.rng)
    }

    pub fn add_new_array_type(&mut self) -> Option<Ty> {
        self.disable_new_array_gen = true;
        let len = self.choose_array_length();
        for _ in 0..self.policy.max_expr_attempts {
            let base_type = Box::new(self.choose_type());
            if base_type.array_depth() + 1 > self.policy.max_array_depth {
                continue;
            }
            let array_type = Ty::Array(base_type, len);
            if self.array_type_dist.iter().any(|(t, _)| t == &array_type) {
                continue;
            }
            let weight = 1.0;
            self.array_type_dist.push((array_type.clone(), weight));
            self.disable_new_array_gen = false;
            return Some(array_type);
        }
        None
    }

    pub fn add_new_array_type_with_type(&mut self, ty: &Ty) -> Option<Ty> {
        let len = self.choose_array_length();
        let base_type = Box::new(ty.clone());
        let array_type = Ty::Array(base_type, len);
        if self.array_type_dist.iter().any(|(t, _)| t == &array_type) {
            return None;
        }
        let weight = 1.0;
        self.array_type_dist.push((array_type.clone(), weight));
        Some(array_type)
    }

    pub fn choose_tuple_type(&mut self) -> Ty {
        if self.choose_new_tuple_type() {
            if let Some(ty) = self.add_new_tuple_type() {
                return ty;
            }
        }
        choose(&self.tuple_type_dist, &mut self.rng)
    }

    pub fn choose_tuple_type_with_elem_type(&mut self, ty: &Ty) -> Ty {
        let filtered_dist: Vec<(Ty, f64)> = self
            .tuple_type_dist
            .iter()
            .filter(|(t, _)| {
                if let Ty::Tuple(tys) = t {
                    tys.contains(ty)
                } else {
                    panic!()
                }
            })
            .cloned()
            .collect();
        if !self.choose_new_tuple_type() && !filtered_dist.is_empty() {
            return choose(&filtered_dist, &mut self.rng);
        }
        if let Some(ty) = self.add_new_tuple_type_with_type(ty) {
            return ty;
        }
        Ty::Tuple(vec![ty.clone(), ty.clone()])
    }

    pub fn choose_tuple_length(&mut self) -> usize {
        self.policy.tuple_length_dist.sample(&mut self.rng)
    }

    pub fn new_tuple_type(&mut self) -> Option<Ty> {
        self.disable_new_tuple_gen = true;
        let len = self.choose_tuple_length();
        for _ in 0..self.policy.max_expr_attempts {
            let mut types: Vec<Ty> = vec![];
            for _ in 0..len {
                for _ in 0..self.policy.max_expr_attempts {
                    let base_type = self.choose_type();
                    if base_type.tuple_depth() + 1 > self.policy.max_tuple_depth {
                        continue;
                    }
                    types.push(base_type);
                    break;
                }
            }
            let tuple_type = Ty::Tuple(types);
            if self.tuple_type_dist.iter().any(|(t, _)| t == &tuple_type) {
                continue;
            }
            self.disable_new_tuple_gen = false;
            return Some(tuple_type);
        }
        None
    }

    pub fn add_new_tuple_type(&mut self) -> Option<Ty> {
        let tuple_type = self.new_tuple_type();
        if let Some(tuple_type) = &tuple_type {
            let weight = 1.0;
            self.tuple_type_dist.push((tuple_type.clone(), weight));
        };
        tuple_type
    }

    pub fn add_new_tuple_type_with_type(&mut self, ty: &Ty) -> Option<Ty> {
        let mut tuple_type = self.new_tuple_type();
        if let Some(types) = &mut tuple_type {
            let tys = if let Ty::Tuple(tys) = types {
                tys
            } else {
                panic!()
            };
            let index = self.rng.gen_range(0..tys.len());
            tys[index] = ty.clone();
            let weight = 1.0;
            self.tuple_type_dist.push((Ty::Tuple(tys.clone()), weight));
        };
        tuple_type
    }

    pub fn choose_base_expr_kind(&mut self) -> ExprKind {
        let base_expr_dist: Vec<(ExprKind, f64)> = self
            .policy
            .expr_dist
            .iter()
            .filter(|(expr_kind, _)| {
                expr_kind.is_base_expr()
                    && !((matches!(expr_kind, ExprKind::Array) && self.disable_new_array_gen)
                        || (matches!(expr_kind, ExprKind::Tuple) && self.disable_new_tuple_gen))
            })
            .cloned()
            .collect();
        choose(&base_expr_dist, &mut self.rng)
    }

    pub fn choose_expr_kind(&mut self) -> ExprKind {
        choose(&self.policy.expr_dist, &mut self.rng)
    }

    pub fn choose_binary_int_op(&mut self) -> BinaryOp {
        choose(&self.policy.binary_int_op_dist, &mut self.rng)
    }

    pub fn choose_binary_bool_op(&mut self) -> BinaryOp {
        choose(&self.policy.binary_bool_op_dist, &mut self.rng)
    }

    pub fn choose_num_stmts(&mut self) -> usize {
        self.policy.num_stmt_dist.sample(&mut self.rng)
    }

    pub fn create_var_name(&mut self) -> String {
        self.name_handler.create_var_name()
    }

    pub fn choose_new_array_type(&mut self) -> bool {
        self.rng.gen_bool(self.policy.new_array_prob)
    }

    pub fn choose_new_tuple_type(&mut self) -> bool {
        self.rng.gen_bool(self.policy.new_tuple_prob)
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
