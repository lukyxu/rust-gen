use crate::ast::expr::{BinaryOp, ExprKind, IdentExpr};
use crate::ast::stmt::StmtKind;
use crate::ast::ty::Ty;
use crate::policy::Policy;
use rand::prelude::{Distribution, SliceRandom, StdRng, ThreadRng};
use rand::{Rng, SeedableRng, thread_rng};
use std::collections::HashMap;

pub struct Context {
    pub policy: Policy,
    pub name_handler: NameHandler,
    pub type_symbol_table: TypeSymbolTable,
    pub rng: StdRng,
    pub if_else_depth: u32,
    pub arith_depth: u32,
}

impl Context {
    pub fn new(seed: Option<u64>) -> Context {
        let rng = if let Some(seed) = seed {
            StdRng::seed_from_u64(seed)
        } else {
            StdRng::seed_from_u64(thread_rng().gen())
        };
        Context {
            policy: Default::default(),
            name_handler: Default::default(),
            type_symbol_table: Default::default(),
            rng,
            if_else_depth: 0,
            arith_depth: 0
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
        choose(&self.policy.type_dist, &mut self.rng)
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

    pub fn choose_otherwise_if_stmt(&mut self) -> bool {
        self.rng.gen_bool(self.policy.otherwise_if_stmt_prob)
    }

    pub fn choose_unsuffixed_int(&mut self) -> bool {
        self.rng.gen_bool(self.policy.unsuffixed_int_prob)
    }

    pub fn choose_boolean_true(&mut self) -> bool {
        self.rng.gen_bool(self.policy.bool_true_prob)
    }

    pub fn choose_ident_expr_by_type(&mut self, ty: &Ty) -> Option<IdentExpr> {
        let ident_exprs = self.type_symbol_table.get_ident_exprs_by_type(ty);
        if let Some(ident) = ident_exprs.choose(&mut self.rng).clone() {
            Some(ident.clone())
        } else {
            None
        }
    }
}

#[derive(Default)]
pub struct NameHandler {
    var_counter: i32,
}

impl NameHandler {
    fn create_var_name(&mut self) -> String {
        let res = format!("var_{}", self.var_counter.to_string());
        self.var_counter += 1;
        res
    }
}

#[derive(Debug, Default, Clone)]
pub struct TypeSymbolTable {
    var_type_mapping: HashMap<String, Ty>,
}

// TODO: Change this to a bidirectional map
impl TypeSymbolTable {
    pub fn add_var(&mut self, key: String, value: Ty) {
        self.var_type_mapping.insert(key, value);
    }

    pub fn contains(&self, key: &String) -> bool {
        self.var_type_mapping.contains_key(key)
    }

    pub fn get_ident_expr_by_name(&self, key: &String) -> Option<IdentExpr> {
        if let Some(ty) = self.var_type_mapping.get(key) {
            Some(IdentExpr {
                name: key.clone(),
                ty: ty.clone(),
            })
        } else {
            None
        }
    }

    pub fn get_ident_exprs_by_type(&self, ty: &Ty) -> Vec<IdentExpr> {
        self.var_type_mapping
            .iter()
            .filter(|&(_k, v)| *v == *ty)
            .map(|(name, ty)| IdentExpr {
                name: name.clone(),
                ty: ty.clone(),
            })
            .collect()
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