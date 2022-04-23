use crate::ast::expr::{BinaryOp, ExprKind, IdentExpr};
use crate::ast::stmt::StmtKind;
use crate::ast::ty::Ty;
use crate::policy::Policy;
use rand::prelude::{Distribution, SliceRandom, StdRng};
use rand::{thread_rng, Rng, SeedableRng};
use std::collections::HashMap;

pub struct Context {
    pub policy: Policy,
    pub name_handler: NameHandler,
    pub type_symbol_table: TypeSymbolTable,
    pub rng: StdRng,
    pub if_else_depth: u32,
    pub block_depth: u32,
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
            block_depth: 0,
            arith_depth: 0,
        }
    }

    pub fn with_policy(seed: Option<u64>, policy: Policy) -> Context {
        let mut context = Context::new(seed);
        context.policy = policy;
        context
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

#[derive(Debug, Clone)]
pub struct TypeMapping {
    ty: Ty,
    mutable: bool,
}

#[derive(Debug, Default, Clone)]
pub struct TypeSymbolTable {
    var_type_mapping: HashMap<String, TypeMapping>,
}

// TODO: Change this to a bidirectional map
impl TypeSymbolTable {
    pub fn add_var(&mut self, key: String, ty: Ty, mutable: bool) {
        self.var_type_mapping
            .insert(key, TypeMapping { ty, mutable });
    }

    #[allow(dead_code)]
    pub fn contains(&self, key: &String) -> bool {
        self.var_type_mapping.contains_key(key)
    }

    #[allow(dead_code)]
    pub fn get_ident_expr_by_name(&self, key: &String) -> Option<IdentExpr> {
        self.var_type_mapping.get(key).map(|ty_mapping| IdentExpr {
            name: key.clone(),
            ty: ty_mapping.ty.clone(),
        })
    }

    // TODO: refactor
    pub fn get_ident_exprs_by_type(&self, ty: &Ty) -> Vec<IdentExpr> {
        self.var_type_mapping
            .iter()
            .filter(|&(_k, v)| v.ty == *ty)
            .map(|(name, ty_mapping)| IdentExpr {
                name: name.clone(),
                ty: ty_mapping.ty.clone(),
            })
            .collect()
    }

    pub fn get_mut_ident_exprs_by_type(&self, ty: &Ty) -> Vec<IdentExpr> {
        self.var_type_mapping
            .iter()
            .filter(|&(_k, v)| v.mutable && v.ty == *ty)
            .map(|(name, ty_mapping)| IdentExpr {
                name: name.clone(),
                ty: ty_mapping.ty.clone(),
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
