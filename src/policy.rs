use crate::ast::expr::{BinaryOp, ExprKind};
use crate::ast::stmt::StmtKind;
use crate::ast::ty::{IntTy, Ty, UIntTy};
use rand::distributions::Uniform;

#[derive(Debug, Clone)]
pub struct Policy {
    pub name: &'static str,
    pub num_stmt_dist: Uniform<usize>,
    pub expr_dist: Vec<(ExprKind, f64)>,
    pub stmt_dist: Vec<(StmtKind, f64)>,
    pub prim_type_dist: Vec<(Ty, f64)>,

    pub new_array_prob: f64,
    pub default_array_type_dist: Vec<(Ty, f64)>,
    pub array_length_dist: Uniform<usize>,
    pub max_array_depth: usize,

    pub new_tuple_prob: f64,
    pub default_tuple_type_dist: Vec<(Ty, f64)>,
    pub tuple_length_dist: Uniform<usize>,
    pub max_tuple_depth: usize,

    pub binary_int_op_dist: Vec<(BinaryOp, f64)>,
    pub binary_bool_op_dist: Vec<(BinaryOp, f64)>,
    // TODO: See if we can make this distribution generalized
    pub unsuffixed_int_prob: f64,
    pub otherwise_if_stmt_prob: f64,
    pub bool_true_prob: f64,
    pub mutability_prob: f64,

    pub max_if_else_depth: usize,
    pub max_block_depth: usize,
    pub max_arith_depth: usize,
    pub max_expr_attempts: usize,
}

#[allow(dead_code)]
impl Policy {
    pub fn get_policies() -> Vec<Policy> {
        vec![
            Policy::stress_test(),
            Policy::mutability_debug(),
            Policy::tuple_debug(),
            Policy::tuple_field_debug(),
            Policy::simple_debug(),
            Policy::simple_debug_with_assignments(),
            Policy::unary_debug(),
            Policy::array_debug(),
            Policy::array_index_debug(),
            Policy::default(),
        ]
    }

    pub fn get_policy_names() -> Vec<&'static str> {
        Policy::get_policies()
            .iter()
            .map(|p| p.name)
            .collect::<Vec<&str>>()
    }

    pub fn get_policy(name: &str) -> Option<Policy> {
        Policy::get_policies()
            .iter()
            .filter(|p| p.name == name)
            .next()
            .cloned()
    }

    pub fn parse_policy_args(policy: Option<String>) -> Policy {
        let policy = if let Some(policy) = policy {
            Policy::get_policy(&policy)
        } else {
            Some(Policy::default())
        };
        policy.unwrap_or_else(|| {
            eprintln!(
                "Invalid policy, choose from from {:?}",
                Policy::get_policy_names()
            );
            std::process::exit(2)
        })
    }

    pub fn stress_test() -> Self {
        let policy = Policy::default_with_name("stress_test");
        Policy {
            stmt_dist: vec![
                (StmtKind::Local, 1.0),
                (StmtKind::Semi, 1.0),
                // (StmtKind::Expr, 0.0): Must be 0
            ],
            prim_type_dist: vec![
                (Ty::Int(IntTy::I8), 3.0),
                // (Ty::Int(IntTy::I16), 1.0),
                (Ty::Tuple(vec![]), 1.0),
            ],
            mutability_prob: 0.8,
            expr_dist: vec![
                (ExprKind::Literal, 3.0),
                (ExprKind::If, 2.0),
                (ExprKind::Binary, 2.0),
                (ExprKind::Ident, 2.0),
            ],
            num_stmt_dist: Uniform::new_inclusive(2, 15),

            max_if_else_depth: 3,
            max_block_depth: 5,
            max_arith_depth: 7,
            ..policy
        }
    }

    pub fn mutability_debug() -> Self {
        let policy = Policy::default_with_name("mutability_debug");
        Policy {
            stmt_dist: vec![
                (StmtKind::Local, 1.0),
                (StmtKind::Semi, 1.0),
                // (StmtKind::Expr, 0.0): Must be 0
            ],
            prim_type_dist: vec![
                (Ty::Int(IntTy::I8), 1.0),
                // (Ty::Int(IntTy::I16), 1.0),
                (Ty::Tuple(vec![]), 2.0),
            ],
            mutability_prob: 0.8,
            expr_dist: vec![
                (ExprKind::Literal, 3.0),
                (ExprKind::If, 2.0),
                (ExprKind::Binary, 2.0),
                (ExprKind::Ident, 2.0),
                (ExprKind::Assign, 5.0),
            ],

            max_if_else_depth: 1,
            max_block_depth: 2,
            max_arith_depth: 1,
            ..policy
        }
    }

    pub fn tuple_debug() -> Self {
        let policy = Policy::default_with_name("tuple_debug");
        Policy {
            prim_type_dist: vec![(Ty::Int(IntTy::I8), 3.0)],
            new_tuple_prob: 0.1,
            default_tuple_type_dist: vec![
                (Ty::Tuple(vec![]), 3.0),
                (Ty::Tuple(vec![Ty::Int(IntTy::I8), Ty::Int(IntTy::I8)]), 1.0),
            ],
            tuple_length_dist: Uniform::new_inclusive(3, 4),
            max_tuple_depth: 3,

            max_if_else_depth: 2,
            max_block_depth: 4,
            max_arith_depth: 2,
            ..policy
        }
    }

    pub fn tuple_field_debug() -> Self {
        let mut policy = Policy::tuple_debug();
        policy.name = "tuple_field_debug";
        policy.expr_dist.push((ExprKind::Field, 0.5));
        policy
    }

    pub fn simple_debug() -> Self {
        let policy = Policy::default_with_name("simple_debug");
        Policy {
            stmt_dist: vec![(StmtKind::Local, 1.0), (StmtKind::Semi, 1.0)],
            prim_type_dist: vec![(Ty::Int(IntTy::I8), 1.0), (Ty::Tuple(vec![]), 1.0)],
            mutability_prob: 0.2,
            expr_dist: vec![
                (ExprKind::Literal, 3.0),
                (ExprKind::If, 2.0),
                (ExprKind::Binary, 2.0),
                (ExprKind::Ident, 2.0),
            ],

            max_if_else_depth: 2,
            max_block_depth: 3,
            max_arith_depth: 1,

            num_stmt_dist: Uniform::new_inclusive(2, 3),
            ..policy
        }
    }

    pub fn simple_debug_with_assignments() -> Self {
        let mut policy = Policy::simple_debug();
        policy.name = "simple_debug_with_assignments";
        policy.expr_dist.push((ExprKind::Assign, 4.0));
        policy.mutability_prob = 1.0;
        policy.max_if_else_depth = 3;
        policy.max_arith_depth = 3;
        policy.num_stmt_dist = Uniform::new_inclusive(2, 5);
        policy
    }

    pub fn unary_debug() -> Self {
        let policy = Policy::default_with_name("unary_debug");
        Policy {
            stmt_dist: vec![(StmtKind::Local, 1.0), (StmtKind::Semi, 1.0)],
            prim_type_dist: vec![
                (Ty::Int(IntTy::I8), 1.0),
                (Ty::Bool, 1.0),
                (Ty::Tuple(vec![]), 1.0),
            ],
            mutability_prob: 0.2,
            expr_dist: vec![
                (ExprKind::Literal, 3.0),
                (ExprKind::Binary, 1.0),
                (ExprKind::Unary, 5.0),
                (ExprKind::Ident, 2.0),
            ],

            max_if_else_depth: 2,
            max_block_depth: 3,
            max_arith_depth: 1,

            num_stmt_dist: Uniform::new_inclusive(2, 10),
            ..policy
        }
    }

    pub fn array_debug() -> Self {
        let policy = Policy::default_with_name("array_debug");
        Policy {
            stmt_dist: vec![(StmtKind::Local, 1.0), (StmtKind::Semi, 1.0)],
            prim_type_dist: vec![(Ty::Int(IntTy::I8), 1.0)],
            new_array_prob: 0.1,
            default_array_type_dist: vec![(Ty::Array(Box::new(Ty::Int(IntTy::I8)), 3), 0.5)],
            array_length_dist: Uniform::new_inclusive(3, 5),
            max_array_depth: 3,

            mutability_prob: 0.2,
            expr_dist: vec![
                (ExprKind::Literal, 3.0),
                (ExprKind::Binary, 1.0),
                (ExprKind::Unary, 5.0),
                (ExprKind::Ident, 2.0),
                (ExprKind::If, 1.0),
                (ExprKind::Array, 2.0),
            ],

            max_if_else_depth: 2,
            max_block_depth: 3,
            max_arith_depth: 1,

            num_stmt_dist: Uniform::new_inclusive(2, 10),
            ..policy
        }
    }

    pub fn array_index_debug() -> Self {
        let mut policy = Policy::array_debug();
        policy.name = "array_index_debug";
        policy.expr_dist.push((ExprKind::Index, 0.5));
        policy
    }
}

impl Default for Policy {
    fn default() -> Self {
        Policy::default_with_name("default")
    }
}

impl Policy {
    // TODO: Add assertions in policy after
    fn default_with_name(name: &'static str) -> Self {
        Policy {
            name,
            stmt_dist: vec![
                (StmtKind::Local, 5.0),
                (StmtKind::Semi, 1.0),
                // (StmtKind::Expr, 0.0): Must be 0
            ],
            prim_type_dist: vec![
                (Ty::Int(IntTy::I8), 3.0),
                (Ty::Int(IntTy::I16), 3.0),
                (Ty::Int(IntTy::I32), 3.0),
                (Ty::Int(IntTy::I64), 1.0),
                (Ty::Int(IntTy::ISize), 1.0),
                (Ty::UInt(UIntTy::U8), 3.0),
                (Ty::UInt(UIntTy::U16), 3.0),
                (Ty::UInt(UIntTy::U32), 3.0),
                (Ty::UInt(UIntTy::U64), 1.0),
                (Ty::UInt(UIntTy::USize), 1.0),
            ],

            new_array_prob: 0.0,
            default_array_type_dist: vec![],
            array_length_dist: Uniform::new_inclusive(3, 5),
            max_array_depth: 0,
            new_tuple_prob: 0.0,
            default_tuple_type_dist: vec![(Ty::Tuple(vec![]), 3.0)],
            tuple_length_dist: Uniform::new_inclusive(3, 5),
            max_tuple_depth: 1,
            expr_dist: vec![
                (ExprKind::Literal, 3.0),
                (ExprKind::If, 2.0),
                (ExprKind::Binary, 2.0),
                (ExprKind::Ident, 2.0),
                (ExprKind::Block, 0.0),
                (ExprKind::Unary, 1.0),
                (ExprKind::Cast, 1.0),
                (ExprKind::Tuple, 1.0),
            ],
            binary_int_op_dist: vec![
                (BinaryOp::Add, 1.0),
                (BinaryOp::Sub, 1.0),
                (BinaryOp::Mul, 1.0),
                (BinaryOp::Div, 1.0),
                (BinaryOp::Rem, 1.0),
            ],
            binary_bool_op_dist: vec![(BinaryOp::And, 1.0), (BinaryOp::Or, 1.0)],
            num_stmt_dist: Uniform::new_inclusive(2, 10),
            unsuffixed_int_prob: 0.0,
            otherwise_if_stmt_prob: 0.5,
            bool_true_prob: 0.5,
            mutability_prob: 0.5,

            max_if_else_depth: 3,
            // max_block_depth: 3 + max_if_else_depth,
            max_block_depth: 4,
            max_arith_depth: 5,

            max_expr_attempts: 100,
        }
    }
}
