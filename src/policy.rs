use crate::ast::expr::ExprKind;
use crate::ast::item::ItemKind;
use crate::ast::op::BinaryOp;
use crate::ast::stmt::StmtKind;
use crate::ast::ty::{ArrayTy, IntTy, PrimTy, StructTy, TupleTy, TyKind, UIntTy};
use rand::distributions::Uniform;

#[derive(Debug, Clone)]
pub struct Policy {
    pub name: &'static str,
    pub num_item_dist: Uniform<usize>,
    pub item_dist: Vec<(ItemKind, f64)>,
    pub num_stmt_dist: Uniform<usize>,
    pub stmt_dist: Vec<(StmtKind, f64)>,
    pub expr_dist: Vec<(ExprKind, f64)>,
    pub type_dist: Vec<(TyKind, f64)>,

    pub prim_type_dist: Vec<(PrimTy, f64)>,
    pub default_array_type_dist: Vec<(ArrayTy, f64)>,
    pub default_tuple_type_dist: Vec<(TupleTy, f64)>,
    pub default_struct_type_dist: Vec<(StructTy, f64)>,

    pub new_array_prob: f64,
    pub array_length_dist: Uniform<usize>,
    pub max_array_depth: usize,
    pub max_expr_depth_in_array: usize,

    pub new_tuple_prob: f64,
    pub tuple_length_dist: Uniform<usize>,
    pub max_tuple_depth: usize,
    pub max_expr_depth_in_tuple: usize,

    pub field_struct_prob: f64,
    pub struct_length_dist: Uniform<usize>,
    pub max_struct_depth: usize,
    pub max_expr_depth_in_struct: usize,

    pub binary_int_op_dist: Vec<(BinaryOp, f64)>,
    pub binary_bool_op_dist: Vec<(BinaryOp, f64)>,
    // TODO: See if we can make this distribution generalized
    pub otherwise_if_stmt_prob: f64,
    pub bool_true_prob: f64,
    pub mutability_prob: f64,

    pub max_if_else_depth: usize,
    pub max_block_depth: usize,
    pub max_arith_depth: usize,
    pub max_expr_depth: usize,

    pub max_item_attempts: usize,
    pub max_stmt_attempts: usize,
    pub max_expr_attempts: usize,
    pub max_ty_attempts: usize,
}

// vec![
//     Policy::stress_test(),
//     Policy::mutability_debug(),
//     Policy::tuple_debug(),
//     Policy::tuple_field_debug(),
//     Policy::simple_debug(),
//     Policy::simple_debug_with_assignments(),
//     Policy::unary_debug(),
//     Policy::array_debug(),
//     Policy::array_index_debug(),
//     Policy::default(),
// ]

#[allow(dead_code)]
impl Policy {
    pub fn get_policies() -> Vec<Policy> {
        vec![
            Policy::tuple_debug(),
            Policy::tuple_field_debug(),
            Policy::simple_debug(),
            Policy::simple_debug_with_assignments(),
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
            .find(|p| p.name == name)
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

    pub fn tuple_debug() -> Self {
        let policy = Policy::default_with_name("tuple_debug");
        Policy {
            prim_type_dist: vec![(IntTy::I8.into(), 3.0)],
            new_tuple_prob: 1.0,
            default_tuple_type_dist: vec![(
                TupleTy {
                    tuple: vec![IntTy::I8.into(), IntTy::I8.into()],
                },
                1.0,
            )],
            tuple_length_dist: Uniform::new_inclusive(3, 4),
            max_tuple_depth: 1,
            expr_dist: vec![
                (ExprKind::Literal, 5.0),
                (ExprKind::Binary, 1.0),
                (ExprKind::Unary, 1.0),
                (ExprKind::Ident, 2.0),
                (ExprKind::If, 1.0),
            ],

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
            prim_type_dist: vec![(IntTy::I8.into(), 1.0)],
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

    pub fn array_debug() -> Self {
        let policy = Policy::default_with_name("array_debug");
        Policy {
            stmt_dist: vec![(StmtKind::Local, 1.0), (StmtKind::Semi, 1.0)],
            prim_type_dist: vec![(IntTy::I8.into(), 1.0)],
            new_array_prob: 0.5,
            default_array_type_dist: vec![(
                ArrayTy {
                    base_ty: Box::new(IntTy::I8.into()),
                    len: 3,
                },
                0.5,
            )],
            array_length_dist: Uniform::new_inclusive(3, 4),
            max_array_depth: 3,

            mutability_prob: 0.2,
            expr_dist: vec![
                (ExprKind::Literal, 5.0),
                (ExprKind::Binary, 1.0),
                (ExprKind::Unary, 5.0),
                (ExprKind::Ident, 2.0),
                (ExprKind::If, 1.0),
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
        policy.expr_dist.push((ExprKind::Index, 1.0));
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
            num_item_dist: Uniform::new_inclusive(2, 10),
            item_dist: vec![(ItemKind::Struct, 1.0)],

            num_stmt_dist: Uniform::new_inclusive(2, 10),
            stmt_dist: vec![
                (StmtKind::Local, 5.0),
                (StmtKind::Semi, 1.0),
                // (StmtKind::Expr, 0.0): Must be 0
            ],
            expr_dist: vec![
                (ExprKind::Literal, 5.0),
                (ExprKind::If, 1.0),
                (ExprKind::Binary, 2.0),
                (ExprKind::Ident, 2.0),
                (ExprKind::Block, 0.0),
                (ExprKind::Unary, 1.0),
                (ExprKind::Cast, 1.0),
                (ExprKind::Index, 1.0),
                (ExprKind::Field, 1.0),
            ],
            type_dist: vec![
                (TyKind::Unit, 1.0),
                (TyKind::Prim, 2.0),
                (TyKind::Array, 0.2),
                (TyKind::Tuple, 0.2),
                (TyKind::Struct, 0.5),
            ],

            prim_type_dist: vec![
                (PrimTy::Bool, 3.0),
                (IntTy::I8.into(), 3.0),
                (IntTy::I16.into(), 3.0),
                (IntTy::I32.into(), 3.0),
                (IntTy::I64.into(), 1.0),
                (IntTy::I128.into(), 1.0),
                (IntTy::ISize.into(), 1.0),
                (UIntTy::U8.into(), 3.0),
                (UIntTy::U16.into(), 3.0),
                (UIntTy::U32.into(), 3.0),
                (UIntTy::U64.into(), 1.0),
                (UIntTy::U128.into(), 1.0),
                (UIntTy::USize.into(), 1.0),
            ],

            new_array_prob: 0.5,
            default_array_type_dist: vec![],
            array_length_dist: Uniform::new_inclusive(2, 3),
            max_array_depth: 2,
            max_expr_depth_in_array: 5,

            new_tuple_prob: 0.5,
            default_tuple_type_dist: vec![],
            tuple_length_dist: Uniform::new_inclusive(2, 3),
            max_tuple_depth: 2,
            max_expr_depth_in_tuple: 5,

            field_struct_prob: 0.5,
            default_struct_type_dist: vec![],
            struct_length_dist: Uniform::new_inclusive(2, 3),
            max_struct_depth: 5,
            max_expr_depth_in_struct: 5,

            binary_int_op_dist: vec![
                (BinaryOp::Add, 1.0),
                (BinaryOp::Sub, 1.0),
                (BinaryOp::Mul, 1.0),
                (BinaryOp::Div, 1.0),
                (BinaryOp::Rem, 1.0),
            ],
            binary_bool_op_dist: vec![
                (BinaryOp::And, 1.0),
                (BinaryOp::Or, 1.0),
                (BinaryOp::Eq, 1.0),
                (BinaryOp::Ne, 1.0),
            ],

            otherwise_if_stmt_prob: 0.5,
            bool_true_prob: 0.5,
            mutability_prob: 0.5,

            max_expr_depth: 100,
            max_if_else_depth: 2,
            // max_block_depth: 3 + max_if_else_depth,
            max_block_depth: 2,
            max_arith_depth: 5,

            max_item_attempts: 1,
            max_stmt_attempts: 1,
            max_expr_attempts: 100,
            max_ty_attempts: 5,
        }
    }
}
