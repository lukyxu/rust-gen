use crate::ast::expr::ExprKind;
use crate::ast::item::ItemKind;
use crate::ast::op::{BinaryOp, UnaryOp};
use crate::ast::stmt::StmtKind;
use crate::ast::ty::{ArrayTy, IntTy, PrimTy, StructTy, TupleTy, TyKind, UIntTy};
use crate::distribution::Distribution;
use derive_builder::Builder;
use rand::prelude::SliceRandom;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Builder, Serialize, Deserialize)]
#[builder(custom_constructor, build_fn(private, name = "fallible_build"))]
pub struct Policy {
    /// Unique identifier for policy.
    pub name: String,

    // Max generation attempts
    /// Max attempts for fuzzing a file.
    pub max_file_attempts: usize,
    /// Max attempts for fuzzing an item.
    pub max_item_attempts: usize,
    /// Max attempts for fuzzing a function.
    pub max_fn_attempts: usize,
    /// Max attempts for fuzzing a type.
    pub max_ty_attempts: usize,
    /// Max attempts for fuzzing a statement.
    pub max_stmt_attempts: usize,
    /// Max attempts for fuzzing an expression.
    pub max_expr_attempts: usize,

    // Items
    /// Distribution of the number of items in a file.
    pub num_item_dist: Distribution,
    /// Distribution of item kinds.
    pub item_dist: Vec<(ItemKind, f64)>,

    // Types
    /// Distribution of type kinds.
    pub type_dist: Vec<(TyKind, f64)>,
    /// Distribution of primitive types.
    pub prim_type_dist: Vec<(PrimTy, f64)>,

    // Stmt distribution
    /// Distribution of the number of statements in a file.
    pub num_stmt_dist: Distribution,
    /// Distribution of statement kinds.
    pub stmt_dist: Vec<(StmtKind, f64)>,
    /// Probability of an local let statement binding a mutable variable.
    pub mutability_prob: f64,

    // Expression distribution
    /// Distribution of expression kinds.
    pub expr_dist: Vec<(ExprKind, f64)>,
    /// Probability of the base boolean primitive generating the value true.
    pub bool_true_prob: f64,
    /// Probability of an if expression, which evaluates to the unit type, of having a else branch.
    /// If expression which evaluate to a value which is not the unit type are excluded as they must have an else expression of the same type.
    pub otherwise_if_stmt_prob: f64,
    /// Maximum nested depth of an if/else expression.
    pub max_if_else_depth: usize,
    /// Maximum nested depth of a block expression.
    pub max_block_depth: usize,
    /// Maximum nested depth of a arithmetic expression.
    pub max_arith_depth: usize,
    /// Maximum nested depth of a expression.
    pub max_expr_depth: usize,
    /// Maximum nested array/struct/tuple depth of a array/struct/tuple.
    pub max_composite_depth: usize,

    // Array distribution
    /// Distribution of the number of elements in a generated array.
    pub array_length_dist: Distribution,
    /// Distribution of the array types which are available at the start of the program.
    pub default_array_type_dist: Vec<(ArrayTy, f64)>,
    /// Probability of generating a new array type.
    pub new_array_prob: f64,
    /// Maximum nested depth of an array.
    pub max_array_depth: usize,
    /// Maximum expression depth within an array.
    pub max_expr_depth_in_array: usize,

    // Tuple distribution
    /// Distribution of the number of elements in a generated tuple.
    pub tuple_length_dist: Distribution,
    /// Distribution of the tuple types which are available at the start of the program.
    pub default_tuple_type_dist: Vec<(TupleTy, f64)>,
    /// Probability of generating a new tuple type.
    pub new_tuple_prob: f64,
    /// Maximum nested depth of an tuple.
    pub max_tuple_depth: usize,
    /// Maximum expression depth within a tuple.
    pub max_expr_depth_in_tuple: usize,

    // Struct distribution
    /// Distribution of the number of fields in a generated struct.
    pub struct_length_dist: Distribution,
    /// Distribution of the struct types which are available at the start of the program.
    pub default_struct_type_dist: Vec<(StructTy, f64)>,
    /// Probability of a newly generated struct type being a field struct.
    pub field_struct_prob: f64,
    /// Probability of a newly generated field struct type, having the copy trait.
    pub field_struct_copy_prob: f64,
    /// Probability of a newly generated tuple struct type, having the copy trait.
    pub tuple_struct_copy_prob: f64,
    /// Maximum nested depth of a struct.
    pub max_struct_depth: usize,
    /// Maximum expression depth within a struct.
    pub max_expr_depth_in_struct: usize,

    // Operation distribution
    /// Distribution of binary operators.
    pub binary_op_dist: Vec<(BinaryOp, f64)>,
    /// Distribution of unary operators.
    pub unary_op_dist: Vec<(UnaryOp, f64)>,

    // Lifetimes
    /// Probability of generating a new lifetime.
    pub new_lifetime_prob: f64,
    /// Option to disable lifetime generation.
    pub disable_lifetime: bool,
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

impl Policy {
    pub fn get_policies() -> Vec<Policy> {
        vec![
            Policy::default(),
            Policy::simple_arithmetics(),
            Policy::arithmetics(),
            Policy::arithmetic_with_control_flow(),
            Policy::assignments(),
            Policy::tuples(),
            Policy::arrays(),
            Policy::structs(),
            Policy::composite(),
            Policy::functions(),
            // Policy::lifetime()
            Policy::ownership_transfer(),
        ]
    }

    pub fn get_policy_names() -> Vec<String> {
        Policy::get_policies()
            .iter()
            .map(|p| p.name.clone())
            .collect::<Vec<String>>()
    }

    pub fn get_policy(name: &str) -> Option<Policy> {
        Policy::get_policies()
            .iter()
            .find(|p| p.name == name)
            .cloned()
    }

    pub fn parse_policy_args_or_random(policy: &Option<String>) -> Policy {
        match policy {
            None => Policy::get_policies()
                .choose(&mut rand::thread_rng())
                .cloned()
                .unwrap(),
            Some(_) => Policy::parse_policy_args(policy),
        }
    }

    pub fn parse_policy_args(policy: &Option<String>) -> Policy {
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
                TupleTy::new(vec![IntTy::I8.into(), IntTy::I8.into()]),
                1.0,
            )],
            tuple_length_dist: Distribution::new_uniform_inclusive(2, 3),
            max_tuple_depth: 2,

            expr_dist: vec![
                (ExprKind::Literal, 5.0),
                (ExprKind::Binary, 1.0),
                (ExprKind::Unary, 1.0),
                (ExprKind::Ident, 2.0),
                (ExprKind::If, 1.0),
            ],

            max_if_else_depth: 2,
            max_block_depth: 1,
            max_arith_depth: 2,
            max_expr_depth_in_tuple: 5,
            ..policy
        }
    }

    pub fn tuple_field_debug() -> Self {
        let mut policy = Policy::tuple_debug();
        policy.name = "tuple_field_debug".to_string();
        policy.expr_dist.push((ExprKind::Field, 0.5));
        policy
    }

    pub fn debug() -> Self {
        Policy {
            name: "debug".to_string(),
            binary_op_dist: vec![
                (BinaryOp::Eq, 1.0),
                (BinaryOp::Ne, 1.0),
                (BinaryOp::Lq, 1.0),
                (BinaryOp::Le, 1.0),
                (BinaryOp::Ge, 1.0),
                (BinaryOp::Gt, 1.0),
            ],
            num_item_dist: Distribution::Uniform(0, 0),
            num_stmt_dist: Distribution::new_uniform_inclusive(2, 2),
            max_if_else_depth: 1,
            max_block_depth: 1,
            ..Policy::simple_debug()
        }
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
                (ExprKind::Unary, 2.0),
            ],

            max_if_else_depth: 1,
            max_block_depth: 3,
            max_arith_depth: 1,

            num_stmt_dist: Distribution::new_uniform_inclusive(2, 3),
            ..policy
        }
    }

    pub fn simple_debug_with_assignments() -> Self {
        let mut policy = Policy::simple_debug();
        policy.name = "simple_debug_with_assignments".to_string();
        policy.expr_dist.push((ExprKind::Assign, 4.0));
        policy.mutability_prob = 1.0;
        policy.max_if_else_depth = 2;
        policy.max_block_depth = 0;
        policy.max_arith_depth = 2;
        policy.num_stmt_dist = Distribution::new_uniform_inclusive(2, 5);
        policy
    }

    pub fn simple_debug_with_reference() -> Self {
        let mut policy = Policy::simple_debug();
        policy.name = "simple_debug_with_reference".to_string();
        policy.max_arith_depth = 1;
        policy.type_dist = vec![
            (TyKind::Unit, 1.0),
            (TyKind::Prim, 2.0),
            (TyKind::Array, 0.5),
            (TyKind::Tuple, 0.5),
            // (TyKind::Struct, 0.5),
        ];
        policy.binary_op_dist = vec![
            (BinaryOp::Add, 1.0),
            (BinaryOp::Sub, 1.0),
            (BinaryOp::Mul, 1.0),
            (BinaryOp::Div, 1.0),
            (BinaryOp::Rem, 1.0),
            (BinaryOp::And, 1.0),
            (BinaryOp::Or, 1.0),
            (BinaryOp::Eq, 1.0),
            (BinaryOp::Ne, 1.0),
            (BinaryOp::Lq, 1.0),
            (BinaryOp::Le, 1.0),
            (BinaryOp::Ge, 1.0),
            (BinaryOp::Gt, 1.0),
        ];
        policy.item_dist = vec![(ItemKind::Struct, 1.0)];
        // policy.type_dist.push((TyKind::Reference, 3.0));
        policy.num_item_dist = Distribution::new_uniform_inclusive(2, 8);
        policy
    }

    pub fn array_debug() -> Self {
        let policy = Policy::default_with_name("array_debug");
        Policy {
            stmt_dist: vec![(StmtKind::Local, 1.0), (StmtKind::Semi, 1.0)],
            prim_type_dist: vec![(IntTy::I8.into(), 1.0)],
            new_array_prob: 0.5,
            default_array_type_dist: vec![(ArrayTy::new(IntTy::I8.into(), 3), 0.5)],
            array_length_dist: Distribution::new_uniform_inclusive(3, 4),
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

            num_stmt_dist: Distribution::new_uniform_inclusive(2, 10),
            ..policy
        }
    }

    pub fn array_index_debug() -> Self {
        let mut policy = Policy::array_debug();
        policy.name = "array_index_debug".to_string();
        policy.expr_dist.push((ExprKind::Index, 0.5));
        policy
    }

    pub fn fields_stress_test() -> Self {
        Policy {
            expr_dist: vec![
                (ExprKind::Literal, 5.0),
                (ExprKind::If, 1.0),
                (ExprKind::Ident, 2.0),
                (ExprKind::Binary, 1.0),
                (ExprKind::Assign, 5.0),
                (ExprKind::Index, 1.0),
                (ExprKind::Field, 1.0),
            ],
            ..Policy::default_with_name("fields_stress_test")
        }
    }

    pub fn my_debug() -> Self {
        let mut policy = Policy::simple_debug();
        policy.name = "my_debug".to_string();
        policy.max_arith_depth = 3;
        policy.max_if_else_depth = 1;
        policy.type_dist = vec![
            (TyKind::Unit, 1.0),
            (TyKind::Prim, 2.0),
            (TyKind::Array, 0.5),
            (TyKind::Tuple, 0.5),
            (TyKind::Struct, 0.5),
        ];
        // policy.binary_op_dist = vec![
        //     (BinaryOp::Add, 1.0),
        //     (BinaryOp::Sub, 1.0),
        //     (BinaryOp::Mul, 1.0),
        //     (BinaryOp::Div, 1.0),
        //     (BinaryOp::Rem, 1.0),
        //     (BinaryOp::And, 1.0),
        //     (BinaryOp::Or, 1.0),
        //     (BinaryOp::Eq, 1.0),
        //     (BinaryOp::Ne, 1.0),
        //     (BinaryOp::Lq, 1.0),
        //     (BinaryOp::Le, 1.0),
        //     (BinaryOp::Ge, 1.0),
        //     (BinaryOp::Gt, 1.0),
        // ];
        policy.expr_dist = vec![
            (ExprKind::Literal, 3.0),
            (ExprKind::If, 2.0),
            (ExprKind::Binary, 2.0),
            (ExprKind::Ident, 2.0),
            (ExprKind::Unary, 2.0),
            (ExprKind::Index, 1.0),
        ];
        policy.unary_op_dist = vec![(UnaryOp::Not, 1.0), (UnaryOp::Neg, 1.0)];
        policy.item_dist = vec![(ItemKind::Struct, 1.0)];
        // policy.type_dist.push((TyKind::Reference, 3.0));
        policy.num_item_dist = Distribution::new_uniform_inclusive(2, 8);
        policy.field_struct_copy_prob = 0.0;
        policy.tuple_struct_copy_prob = 0.0;
        policy
    }
}

impl Default for Policy {
    fn default() -> Self {
        Policy::default_with_name("default")
    }
}

impl Policy {
    pub fn simple_arithmetics() -> Policy {
        PolicyBuilder::from_policy(Policy::default())
            .name("simple_arithmetics".to_owned())
            .num_stmt_dist(Distribution::Const(100))
            .num_item_dist(Distribution::none())
            .type_dist(vec![(TyKind::Prim, 1.0)])
            .prim_type_dist(vec![(IntTy::I8.into(), 1.0), (UIntTy::U8.into(), 1.0)])
            .expr_dist(vec![
                (ExprKind::Binary, 3.0),
                (ExprKind::Ident, 1.0),
                (ExprKind::Unary, 2.0),
                (ExprKind::Literal, 1.0),
            ])
            .max_arith_depth(4)
            .max_expr_attempts(100)
            .binary_op_dist(vec![
                (BinaryOp::Add, 1.0),
                (BinaryOp::Sub, 1.0),
                (BinaryOp::Mul, 1.0),
                (BinaryOp::Div, 1.0),
                (BinaryOp::Rem, 1.0),
            ])
            .build()
            .unwrap()
    }

    pub fn arithmetics() -> Policy {
        PolicyBuilder::from_policy(Policy::simple_arithmetics())
            .name("arithmetics".to_owned())
            .num_stmt_dist(Distribution::Const(1000))
            .prim_type_dist(Policy::default().prim_type_dist)
            .binary_op_dist(Policy::default().binary_op_dist)
            .expr_dist_with(ExprKind::Binary, 5.0)
            .expr_dist_with(ExprKind::Unary, 3.0)
            .expr_dist_with(ExprKind::Cast, 1.0)
            .max_expr_depth(10)
            .max_arith_depth(5)
            .build()
            .unwrap()
    }

    pub fn arithmetic_with_control_flow() -> Policy {
        PolicyBuilder::from_policy(Policy::arithmetics())
            .name("arithmetic_with_control_flow".to_owned())
            .num_stmt_dist(Distribution::new_uniform_inclusive(2, 8))
            .expr_dist_with(ExprKind::Binary, 2.0)
            .expr_dist_with(ExprKind::Unary, 1.0)
            .expr_dist_with(ExprKind::If, 0.5)
            .expr_dist_with(ExprKind::Block, 0.5)
            .max_block_depth(2)
            .max_if_else_depth(2)
            .max_expr_depth(10)
            .build()
            .unwrap()
    }

    pub fn assignments() -> Policy {
        PolicyBuilder::from_policy(Policy::arithmetic_with_control_flow())
            .name("assignments".to_owned())
            .expr_dist_with(ExprKind::Assign, 8.0)
            .type_dist(vec![(TyKind::Prim, 2.0), (TyKind::Unit, 0.5)])
            .build()
            .unwrap()
    }

    pub fn tuples() -> Policy {
        PolicyBuilder::from_policy(Policy::assignments())
            .name("tuples".to_owned())
            .num_stmt_dist(Distribution::new_uniform_inclusive(2, 6))
            .type_dist(vec![
                (TyKind::Prim, 2.0),
                (TyKind::Tuple, 0.5),
                (TyKind::Unit, 1.0),
            ])
            .expr_dist_with(ExprKind::Field, 0.25)
            .new_tuple_prob(0.5)
            .default_tuple_type_dist(vec![(
                TupleTy::new(vec![IntTy::I8.into(), IntTy::I8.into()]),
                1.0,
            )])
            .tuple_length_dist(Distribution::new_uniform_inclusive(2, 3))
            .max_expr_depth_in_tuple(5)
            .max_tuple_depth(2)
            .max_block_depth(1)
            .build()
            .unwrap()
    }

    pub fn arrays() -> Policy {
        PolicyBuilder::from_policy(Policy::assignments())
            .name("arrays".to_owned())
            .num_stmt_dist(Distribution::new_uniform_inclusive(2, 6))
            .type_dist(vec![
                (TyKind::Prim, 2.0),
                (TyKind::Array, 0.5),
                (TyKind::Unit, 1.0),
            ])
            .expr_dist_with(ExprKind::Index, 0.25)
            .new_array_prob(0.5)
            .default_array_type_dist(vec![(ArrayTy::new(IntTy::I8.into(), 3), 0.5)])
            .array_length_dist(Distribution::new_uniform_inclusive(2, 5))
            .max_array_depth(2)
            .max_expr_depth_in_array(5)
            .max_block_depth(1)
            .build()
            .unwrap()
    }

    pub fn structs() -> Policy {
        PolicyBuilder::from_policy(Policy::assignments())
            .name("structs".to_owned())
            .num_stmt_dist(Distribution::new_uniform_inclusive(2, 6))
            .type_dist(vec![
                (TyKind::Prim, 2.0),
                (TyKind::Struct, 0.5),
                (TyKind::Unit, 1.0),
            ])
            .expr_dist_with(ExprKind::Field, 0.25)
            .item_dist(vec![(ItemKind::Struct, 1.0)])
            .num_item_dist(Distribution::Uniform(5, 15))
            .field_struct_copy_prob(0.5)
            .tuple_struct_copy_prob(0.5)
            .max_struct_depth(2)
            .max_expr_depth_in_struct(5)
            .max_block_depth(1)
            .build()
            .unwrap()
    }

    pub fn composite() -> Policy {
        PolicyBuilder::from_policy(Policy::assignments())
            .name("composite".to_owned())
            .num_stmt_dist(Distribution::new_uniform_inclusive(2, 6))
            .type_dist(vec![
                (TyKind::Prim, 2.0),
                (TyKind::Struct, 0.1),
                (TyKind::Array, 0.1),
                (TyKind::Tuple, 0.1),
                (TyKind::Unit, 1.0),
            ])
            .expr_dist_with(ExprKind::Index, 0.25)
            .expr_dist_with(ExprKind::Field, 0.5)
            .item_dist(vec![(ItemKind::Struct, 1.0)])
            .num_item_dist(Distribution::Uniform(5, 15))
            .new_tuple_prob(0.5)
            .default_tuple_type_dist(vec![(
                TupleTy::new(vec![IntTy::I8.into(), IntTy::I8.into()]),
                1.0,
            )])
            .tuple_length_dist(Distribution::new_uniform_inclusive(2, 3))
            .new_array_prob(0.5)
            .default_array_type_dist(vec![(ArrayTy::new(IntTy::I8.into(), 3), 0.5)])
            .array_length_dist(Distribution::new_uniform_inclusive(2, 5))
            .field_struct_copy_prob(0.5)
            .tuple_struct_copy_prob(0.5)
            .max_expr_depth_in_tuple(5)
            .max_expr_depth_in_array(5)
            .max_expr_depth_in_struct(5)
            .max_composite_depth(3)
            .max_tuple_depth(2)
            .max_array_depth(2)
            .max_struct_depth(2)
            .max_block_depth(1)
            .build()
            .unwrap()
    }

    pub fn ownership_transfer() -> Policy {
        PolicyBuilder::from_policy(Policy::composite())
            .name("ownership_transfer".to_owned())
            .num_item_dist(Distribution::Const(3))
            .new_array_prob(0.0)
            .new_tuple_prob(1.0)
            .tuple_struct_copy_prob(0.0)
            .field_struct_copy_prob(0.0)
            .build()
            .unwrap()
    }

    pub fn functions() -> Policy {
        PolicyBuilder::from_policy(Policy::default())
            .name("functions".to_owned())
            .expr_dist_with(ExprKind::FunctionCall, 2.0)
            .item_dist(vec![(ItemKind::Struct, 1.0), (ItemKind::Function, 1.0)])
            .build()
            .unwrap()
    }
}

impl Policy {
    fn default_with_name(name: &str) -> Self {
        Policy {
            name: name.to_string(),
            num_item_dist: Distribution::new_uniform_inclusive(2, 10),
            item_dist: vec![(ItemKind::Struct, 1.0), (ItemKind::Function, 1.0)],
            num_stmt_dist: Distribution::new_uniform_inclusive(2, 10),
            stmt_dist: vec![
                (StmtKind::Local, 5.0),
                (StmtKind::Semi, 3.0),
                // (StmtKind::Expr, 0.0): Must be 0
            ],
            expr_dist: vec![
                (ExprKind::Literal, 5.0),
                (ExprKind::If, 1.0),
                (ExprKind::Binary, 2.0),
                (ExprKind::Ident, 2.0),
                (ExprKind::Block, 2.0),
                (ExprKind::Unary, 1.0),
                (ExprKind::Cast, 1.0),
                (ExprKind::Assign, 1.0),
                (ExprKind::Index, 0.5),
                (ExprKind::Field, 0.5),
            ],
            type_dist: vec![
                (TyKind::Unit, 1.0),
                (TyKind::Prim, 2.0),
                (TyKind::Array, 0.5),
                (TyKind::Tuple, 0.5),
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
            array_length_dist: Distribution::new_uniform_inclusive(2, 3),
            max_array_depth: 2,
            max_expr_depth_in_array: 2,

            new_tuple_prob: 0.5,
            default_tuple_type_dist: vec![],
            tuple_length_dist: Distribution::new_uniform_inclusive(2, 3),
            max_tuple_depth: 2,
            max_expr_depth_in_tuple: 2,

            field_struct_prob: 0.5,
            default_struct_type_dist: vec![],
            struct_length_dist: Distribution::new_uniform_inclusive(2, 3),
            max_struct_depth: 2,
            max_expr_depth_in_struct: 2,

            binary_op_dist: vec![
                (BinaryOp::Add, 1.0),
                (BinaryOp::Sub, 1.0),
                (BinaryOp::Mul, 1.0),
                (BinaryOp::Div, 1.0),
                (BinaryOp::Rem, 1.0),
                (BinaryOp::BitXor, 1.0),
                (BinaryOp::BitAnd, 1.0),
                (BinaryOp::BitOr, 1.0),
                (BinaryOp::And, 1.0),
                (BinaryOp::Or, 1.0),
                (BinaryOp::Eq, 1.0),
                (BinaryOp::Ne, 1.0),
                (BinaryOp::Lq, 1.0),
                (BinaryOp::Le, 1.0),
                (BinaryOp::Ge, 1.0),
                (BinaryOp::Gt, 1.0),
                (BinaryOp::WrappingAdd, 1.0),
                (BinaryOp::WrappingSub, 1.0),
                (BinaryOp::WrappingMul, 1.0),
                (BinaryOp::WrappingDiv, 1.0),
                (BinaryOp::WrappingRem, 1.0),
                (BinaryOp::WrappingShl, 1.0),
                (BinaryOp::WrappingShr, 1.0),
            ],

            unary_op_dist: vec![
                // (UnaryOp::Deref, 1.0),
                (UnaryOp::Not, 1.0),
                (UnaryOp::Neg, 1.0),
            ],

            otherwise_if_stmt_prob: 0.5,
            bool_true_prob: 0.5,
            mutability_prob: 1.0,
            new_lifetime_prob: 0.5,
            disable_lifetime: true,

            max_expr_depth: 10,
            max_composite_depth: 3,
            max_if_else_depth: 1,
            max_block_depth: 1,
            max_arith_depth: 5,

            max_file_attempts: 1,
            max_fn_attempts: 1,
            max_item_attempts: 1,
            max_stmt_attempts: 30,
            max_expr_attempts: 5,
            max_ty_attempts: 10,

            tuple_struct_copy_prob: 0.5,
            field_struct_copy_prob: 0.5,
        }
    }
}

impl PolicyBuilder {
    pub fn build(&self) -> Result<Policy, PolicyBuilderError> {
        let policy = self.fallible_build().unwrap();
        if policy.max_file_attempts == 0
            || policy.max_item_attempts == 0
            || policy.max_fn_attempts == 0
            || policy.max_ty_attempts == 0
            || policy.max_stmt_attempts == 0
            || policy.max_expr_attempts == 0
        {
            return Err(PolicyBuilderError::ValidationError(
                "Max file/item/fn/ty/stmt/expr attempts must be greater than 0.".to_string(),
            ));
        };
        Ok(policy)
    }

    pub fn from_policy(policy: Policy) -> PolicyBuilder {
        PolicyBuilder {
            name: Some(policy.name),
            max_file_attempts: Some(policy.max_file_attempts),
            max_item_attempts: Some(policy.max_item_attempts),
            max_fn_attempts: Some(policy.max_fn_attempts),
            max_ty_attempts: Some(policy.max_ty_attempts),
            max_stmt_attempts: Some(policy.max_stmt_attempts),
            max_expr_attempts: Some(policy.max_expr_attempts),
            num_item_dist: Some(policy.num_item_dist),
            item_dist: Some(policy.item_dist),
            type_dist: Some(policy.type_dist),
            prim_type_dist: Some(policy.prim_type_dist),
            num_stmt_dist: Some(policy.num_stmt_dist),
            stmt_dist: Some(policy.stmt_dist),
            mutability_prob: Some(policy.mutability_prob),
            expr_dist: Some(policy.expr_dist),
            bool_true_prob: Some(policy.bool_true_prob),
            otherwise_if_stmt_prob: Some(policy.otherwise_if_stmt_prob),
            max_if_else_depth: Some(policy.max_if_else_depth),
            max_block_depth: Some(policy.max_block_depth),
            max_arith_depth: Some(policy.max_arith_depth),
            max_expr_depth: Some(policy.max_expr_depth),
            max_composite_depth: Some(policy.max_composite_depth),
            array_length_dist: Some(policy.array_length_dist),
            default_array_type_dist: Some(policy.default_array_type_dist),
            new_array_prob: Some(policy.new_array_prob),
            max_array_depth: Some(policy.max_array_depth),
            max_expr_depth_in_array: Some(policy.max_expr_depth_in_array),
            tuple_length_dist: Some(policy.tuple_length_dist),
            default_tuple_type_dist: Some(policy.default_tuple_type_dist),
            new_tuple_prob: Some(policy.new_tuple_prob),
            max_tuple_depth: Some(policy.max_tuple_depth),
            max_expr_depth_in_tuple: Some(policy.max_expr_depth_in_tuple),
            struct_length_dist: Some(policy.struct_length_dist),
            default_struct_type_dist: Some(policy.default_struct_type_dist),
            field_struct_prob: Some(policy.field_struct_prob),
            field_struct_copy_prob: Some(policy.field_struct_copy_prob),
            tuple_struct_copy_prob: Some(policy.tuple_struct_copy_prob),
            max_struct_depth: Some(policy.max_struct_depth),
            max_expr_depth_in_struct: Some(policy.max_expr_depth_in_struct),
            binary_op_dist: Some(policy.binary_op_dist),
            unary_op_dist: Some(policy.unary_op_dist),
            new_lifetime_prob: Some(policy.new_lifetime_prob),
            disable_lifetime: Some(policy.disable_lifetime),
        }
    }

    pub fn no_assignments(&mut self) -> &mut Self {
        self.expr_dist
            .as_mut()
            .unwrap()
            .retain(|(kind, _)| !matches!(kind, ExprKind::Assign));
        self
    }

    pub fn expr_dist_with(&mut self, kind: ExprKind, weight: f64) -> &mut Self {
        self.expr_dist.as_mut().unwrap().retain(|(k, _)| *k != kind);
        if weight > 0.0 {
            self.expr_dist.as_mut().unwrap().push((kind, weight));
        }
        self
    }

    pub fn no_items(&mut self) -> &mut Self {
        self.num_item_dist = Some(Distribution::none());
        self
    }
}
