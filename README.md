# Rust Gen

Rustgen is a random program that can generate correct runnable Rust programs along with the expected output of the program. It's primary purpose is to find compiler bugs either through comparing the output of running the test program with the expected output, or through differential testing.


## Building and running

Clone the project

```bash
  git clone https://github.com/lukyxu/rust-gen
```

Go to the project directory

```bash
  cd rust-gen
```

Build and run rustgen generator

```bash
  cargo run
```

Build and run rustgen runner for 100 programs and saving all outputs including generated code and binaries.

```bash
  cargo run --bin runner -- -n 100 -s
```

## Running Tests

To run tests, run the following command

```bash
  npm run test
```

## Features

Rust Gen creates programs with the following features:

<details><summary>Primitive types</summary>

| Feature      | Status |
| -----------  | -------|
| Int (all)    | 🟢    |
| UInt (all)   | 🟢    |
| Bool         | 🟢    |
| Char         | 🔴    |
| String       | 🔴    |
| Float        | 🔴    |

</details>

<details><summary>Composite types</summary>

| Feature      | Status |
| -----------  | -------|
| Tuple        | 🟢    |
| Array        | 🟢    |
| Box          | 🔴    |
| Pointer      | 🔴    |
| Enum         | 🔴    |
| Struct       | 🔴    |

</details>

<details><summary>Statements</summary>

| Feature              | Status |
| -------------------- | -------|
| Local Declaration    | 🟢    |
| Local Initialization | 🔴    |
| Expression           | 🟢    |
| Semicolon            | 🟢    |
| Item                 | 🟡    |

</details>

<details><summary>Expressions</summary>

| Feature               | Status |
| ----------------------| -------|
| Literal               | 🟢    |
| Binary                | 🟡    |
| Unary                 | 🟡    |
| Cast                  | 🟡    |
| If                    | 🟢    |
| Block                 | 🟢    |
| Ident                 | 🟡    |
| Tuple                 | 🟢    |
| Assign                | 🟢    |
| Call (Function)       | 🔴    |
| Call (Method)         | 🔴    |
| Type (Ascription)     | 🔴    |
| While                 | 🔴    |
| For Loop              | 🔴    |
| Loop                  | 🔴    |
| Match                 | 🔴    |
| Field                 | 🟢    |
| Index                 | 🟢    |
| Address Of            | 🔴    |
| Repeat (Array)        | 🔴    |
| Struct                | 🟢    |

</details>

<details><summary>Binary Operations</summary>

| Feature     | Status |
| ----------- | -------|
| Add         | 🟢    |
| Sub         | 🟢    |
| Mul         | 🟢    |
| Div         | 🟢    |
| Rem         | 🟢    |
| And         | 🟢    |
| Or          | 🟢    |
| BitXor      | 🔴    |
| BitAnd      | 🔴    |
| BitOr       | 🔴    |
| Shl         | 🔴    |
| Shr         | 🔴    |
| Eq          | 🟢    |
| Lq          | 🔴    |
| Ne          | 🟢    |
| Ge          | 🔴    |
| Gt          | 🔴    |

</details>

<details><summary>Unary Operations</summary>

| Feature     | Status |
| ----------- | -------|
| Deref       | 🔴    |
| Not         | 🟢    |
| Neg         | 🟢    |

</details>


## Authors
- [@Kerry Xu](https://www.github.com/lukyxu)