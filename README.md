# Hindley-Milner Type Inference Example with Algorithm W

This repo contains an interpreter for a toy language based on simply-typed lambda calculus.
It is designed to be a practical illustration of how Algorithm W works, now blazingly fast in Rust! 🚀🦀

The code includes:
- `src/ast.rs`: Abstract syntax tree definitions for expressions
- `src/lexer.rs`: Lexer for tokenizing input
- `src/parser.rs`: Parser for building ASTs from tokens
- `src/type_check.rs`: Type inference/checking code implementing Algorithm W
- `src/eval.rs`: Evaluator for executing expressions
- `src/main.rs`: CLI interface

## Instructions to Build and Run

This project uses [Cargo](https://doc.rust-lang.org/cargo/) to build and run.

### Prerequisites

Make sure you have Rust and Cargo installed. You can install them from [rustup.rs](https://rustup.rs/).

### Building

```bash
cargo build --release
```

### Running

You can run the interpreter with an expression as a command-line argument:

```bash
cargo run -- "let id = x -> x in id 1"
```

Or run it in REPL mode:

```bash
cargo run
```

### Running Tests

```bash
cargo test
```

## Example Expressions

Here are some example expressions you can try:

```
1 + 2 * 3
let x = 5 in x + 10
x -> x
let id = x -> x in id 1
let twice = f -> x -> f (f x) in twice
if true then 1 else 2
```

## Original OCaml Version

The original OCaml implementation can still be found in the `/lib` and `/bin` directories. To build and run the OCaml version:

This project originally used [Dune](https://dune.build/) to build and run tests.

There are a couple of options to get Dune:
- If you already have [Nix](https://nixos.org), you can run `nix develop` in the root directory of this repo. That will install Dune with required OPAM packages.
- You can install Dune and the required OPAM packages manually, following the normal Dune documentation.

To run the web version (OCaml compiled to JavaScript):
Run `python3 -m http.server --bind 127.0.0.1` in the root directory and open `http://localhost:8000` in your browser.

