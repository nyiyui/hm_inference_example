# Hindley-Milner Type Inference Example with Algorithm W

This repo contains an interpreter for a toy language based on simply-typed lambda calculus.
It is designed to be a practical illustration of how Algorithm W works in OCaml code.

Most of the code (e.g. parser, type checker) is contained in `/lib`, with `/lib/type_check.ml` being the type inference/checking code, `/lib/parser.mly` containing parser definitions, etc.

## Instructions to Run

This project uses [Dune](https://dune.build/) to build and run tests.

There are a couple of options to get Dune:
- If you already have [Nix](https://nixos.org), you can run `nix develop` in the root directory of this repo. That will install Dune with required OPAM packages.
- You can install Dune and the required OPAM packages manually, following the normal Dune documentation.
