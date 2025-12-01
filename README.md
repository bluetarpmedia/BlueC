# BlueC
***A C compiler, written in Rust.*** 🦀

[![Build BlueC](https://github.com/bluetarpmedia/BlueC/actions/workflows/build-bluec.yml/badge.svg)](https://github.com/bluetarpmedia/BlueC/actions/workflows/build-bluec.yml)
[![Tests](https://img.shields.io/endpoint?url=https://gist.githubusercontent.com/bluetarpmedia/4a3049e2f6a53832726ab63f0b395357/raw/bluec-junit-tests.json)](https://github.com/bluetarpmedia/BlueC/actions/workflows/build-bluec.yml)
[![Build documentation](https://github.com/bluetarpmedia/BlueC/actions/workflows/build-docs.yml/badge.svg)](https://github.com/bluetarpmedia/BlueC/actions/workflows/build-docs.yml)

The initial goal is to write a fully conforming C17 compiler, from the preprocessor stage through to the assembly code emission stage, supporting multiple targets.

See the [Status and Roadmap](#status-and-roadmap) section for current progress.

By default, the BlueC compiler driver runs the appropriate platform development tool (`gcc` / `msvc`) for final assembly and linking to produce a binary/object file. If desired, the driver can stop after writing an `.s` file with the assembly code.

### Table of Contents

* [Usage](#usage)
  * [Single-file compilation](#traditional-single-file-compilation-to-object-file-for-later-linking)
  * [Multi-file compilation and linking](#multi-file-compilation-and-linking)
* [Targets](#targets)
* [Build and Test](#build-and-test)
  * [Dependencies & SBOM](#dependencies--sbom)
* [Design Goals](#design-goals)
* [Architecture](#architecture)
* [Compiler Pipeline](#compiler-pipeline)
* [Status and Roadmap](#status-and-roadmap)
* [Extensions](#extensions)
* [References](#references)

## Usage

```
Usage: BlueC [options] file...

Options:
  -D <macro>=<value>     Define <macro> to <value> (or 1 if <value> omitted)
  -S                     Only run preprocess and compilation steps to produce
                         a '.s' assembly file
  -Wall                  Enable all warnings
  -Werror                Treat warnings as errors
  -c                     Only run preprocess, compiler, and assembler steps to
                         produce an object file (.o)
  -l <lib>               Specifies a library to link with
  -o <file>              Write output to <file>

  -fprint-terse          Print terse diagnostics without text wrapping, nor
                         filename/line/column and source code
  -fprint-no-source-loc  Print diagnostics without filename/line/column

  Developer options
  --validate             Only run the lexer, parser, and sema stages
  --print-ast            Print the type-checked AST to stdout (implies --validate)

  -h, --help             Print help
  -V, --version          Print version
```

#### Traditional single-file compilation to object file, for later linking

```
$ bluec -c file1.c -o file1.o
$ bluec -c file2.c -o file2.o
$ <linker> file1.o file2.o -o my_program
$ ./my_program
```

#### Multi-file compilation and linking

```
$ bluec file1.c file2.c -o my_program
$ ./my_program
```

## Targets

BlueC currently supports:

* System V AMD64 ABI (x86_64) for macOS and Linux

Future targets:

* Microsoft Windows x86_64
* AArch64 ARM64
* WebAssembly

## Build and Test

Run `cargo build` to build the debug/unoptimized version.

Run `cargo build --release` to build the release/optimized version.

The crate has a [build script](build.rs) which generates integration tests based on the valid and invalid source files located in the [tests](tests) directory.

Run `cargo test` to run all unit tests and integration tests.

### Dependencies & SBOM

BlueC has been deliberately designed without depending on any third-party crates for the library and executable, and to use as few third-party dependencies as possible for integration tests and the build script. See [Design Goals](#-design-goals).

| Component                    | Third-party dependencies |
| ---------------------------- | ------------------------ |
| BlueC library                | None                     |
| BlueC compiler driver executable | None                 |
| Integration Tests            | **serde_json** <br> Used to load `expected_results.json` files containing expected exit codes |
| Build script                 | **serde_json, glob** <br> The `build.rs` build script generates test cases based on files under `tests/valid` and `tests/invalid`. It uses **glob** to find test `.c` source files and **serde_json** to parse `multi_file_tests.json` files. |

## Design goals

1. No third-party crate dependencies for the BlueC library and driver executable.<br>See [Dependencies & SBOM](#dependencies--sbom) for the crates that the integration tests and build script depend on.<br>The reasons for this choice are:
    * Minimize compile times (dependencies can often have long chains).
    * Easier to audit.
    * Avoid the risk of supply chain attacks.
2. No `unsafe` Rust, with one exception that can be disabled:
    * Parsing of hexadecimal floating-point literals (e.g. `0xFFp-1`) requires `unsafe` calls to C stdlib functions `strtod` and `strtof`.<br>
    This functionality is enabled by default but can be turned off by disabling the `hex-float-literal` feature. See [Cargo.toml](Cargo.toml).<br>If you disable that feature then there is no `unsafe` Rust in the BlueC library or driver executable.
3. Hand-written, not AI-generated. I'm happy to use LLMs for things like code completion, generating boilerplate, or doing repetitive tasks, but I'm writing the compiler myself.

## Architecture

BlueC is both a library and a binary executable.

All compiler stages, plus the compiler driver itself, are exported from the library.

The binary executable is a very thin wrapper around the `compiler_driver::Driver`.

## Compiler Pipeline

The BlueC compiler pipeline is as follows.

| Stage    | Notes   | Module  |
| -------- | ------- | ------- |
| Preprocessor | Currently uses an external tool (`gcc`); will replace with a custom preprocessor | [compiler_driver.rs](src/compiler_driver.rs) |
| Lexer | Hand-written | [lexer.rs](src/lexer.rs) |
| Parser | Hand-written, recursive descent with precedence climbing for binary operations | [parser.rs](src/parser.rs) |
| Sema | Semantic analysis of the C AST produced by the Parser | [sema.rs](src/sema.rs) |
| IR translation | Lowers the C AST into a custom three-address code (TAC) intermediate representation, called BlueTac | [translation.rs](src/translation.rs) |
| Optimizer | Todo! Lowers the high-level IR to SSA form; applies a series of optimization transformations (see below) | |
| Codegen | Generates an `x86_64` AST from the IR for the `System V AMD64 ABI` | [codegen.rs](src/codegen.rs) |
| Code emission | Writes the `x86_64` assembly code to an output file, in AT&T syntax (Intel syntax coming later) | [x86_emit.rs](src/codegen/x86_emit.rs) |
| Assembler & linker | Uses an external tool (`gcc`) | [compiler_driver.rs](src/compiler_driver.rs) |

## Status and Roadmap

* ✅ BlueC library and compiler driver executable
* ✅ Expressions and logical, bitwise, relational & arithmetic operators
* ✅ Variable, function and `typedef` declarations
* ✅ Symbol Table for identifier & type alias resolution, which allows us to solve the `type-identifier: name` grammar ambiguity problem
* ✅ If, compound, expression, return, and null (empty) statements
* ✅ Goto and labeled statements
* ✅ For, while, do-while loop statements
* ✅ Semantic analysis for type checking, expression annotation, and validating label names, goto statement targets, switch cases
* ✅ Compile-time evaluation of constant expressions
* ✅ Switch statements
* ✅ Function calls
* ✅ File scope declarations and storage-specifiers
* Literals
  * ✅ Integer (Decimal, Hex, Octal, Binary (as an extension) and suffixes).
  * ✅ Floating Point (Decimal and Hex)
  * Character
  * String
* Types
  * ✅ `short` (16-bit)
  * ✅ `int` (32-bit)
  * ✅ `long` (64-bit)
  * ✅ `long long` (64-bit)
  * ✅ `signed` and `unsigned`
  * ✅ `float`, `double` and `long double` <br>
  `long double` is effectively an alias for `double`; this is Standard-conforming but in future we may support 80-bit and/or 128-bit long doubles for certain targets
  * ✅ Pointers
  * Function pointers
  * Arrays
  * `_Bool`, `char`, `void`
  * Structs
  * Enums
  * Unions
* Type qualifiers (`const`, `volatile`, `restrict`, `_Atomic`)
* CI improvements
  * Fuzzing
  * Miri
* Optimization in various stages
  * String interning
  * Refactor BlueTac IR, and add BlueTac SSA form
  * Constant folding and propagation
  * Dead code elimination (DCE)
  * Dead store elimination
  * Switch statement jump table and binary tree
  * Tail recursion
  * Instruction selection
  * Instruction scheduling
  * Register allocation
* Emit either `x86_64` Intel syntax or AT&T syntax
* Microsoft `x86_64` ABI and use `link.exe` on Windows
* AArch64 (ARM64) and WebAssembly targets
* Extensions! (See below)
* Bytecode interpreter for constant expression evaluation
* Custom preprocessor
* Sanitizers
* C23 features

## Extensions

Ideas for non-standard extensions:

* Embedded DSL: define your own DSL grammar inside your `.c` source file, compiler generates the DSL parser at compile-time, call the generated DSL parser function at runtime with runtime-supplied input data.
* `--strict` mode which removes various undefined behaviour:
  * Compile-time: Diagnose uninitialized variables (unless data flow analysis proves they are set before read).
  * Runtime: Checked arithmetic.
  * Runtime: Null pointer tests.
  * Runtime: Experiment with fat pointers for potential spatial bounds safety and use-after-free safety.
* More `constexpr` / `consteval`-style support beyond what C23 allows.
* Reflection and generation.
* Pattern matching.
* Rust interop (like an in-built version of `cbindgen`)
  * `use <path to Cargo.toml>;`
  * BlueC then scans and finds all `#[repr(C)]` `pub structs` and `#[no_mangle]` `pub extern "C" functions` in the crate.
* Linear types.

## References

* 📘 Engineering a Compiler, 3rd Ed, Cooper & Torczon (2022)
* 📘 LLVM Code Generation: A deep dive into compiler backend development, Colombet (2025)
* 📘 Writing a C Compiler, Sandler (2024)
* 🌐 [Combining Analyses, Combining Optimizations](https://www.researchgate.net/publication/2394127_Combining_Analyses_Combining_Optimizations), Clifford Click (1995)
* 🌐 [Simple and Efficient Construction of Static Single Assignment Form](https://link.springer.com/chapter/10.1007/978-3-642-37051-9_6), Braun et al. (2013)
* 🌐 [Sea of Nodes](https://darksi.de/d.sea-of-nodes/), Fedor Indutny (2015)
* 🌐 [The context sensitivity of C's grammar](https://eli.thegreenplace.net/2007/11/24/the-context-sensitivity-of-cs-grammar/), Eli Bendersky (2007)
* 🌐 [The context sensitivity of C’s grammar, revisited ](https://eli.thegreenplace.net/2011/05/02/the-context-sensitivity-of-cs-grammar-revisited/), Eli Bendersky (2011)
* 🌐 [Reading C type declarations](http://unixwiz.net/techtips/reading-cdecl.html), Steve Friedl (2003)
