# BlueC
***A C compiler, built with Rust.*** 🦀 ***Handwritten, not AI-generated.***

[![Build BlueC](https://github.com/bluetarpmedia/BlueC/actions/workflows/build-bluec.yml/badge.svg)](https://github.com/bluetarpmedia/BlueC/actions/workflows/build-bluec.yml)
[![Tests](https://img.shields.io/endpoint?url=https://gist.githubusercontent.com/bluetarpmedia/4a3049e2f6a53832726ab63f0b395357/raw/bluec-junit-tests.json)](https://github.com/bluetarpmedia/BlueC/actions/workflows/build-bluec.yml)
[![Build documentation](https://github.com/bluetarpmedia/BlueC/actions/workflows/build-docs.yml/badge.svg)](http://bluetarpmedia.github.io/BlueC/libbluec/index.html)

The initial goal is to write a fully conforming C17 optimizing compiler, from the preprocessor stage through to the assembly code emission stage, supporting multiple targets, with a focus on friendly diagnostics, and using no `unsafe` Rust and no third-party dependencies.

See the [Status and Roadmap](#status-and-roadmap) section for current progress and [Design Goals](#design-goals) for further information about design choices.

See also the BlueC crate [documentation](http://bluetarpmedia.github.io/BlueC/libbluec/index.html).

### Table of Contents

* [License and Contributing](#license-and-contributing)
* [Usage](#usage)
  * [Single-file compilation](#traditional-single-file-compilation-to-object-file-for-later-linking)
  * [Multi-file compilation and linking](#multi-file-compilation-and-linking)
* [Targets](#targets)
* [Build and Test](#build-and-test)
  * [Dependencies & SBOM](#dependencies--sbom)
* [Design Goals](#design-goals)
* [Architecture](#architecture)
  * [Library Modules](#library-modules)
  * [Unit Tests](#unit-tests)
  * [Integration Tests](#integration-tests)
* [Compiler Pipeline](#compiler-pipeline)
* [Status and Roadmap](#status-and-roadmap)
* [Extensions](#extensions)
* [References](#references)

## License and Contributing

I haven't decided on a license yet but will probably go with the typical `MIT OR Apache-2.0` like most Rust crates do.

I'm not taking contributions yet because this is a personal project where I want to implement and solve most of the problems myself, at least until most of the optimizer and back-end is complete.

## Usage

```
Usage: BlueC [options] file...

Options:
  -D <macro>=<value>     Define <macro> to <value> (or 1 if <value> omitted)
  -S                     Only run preprocess and compilation steps to produce
                         a '.s' assembly file
  -c                     Only run preprocess, compiler, and assembler steps to
                         produce an object file (.o)
  -l <lib>               Specifies a library to link with
  -o <file>              Write output to <file>

  -Wall                  Enable all warnings
  -w                     Disable all warnings
  -Werror                Treat warnings as errors
  -W<warning>            Enable a specific warning
  -Wno-<warning>         Disable a specific warning

  -fprint-terse          Print terse diagnostics without text wrapping, nor
                         filename/line/column and source code
  -fprint-no-source-loc  Print diagnostics without filename/line/column

  Developer options
  --parse                Only run the lexer and parser stages
  --validate             Only run the lexer, parser, and sema stages
  --print-ast            Print the parsed AST to stdout (implies --parse)
  --print-tast           Print the type-checked AST to stdout (implies --validate)
  --print-ir             Print the BlueTac intermediate representation

  -h, --help             Print help
  -V, --version          Print version
```

#### Compile to assembly code

```
$ bluec -S file.c
```

#### Traditional single-file compilation to object file, for later linking

The BlueC driver invokes `gcc` to run the assembler.

```
$ bluec -c file1.c -o file1.o
$ bluec -c file2.c -o file2.o
$ <linker> file1.o file2.o -o my_program
$ ./my_program
```

#### Multi-file compilation and linking

The BlueC driver invokes `gcc` to run the assembler and linker.
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
3. Handwritten, not AI-generated. I'm happy to use LLMs for things like code completion, generating boilerplate, or doing repetitive tasks, but I'm writing the compiler myself.

## Architecture

BlueC is both a library and a binary executable. The Cargo workspace includes 3 crates:

| Crate       | Notes |
| ----------- | ----- |
| libbluec    | The BlueC compiler library |
| bluec       | The binary executable (a very thin wrapper which invokes the library's `compiler_driver`) |
| bluec-tests | Integration tests |

### Library Modules

Currently, the entire compiler is implemented in one `libbluec` crate and each stage in the compiler is a different top-level module in the crate. (In future, we may extract these stages into their own crates.) See [Compiler Pipeline](#compiler-pipeline) for a description of the top-level modules.

### Unit Tests

Each library module's unit tests are placed in their own `tests.rs` submodule, and never written directly next to code within the module they are testing. (E.g. see [parser/tests.rs](libbluec/src/parser/tests.rs) or [sema/tests.rs](libbluec/src/sema/tests.rs)). This is a deliberate strategy to optimize build times (see 'Assorted Tricks' in [One Hundred Thousand Lines of Rust](https://matklad.github.io/2021/02/27/delete-cargo-integration-tests.html)).

### Integration Tests

[![Tests](https://img.shields.io/endpoint?url=https://gist.githubusercontent.com/bluetarpmedia/4a3049e2f6a53832726ab63f0b395357/raw/bluec-junit-tests.json)](https://github.com/bluetarpmedia/BlueC/actions/workflows/build-bluec.yml)

The vast majority of tests are integration tests. There are 3 categories of test cases:

| Tests | Description |
| ---------- | ----- |
| Valid      | Valid C source files.<br>We expect to successfully compile each file without any error diagnostics, and then we run the resulting executable file and check its return code against an expected result. In addition, some tests also link with object files compiled by `gcc` to verify ABI compatibility. |
| Invalid    | Invalid (ill-formed) C source files.<br>We expect the compiler to emit one or more error diagnostics when compiling these files. |
| Warnings   | Syntactically valid C source files which should produce warnings.<br>When compiling we expect the compiler to emit one or more warning diagnostics. We verify the emitted warnings match the expected results. |

## Compiler Pipeline

The BlueC compiler pipeline is as follows.

| Stage    | Notes   | Module  |
| -------- | ------- | ------- |
| Preprocessor | Currently uses an external tool (`gcc`); will replace with a custom preprocessor | [compiler_driver.rs](libbluec/src/compiler_driver.rs) |
| Lexer | Hand-written | [lexer.rs](libbluec/src/lexer.rs) |
| Parser | Hand-written, recursive descent with precedence climbing for binary operations | [parser.rs](libbluec/src/parser.rs) |
| Sema | Semantic analysis of the C AST produced by the Parser | [sema.rs](libbluec/src/sema.rs) |
| IR lowering | Lowers the C AST into a custom three-address code (TAC) intermediate representation, called BlueTac | [ir.rs](libbluec/src/ir.rs) |
| Optimizer | Todo! Lowers the IR to SSA form; applies a series of optimization transformations (see below) | |
| Codegen | Generates an `x86_64` AST from the IR for the `System V AMD64 ABI` | [codegen.rs](libbluec/src/codegen.rs) |
| Code emission | Writes the `x86_64` assembly code to an output file, in AT&T syntax (Intel syntax coming later) | [x86_emit.rs](libbluec/src/codegen/x86/emit.rs) |
| Assembler & linker | Uses an external tool (`gcc`) | [compiler_driver.rs](libbluec/src/compiler_driver.rs) |

## Status and Roadmap

So far I've concentrated mostly on the front-end to implement C language features, perform semantic analysis, and building the infrastructure to emit really nice, friendly diagnostics. The only optimization so far is some constant folding in sema.

* Compiler driver
  * ✅ Single-file compilation to object file
  * ✅ Single and multi-file compilation and linking
  * ✅ Write `x86_64` assembly to `.s` file
* Language support
  * ✅ Literals
    * ✅ Integer: Decimal, Hex, Octal, Binary and suffixes.
    * ✅ Floating Point: Decimal and Hex
    * ✅ Character
    * ✅ String
  * ✅ Expressions
  * ✅ Declarations, storage-specifiers, and `typedef`
  * ✅ Statements
      * ✅ Expression
      * ✅ Compound
      * ✅ Control (if, switch, while, do-while, for, break, continue, goto, return)
      * ✅ Labeled
  * Types
    * ✅ `char` (8-bit)
    * ✅ `short` (16-bit)
    * ✅ `int` (32-bit)
    * ✅ `long` (64-bit)
    * ✅ `long long` (64-bit)
    * ✅ `signed` and `unsigned`
    * ✅ `float`, `double` and `long double` <br>
    `long double` is effectively an alias for `double`; this is Standard-conforming but in future we may support 80-bit and/or 128-bit long doubles for certain targets
    * ✅ Pointers
    * ✅ Function pointers
    * ✅ Arrays
    * `_Bool`, `void`
    * Structs
    * Enums
    * Unions
  * Type qualifiers (`const`, `volatile`, `restrict`, `_Atomic`)
* Sema
  * ✅ Type checking
  * ✅ Compile-time constant expression evaluator
  * ✅ Constant folding
  * ✅ Symbol Table for identifier & type alias resolution, which allows us to solve the `type-identifier: name` grammar ambiguity problem
* Warning diagnostics (`-W` or `-Wno-`)
  * Literals
    * ✅ `multichar, unknown-escape-sequence, implicitly-unsigned-literal, literal-range`
  * Declarations and initializers
    * ✅ `missing-declarations, duplicate-decl-specifier, extern-initializer, uninitialized, unused-variable, unused-function, unused-local-typedef, 
excess-initializers, missing-braces, many-braces-around-scalar-init`
  * Expressions
    * ✅ `logical-op-parentheses, bitwise-op-parentheses, parentheses, array-bounds, unused-value, unused-comparison`
  * Arithmetic
      * ✅ `division-by-zero, integer-overflow, floating-point-overflow, shift-count-negative, shift-count-overflow, shift-count-zero`
  * Conversions and casts
      * ✅ `constant-conversion, implicit-conversion, implicit-promotion-conversion, implicit-int-conversion, implicit-float-conversion, implicit-int-float-conversion, float-conversion, sign-conversion, pointer-to-int-cast, non-literal-null-conversion`
  * Comparisons
      * ✅ `compare-distinct-pointer-types, pointer-integer-compare`
  * Types
      * ✅ `conditional-type-mismatch, pointer-type-mismatch`
* Front-end improvements
  * String interning
  * Custom preprocessor
  * C23 features
  * Bytecode interpreter for constant expression evaluation
  * Extensions! (See below)
* ✅ BlueTac three-address code IR
* Optimizer
  * Refactor BlueTac IR, add SSA form, build CFG
  * Inlining, loop optimization, common sub-expression elimination
  * Constant folding and propagation
  * Dead store elimination
  * Dead code elimination
      * Including unreachable code elimination
* Back-end
  * ✅ Emit AT&T `x86_64` for System V AMD64 ABI for macOS and Linux
  * Switch statement jump table and binary tree heuristics
  * Tail recursion
  * Instruction selection
  * Instruction scheduling
  * Register allocation
  * Emit either `x86_64` Intel syntax or AT&T syntax
  * Sanitizers
  * More targets
    * Microsoft Windows `x86_64` ABI
    * `AArch64` (ARM64)
    * WebAssembly
* CI
  * ✅ Linux
  * macOS
  * Fuzzing
  * Miri

## Extensions

Ideas for non-standard extensions:

* Embedded DSL: define your own DSL grammar inside your `.c` source file, compiler generates the DSL parser at compile-time, call the generated DSL parser function at runtime with runtime-supplied input data.
* A compilation mode which adds extra checks at runtime to detect UB:
  * Checked arithmetic
  * Null pointer tests
  * Experiment with fat pointers for potential spatial bounds safety and use-after-free safety.
* More `constexpr` / `consteval`-style support beyond what C23 allows.
* Reflection and generation.
* Pattern matching.
* Rust interop (like an in-built version of `cbindgen`)
  * `use <path to Cargo.toml>;`
  * BlueC then scans and finds all `#[repr(C)]` `pub structs` and `#[no_mangle]` `pub extern "C" functions` in the crate.
* Linear types (ensure a variable is used exactly once).

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
