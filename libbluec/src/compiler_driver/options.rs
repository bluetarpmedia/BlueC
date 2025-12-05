// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `options` module defines the compiler driver's options.

use std::collections::HashSet;

/// Options that control the compiler driver's behavior.
#[derive(Debug, Default)]
pub struct DriverOptions {
    /// Define preprocessor macros.
    pub preprocessor_defns: Vec<String>,

    /// Only run preprocess, compiler, and assembler steps to produce an object file (.o)
    pub generate_object_file: bool,

    /// The file to write the output to, either an object file (.o) or an executable binary file.
    pub output_file: Option<String>,

    /// Object files to link with.
    pub link_obj_files: Vec<String>,

    /// Libraries to link with.
    pub link_libs: Vec<String>,

    /// Enable warnings.
    pub warnings_enabled: bool,

    /// Treat warnings as errors.
    pub warnings_as_errors: bool,

    /// Only run preprocess and compiler steps to produce an assembly file (.s)
    pub only_create_asm_file: bool,

    /// Run the lexer and then stop.
    pub lex: bool,

    /// Run the lexer and parser and then stop.
    pub parse: bool,

    /// Run the lexer, parser and semantic analysis and then stop.
    pub validate: bool,

    /// Run the lexer, parser, sema, IR translation, and assembly codegen and then stop.
    pub codegen: bool,

    /// Prints the parsed AST and stops after parsing (before sema).
    pub print_ast: bool,

    /// Prints the type-checked AST and stops after sema.
    pub print_typechecked_ast: bool,

    /// Prints the BlueTac intermediate representation and stops after lowering to IR.
    pub print_ir: bool,

    /// Flags which were passed on the command-line as '-f<flag>'.
    pub flags: HashSet<String>,
}

/// A compiler driver flag specified on the command-line as '-f<flag>'.
pub struct DriverFlag;

impl DriverFlag {
    pub const PRINT_TERSE: &'static str = "print-terse";
    pub const PRINT_NO_SOURCE_LOC:  &'static str = "print-no-source-loc";
}
