// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The codegen module converts the BlueTac IR to an x86_64 Assembly AST and then writes/emits
//! the assembly code to an '.s' file.
//! In the future, we'll add other targets such as Arm64.

mod x86;

use crate::compiler_driver;
use crate::ir;
use crate::sema::constant_table::ConstantTable;
use crate::sema::symbol_table::SymbolTable;

/// Generates the assembly code for the given IR program and then emits/writes the code to an '.s' file.
pub fn codegen(
    driver: &mut compiler_driver::Driver,
    bt_root: ir::BtRoot,
    symbols: SymbolTable,
    constants: ConstantTable,
) {
    // Create an assembly AST of the program and transform the parser's symbol table into
    // a back-end symbol table for assembly code generation.
    let (asm_root, asm_symbols) = x86::generate_asm(bt_root, symbols, constants);

    // If user only wants to run the lexer, parser, IR translation & codegen then we're done.
    if driver.options().codegen {
        return;
    }

    // Code emission: write to '.s' file.
    x86::emit_asm_to_file(&asm_root, &driver.asm_filename, asm_symbols);
}
