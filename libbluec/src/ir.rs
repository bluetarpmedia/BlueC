// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `ir` module is responsible for lowering the C AST into the "BlueTac" intermediate representation (IR).
//!
//! BlueTac is a custom three-address code (TAC) IR for the BlueC compiler.
//! In the future, we'll add a Static Single Assignment (SSA) constraint.

mod bluetac;
mod label_maker;
mod printer;
mod translator;

#[cfg(test)]
mod tests;

use crate::codegen;
use crate::compiler_driver;
use crate::parser;
use crate::sema::symbol_table::SymbolTable;

pub use bluetac::{
    BtBinaryOp, BtConstantValue, BtDefinition, BtFunctionDefn, BtInstruction, BtLabelIdentifier, BtRoot,
    BtStaticStorageVariable, BtSwitchCase, BtType, BtUnaryOp, BtValue,
};

/// Translates the C AST produced by the parser into BlueTac intermediate representation (IR), and then passes
/// the generated IR to the codegen stage.
pub fn translate(
    driver: &mut compiler_driver::Driver,
    ast: parser::AstRoot,
    metadata: parser::AstMetadata,
    symbols: SymbolTable,
) {
    let (bt_root, symbols) = translator::translate_ast_to_ir(ast, metadata, symbols);

    if driver.options().print_ir {
        printer::print(&bt_root, &symbols);
        return;
    }

    codegen::codegen(driver, bt_root, symbols);
}
