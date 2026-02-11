// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `ir` module is responsible for lowering the C AST into the "BlueTac" intermediate representation (IR).
//!
//! BlueTac is a custom three-address code (TAC) IR for the BlueC compiler.
//! In the future, we'll add a lower-level IR in SSA form.

mod bluetac;
mod label_maker;
mod printer;
mod translator;

#[cfg(test)]
mod tests;

use crate::codegen;
use crate::compiler_driver;
use crate::parser;
use crate::sema::constant_table::ConstantTable;
use crate::sema::symbol_table::SymbolTable;

pub use bluetac::{
    BtBinaryOp, BtConstantValue, BtDefinition, BtFunctionDefn, BtInstruction, BtLabelIdentifier, BtRoot,
    BtStaticConstant, BtStaticStorageInitializer, BtStaticStorageVariable, BtSwitchCase, BtType, BtUnaryOp, BtValue,
};

/// Lowers the C AST produced by the parser into BlueTac intermediate representation (IR), and then passes
/// the generated IR to the codegen stage.
pub fn translate(
    driver: &mut compiler_driver::Driver,
    ast: parser::AstRoot,
    metadata: parser::AstMetadata,
    symbols: SymbolTable,
    constants: ConstantTable,
) {
    let (bt_root, symbols, constants) = translator::translate_ast_to_ir(ast, metadata, symbols, constants, driver);

    if driver.options().print_ir {
        printer::print(&bt_root, &symbols, &constants);
        return;
    }

    codegen::codegen(driver, bt_root, symbols, constants);
}
