// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `type_check` module defines the main part of the semantic analysis stage, which is type checking.
//! This involves the following tasks:
//! - type resolution: Resolve all declared types in the AST to their canonical `AstType` representations.
//! - type annotation: Annotate every expression in the AST with its `AstType`, and add implicit type casts
//!   where necessary.
//! - type validation: Emit diagnostics for semantic errors where usage of types are invalid.
//! - build symbols:   Build a Symbol Table of all symbols in the AST and their types.

pub(super) mod checker;
pub(super) mod utils;

mod symbols;
mod traverse;

use crate::compiler_driver::Driver;
use crate::parser::{AstMetadata, AstRoot};

use self::checker::TypeChecker;
use super::constant_table::ConstantTable;
use super::switch_stmt;
use super::symbol_table::SymbolTable;
use super::type_resolution;

/// Performs type checking on the AST.
pub fn type_check(
    ast_root: &mut AstRoot,
    metadata: AstMetadata,
    driver: &mut Driver,
) -> (SymbolTable, ConstantTable, AstMetadata) {
    let symbols = SymbolTable::new();
    let constants = ConstantTable::new();
    let mut checker = TypeChecker::new(metadata, symbols, constants);

    traverse::typecheck_ast(ast_root, &mut checker, driver);

    // Switch statement cases must have unique constant values. But skip this validation if we already have
    // type checking errors, since that may prevent us performing the validation.
    if !driver.has_error_diagnostics() {
        switch_stmt::validate_switch_statements(ast_root, &mut checker, driver);
    }

    // After we're finished with annotation, destructure the type annotator and take back ownership of what we need.
    let TypeChecker { metadata, symbols, constants, .. } = checker;

    (symbols, constants, metadata)
}
