// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `sema` module is responsible for semantic analysis of the AST produced by the Parser.
//!
//! The main part of semantic analysis is type checking (see the `type_check` module). After that there are additional
//! analyses for unused symbols, label validation, switch statements, and warning about ambiguous (to the reader)
//! expressions that are missing parentheses.
//!
//! Sema also performs integer literal promotion, where a Cast expression containing an IntegerLiteral expression is
//! replaced with a new IntegerLiteral expression of the desired type (where possible).

pub mod constant_eval;
pub mod constant_table;
pub mod symbol_table;
pub mod type_conversion;
pub mod type_resolution;

mod expr;
mod labels;
mod literal_promotion;
mod switch_stmt;
mod type_check;
mod visitor;

#[cfg(test)]
mod tests;

use crate::compiler_driver::{Driver, Warning};
use crate::core::SourceIdentifier;
use crate::ir;
use crate::parser;

/// Analyzes the C AST produced by the Parser for semantic errors, performs typechecking and decorates the AST with
/// their resolved types, and then passes ownership of the AST to the IR lowering stage.
pub fn semantic_analysis(mut ast_root: parser::AstRoot, metadata: parser::AstMetadata, driver: &mut Driver) {
    // Type checking
    //      This is the main part of semantic analysis.
    //
    let (symbols, constants, mut metadata) = type_check::type_check(&mut ast_root, metadata, driver);

    // Validate labels
    //      Verify that label names are unique and goto targets are valid in each function.
    //
    labels::validate_labels(&mut ast_root, driver, &metadata);

    // Numeric literal promotion
    literal_promotion::promote_integer_literals(&mut ast_root, &mut metadata);

    // Warn about unused symbols.
    //
    let unused_symbols = symbols.get_unused_symbols();
    for (name, kind, loc) in unused_symbols {
        let symbol = SourceIdentifier(&name, loc);
        Warning::unused_symbol(symbol, kind, driver);
    }

    // Warn about expressions missing parentheses.
    //
    expr::warn_about_expressions_with_mixed_operators(&mut ast_root, driver, &mut metadata);
    expr::warn_about_assignment_in_condition_missing_parens(&mut ast_root, driver, &mut metadata);

    // Don't proceed to the next stage if we've emitted errors, or if client only wants to run up to this stage.
    if driver.has_error_diagnostics() || driver.options().validate {
        return;
    }

    // If client wants to print the typechecked AST then we're done.
    if driver.options().print_typechecked_ast {
        parser::printer::print(&ast_root, Some(&metadata));
        return;
    }

    // Pass the validated AST to the IR translaton stage.
    ir::translate(driver, ast_root, metadata, symbols, constants);
}
