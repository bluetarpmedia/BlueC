// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `sema` module is responsible for semantic analysis of the AST produced by the Parser.
//!
//! The main part of semantic analysis is type checking (see the [type_check] module). After that there are additional
//! analyses for unused symbols, label validation, switch statements, and warning about ambiguous (to the reader)
//! expressions that are missing parentheses.
//!
//! Sema also performs some constant folding.

pub mod constant_table;
pub mod symbol_table;
pub mod type_check;
pub mod type_conversion;
pub mod type_resolution;

mod constant_eval;
mod constant_folding;
mod expr;
mod labels;
mod switch_stmt;
mod visitor;

#[cfg(test)]
mod tests;

use crate::compiler_driver::{Driver, Warning};
use crate::core::SourceIdentifier;
use crate::ir;
use crate::parser;

use type_check::TypeChecker;

/// Analyzes the C AST produced by the Parser for semantic errors, performs typechecking and decorates the AST with
/// their resolved types, and then passes ownership of the AST to the IR lowering stage.
pub fn semantic_analysis(mut ast_root: parser::AstRoot, metadata: parser::AstMetadata, driver: &mut Driver) {
    // Type checking
    //      This is the main part of semantic analysis.
    //
    let mut chk = TypeChecker::new(metadata);
    type_check::type_check(&mut ast_root, &mut chk, driver);

    // Warn about expressions missing parentheses.
    //      Do this before constant folding so that the user can receive a warning before a constant expression
    //      is folded into a literal.
    //
    expr::warn_about_expressions_with_mixed_operators(&mut ast_root, &mut chk.metadata, driver);
    expr::warn_about_assignment_in_condition_missing_parens(&mut ast_root, &mut chk.metadata, driver);

    // Constant folding and switch statement validation.
    //      Only do so if we have no error diagnostics from type checking.
    //
    if !driver.has_error_diagnostics() {
        constant_folding::fold(&mut ast_root, &mut chk, driver);

        switch_stmt::validate_switch_statements(&mut ast_root, &mut chk, driver);
    }

    // Warn about implicit conversions.
    //
    expr::warn_about_implicit_arithmetic_conversions(&mut ast_root, &mut chk.metadata, driver);

    // Warn about binary and compound assignment expressions with invalid constant operands.
    //      E.g. divide by zero, shift by negative.
    //
    expr::warn_about_expressions_with_invalid_constant_operands(&mut ast_root, &mut chk.metadata, driver);

    // Warn about unused expression results (i.e. an expression statement with no side-effects).
    //
    expr::warn_about_unused_expression_results(&mut ast_root, &mut chk.metadata, driver);

    // Warn about unused symbols.
    //
    let unused_symbols = chk.symbols.get_unused_symbols();
    for (name, kind, loc) in unused_symbols {
        let symbol = SourceIdentifier(&name, loc);
        Warning::unused_symbol(symbol, kind, driver);
    }

    // Validate labels
    //      Verify that label names are unique and goto targets are valid in each function.
    //
    labels::validate_labels(&mut ast_root, &chk.metadata, driver);

    // Don't proceed to the next stage if we've emitted errors, or if client only wants to run up to this stage.
    if driver.has_error_diagnostics() || driver.options().validate {
        return;
    }

    // If client wants to print the typechecked AST then we're done.
    if driver.options().print_typechecked_ast {
        parser::printer::print(&ast_root, &chk.metadata);
        return;
    }

    // After we're finished with annotation, destructure the type annotator and take back ownership of what we need.
    let type_check::TypeChecker { metadata, symbols, constants, .. } = chk;

    // Pass the validated AST to the IR translaton stage.
    ir::translate(driver, ast_root, metadata, symbols, constants);
}
