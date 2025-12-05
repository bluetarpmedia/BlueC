// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `sema` module is responsible for semantic analysis of the AST produced by the Parser.
//!
//! The main part of semantic analysis is type checking (see the `type_check` module). After that we have additional
//! analysis for unused symbols, label validation, switch statements, and ambiguous (to the reader) expressions that
//! are missing parentheses.

pub mod symbol_table;
pub mod type_conversion;
pub mod type_resolution;

mod constant_eval;
mod expr;
mod switch_stmt;
mod type_check;
mod visitor;

#[cfg(test)]
mod tests;

use crate::compiler_driver;
use crate::compiler_driver::diagnostics::{Diagnostic, SourceIdentifier};
use crate::compiler_driver::warnings::Warning;
use crate::ir;
use crate::lexer::SourceLocation;
use crate::parser;
use crate::parser::{AstFunction, AstNodeId, AstStatement};

use std::collections::HashMap;

/// Analyzes the C AST produced by the Parser for semantic errors, and then passes ownership of the AST to the IR
/// lowering stage.
pub fn semantic_analysis(
    mut ast_root: parser::AstRoot,
    metadata: parser::AstMetadata,
    driver: &mut compiler_driver::Driver,
) {
    // Type checking
    //      This is the main part of semantic analysis.
    //
    let (symbols, metadata) = type_check::type_check(&mut ast_root, metadata, driver);

    // Don't proceed with further sema if type checking produced errors.
    if driver.has_error_diagnostics() {
        return;
    }

    // Warn about unused symbols.
    //
    let unused_symbols = symbols.get_unused_symbols();
    for (name, kind, loc) in unused_symbols {
        let symbol = SourceIdentifier(&name, loc);
        Warning::unused_symbol(symbol, kind, driver);
    }

    // Validate labels
    //      Verify that label names are unique and goto targets are valid in each function.
    //
    validate_labels(&ast_root, driver, &metadata);

    // Validate switch statements
    //      Verify that switch cases have unique constant values.
    //
    let mut metadata = metadata;
    switch_stmt::validate_switch_statements(&ast_root, &mut metadata, driver);

    // Warn about expressions missing parentheses.
    //
    if driver.options().warnings_enabled {
        expr::warn_about_expressions_with_mixed_operators(&ast_root, driver, &mut metadata);
        expr::warn_about_assignment_in_condition_missing_parens(&ast_root, driver, &mut metadata);
    }

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
    ir::translate(driver, ast_root, metadata, symbols);
}

/// Validates that label names are unique within the function, and that all `goto <label>` targets
/// have been declared.
fn validate_labels(ast_root: &parser::AstRoot, driver: &mut compiler_driver::Driver, metadata: &parser::AstMetadata) {
    // Visit each function definition, and for each one, visit each statement and take note of the
    // declared labels and the goto labels.
    //
    //      While doing so, emit errors if labels are redeclared. But we have to verify goto labels
    //      after recording all of the label names, since it's possible to goto a label that is not
    //      declared until afterwards.
    //
    visitor::visit_functions(ast_root, &mut |func: &AstFunction| {
        let mut declared_labels: HashMap<String, AstNodeId> = HashMap::new();
        let mut goto_labels: HashMap<String, AstNodeId> = HashMap::new();

        let block = func.body.as_ref().unwrap();

        visitor::visit_statements_in_block(block, &mut |stmt: &AstStatement| {
            record_statement_labels(driver, metadata, stmt, &mut declared_labels, &mut goto_labels);
        });

        // Validate that all `goto <label>` targets have been declared.
        goto_labels.into_iter().for_each(|(label_name, goto_stmt_node_id)| {
            if !declared_labels.contains_key(&label_name) {
                emit_goto_undeclared_label_error_diagnostic(&label_name, &goto_stmt_node_id, driver, metadata);
            }
        });
    });
}

fn record_statement_labels(
    driver: &mut compiler_driver::Driver,
    metadata: &parser::AstMetadata,
    stmt: &AstStatement,
    declared_labels: &mut HashMap<String, AstNodeId>,
    goto_labels: &mut HashMap<String, AstNodeId>,
) {
    match stmt {
        AstStatement::Labeled { node_id, label_name, .. } => {
            if declared_labels.contains_key(label_name) {
                let previous_decl_node_id = declared_labels.get(label_name).unwrap();
                emit_redeclared_label_error_diagnostic(label_name, node_id, previous_decl_node_id, driver, metadata);
            } else {
                // We use `constains_key` above instead of testing the Option result of `insert`.
                // HashMap::insert updates the value if the key already exists, and then returns `Some` if
                // the key already existed and `None` otherwise. But we want to keep the first node_id
                // value that we found in the AST instead of updating it.
                _ = declared_labels.insert(label_name.clone(), *node_id);
            }
        }

        AstStatement::Goto { node_id, label_name } => {
            // Only insert the first time, e.g. don't warn on multiple `goto <undeclared_label>`, just the first one
            goto_labels.entry(label_name.clone()).or_insert(*node_id);
        }

        _ => (),
    }
}

fn emit_redeclared_label_error_diagnostic(
    label_name: &str,
    node_id: &AstNodeId,
    previous_decl_node_id: &AstNodeId,
    driver: &mut compiler_driver::Driver,
    metadata: &parser::AstMetadata,
) {
    let err = format!("Redefinition of label `{}`", label_name);
    let loc: SourceLocation = metadata.get_source_span(node_id).unwrap().into();
    let mut diag = Diagnostic::error_at_location(err, loc);

    let note_loc: SourceLocation = metadata.get_source_span(previous_decl_node_id).unwrap().into();
    diag.add_note(format!("`{}` was previously declared here:", label_name), Some(note_loc));

    driver.add_diagnostic(diag);
}

fn emit_goto_undeclared_label_error_diagnostic(
    label_name: &str,
    node_id: &AstNodeId,
    driver: &mut compiler_driver::Driver,
    metadata: &parser::AstMetadata,
) {
    let err = format!("Use of undeclared label `{}` in goto statement", label_name);
    let loc: SourceLocation = metadata.get_source_span(node_id).unwrap().into();
    driver.add_diagnostic(Diagnostic::error_at_location(err, loc));
}
