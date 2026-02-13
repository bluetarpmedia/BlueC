// Copyright 2025-2026 Neil Henderson
//
//! The `labels` module provides functionality to validate that label names are unique and `goto` targets are valid
//! within their function.

use std::collections::HashMap;

use crate::compiler_driver::{Diagnostic, Driver};
use crate::parser::{AstFunction, AstMetadata, AstNodeId, AstRoot, AstStatement};

use super::visitor;

/// Validates that label names are unique within the function, and that all `goto <label>` targets
/// have been declared.
pub fn validate_labels(ast_root: &mut AstRoot, metadata: &AstMetadata, driver: &mut Driver) {
    // Visit each function definition, and for each one, visit each statement and take note of the
    // declared labels and the goto labels.
    //
    //      While doing so, emit errors if labels are redeclared. But we have to verify goto labels
    //      after recording all of the label names, since it's possible to goto a label that is not
    //      declared until afterwards.
    //
    visitor::visit_function_defns(ast_root, &mut |func: &mut AstFunction| {
        let mut declared_labels: HashMap<String, AstNodeId> = HashMap::new();
        let mut goto_labels: HashMap<String, AstNodeId> = HashMap::new();

        let block = func.body.as_mut().unwrap();

        visitor::visit_statements_in_block(block, &mut |stmt: &mut AstStatement| {
            record_statement_labels(driver, metadata, stmt, &mut declared_labels, &mut goto_labels);
        });

        // Validate that all `goto <label>` targets have been declared.
        goto_labels.into_iter().for_each(|(label_name, goto_stmt_node_id)| {
            if !declared_labels.contains_key(&label_name) {
                emit_goto_undeclared_label_error_diagnostic(&label_name, goto_stmt_node_id, driver, metadata);
            }
        });
    });
}

fn record_statement_labels(
    driver: &mut Driver,
    metadata: &AstMetadata,
    stmt: &AstStatement,
    declared_labels: &mut HashMap<String, AstNodeId>,
    goto_labels: &mut HashMap<String, AstNodeId>,
) {
    match stmt {
        AstStatement::Labeled { node_id, label_name, .. } => {
            if declared_labels.contains_key(label_name) {
                let previous_decl_node_id = declared_labels.get(label_name).unwrap();
                emit_redeclared_label_error_diagnostic(label_name, *node_id, *previous_decl_node_id, driver, metadata);
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
    node_id: AstNodeId,
    previous_decl_node_id: AstNodeId,
    driver: &mut Driver,
    metadata: &AstMetadata,
) {
    let err = format!("Redefinition of label `{}`", label_name);
    let loc = metadata.get_source_location(node_id);
    let mut diag = Diagnostic::error_at_location(err, loc);

    let note_loc = metadata.get_source_location(previous_decl_node_id);
    diag.add_note(format!("`{}` was previously declared here:", label_name), Some(note_loc));

    driver.add_diagnostic(diag);
}

fn emit_goto_undeclared_label_error_diagnostic(
    label_name: &str,
    node_id: AstNodeId,
    driver: &mut Driver,
    metadata: &AstMetadata,
) {
    let err = format!("Use of undeclared label `{}` in goto statement", label_name);
    let loc = metadata.get_source_location(node_id);
    driver.add_diagnostic(Diagnostic::error_at_location(err, loc));
}
