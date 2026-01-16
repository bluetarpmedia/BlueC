// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `switch_stmt` module provides functionality to validate switch statements.

use super::constant_eval;
use super::type_check::checker::TypeChecker;
use super::visitor;

use crate::ICE;
use crate::compiler_driver::Driver;
use crate::compiler_driver::diagnostics::Diagnostic;
use crate::compiler_driver::warnings::Warning;
use crate::parser;
use crate::parser::{AstConstantValue, AstFunction, AstStatement};

/// Validates all switch statements in the AST.
///
/// Cases in a switch statement must have constant expressions that evaluate to unique integer values.
pub fn validate_switch_statements(ast_root: &mut parser::AstRoot, chk: &mut TypeChecker, driver: &mut Driver) {
    // Visit each function definition, and for each one, visit each switch statement and verify that its cases have
    // unique constant values.
    //
    visitor::visit_functions(ast_root, &mut |func: &mut AstFunction| {
        let block = func.body.as_mut().unwrap();

        visitor::visit_statements_in_block(block, &mut |stmt: &mut AstStatement| {
            if let AstStatement::Switch { node_id: enclosing_switch_node_id, controlling_expr, body } = stmt {
                // Get the type of the switch statement's controlling expression.
                //      All case values must be of this type or cast to it.
                //
                let current_switch_data_type = chk.metadata.get_node_type(&controlling_expr.node_id).cloned();

                // Visit all the 'case' statements inside the switch body.
                //      Remember this will find 'case' statements in nested switches too.
                //
                visitor::visit_statement(body, &mut |stmt: &mut AstStatement| {
                    if let AstStatement::Case { switch_node_id, constant_expr, .. } = stmt {
                        // We only want to examine 'case' statements immediately in our own switch body, not nested
                        // switch statement cases.
                        if switch_node_id != enclosing_switch_node_id {
                            return;
                        }

                        // Evaluate the expression and ensure it's a constant value.
                        //
                        let ctx = constant_eval::ConstantEvalContext::from_type_checker(chk, driver);
                        let constant_value = constant_eval::evaluate_constant_full_expr(constant_expr, ctx);
                        if constant_value.is_none() {
                            let loc = chk.metadata.get_source_span_as_loc(&constant_expr.node_id).unwrap();
                            let err = "Expression cannot be evaluated at compile-time".to_string();
                            driver.add_diagnostic(Diagnostic::error_at_location(err, loc));
                            return;
                        };

                        // Ensure the constant expression evaluated as an integer, and not a float.
                        let mut constant_value = constant_value.unwrap();

                        if !matches!(constant_value, AstConstantValue::Integer(_)) {
                            let loc = chk.metadata.get_source_span_as_loc(&constant_expr.node_id).unwrap();
                            let err = "Case label expression must evaluate as an integer constant".to_string();
                            driver.add_diagnostic(Diagnostic::error_at_location(err, loc));
                            return;
                        }

                        // If necessary cast the case constant value to the type of the switch statement's
                        // controlling expression.
                        let case_data_type = constant_value.get_ast_type();
                        let switch_data_type = current_switch_data_type.as_ref().unwrap();

                        if case_data_type != *switch_data_type {
                            let old_constant_value = constant_value.clone(); // In case we need to warn of conversion

                            // Cast the constant value to the new type
                            constant_value = constant_value.cast_to(switch_data_type);

                            if constant_value != old_constant_value {
                                let old_value = old_constant_value.to_string();
                                let new_value = constant_value.to_string();
                                let loc = chk.metadata.get_source_span_as_loc(&constant_expr.node_id).unwrap();
                                Warning::implicit_switch_case_conversion(
                                    &case_data_type,
                                    switch_data_type,
                                    &old_value,
                                    &new_value,
                                    loc,
                                    driver,
                                );
                            }
                        }

                        let AstConstantValue::Integer(constant_integer) = constant_value else {
                            ICE!("Switch case value '{constant_value}' should be a constant integer");
                        };

                        // Add the switch case value to the metadata and emit a diagnostic if it's not unique.
                        //
                        if let Some(existing_case_node_id) =
                            chk.metadata.add_switch_case(*switch_node_id, constant_expr.node_id, constant_integer)
                        {
                            let err = format!("Duplicate case value '{}'", constant_value);
                            let mut diag = Diagnostic::error_at_location(
                                err,
                                chk.metadata.get_source_span_as_loc(&constant_expr.node_id).unwrap(),
                            );

                            diag.add_note(
                                "Previous case defined here:".to_string(),
                                chk.metadata.get_source_span_as_loc(&existing_case_node_id),
                            );
                            driver.add_diagnostic(diag);
                        }
                    }
                });
            }
        });
    });
}
