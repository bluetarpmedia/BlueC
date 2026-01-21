// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `expr` module provides semantic analysis functionality for expressions.

use crate::compiler_driver;
use crate::compiler_driver::Warning;
use crate::parser;
use crate::parser::{
    AstBinaryOp, AstBinaryOpFamily, AstExpression, AstFullExpression, AstFunction, AstNodeId, AstStatement,
};

use super::visitor;

/// Emit warnings about expressions with mixed logical or bitwise operators and missing parentheses.
pub fn warn_about_expressions_with_mixed_operators(
    ast_root: &mut parser::AstRoot,
    driver: &mut compiler_driver::Driver,
    metadata: &mut parser::AstMetadata,
) {
    visitor::visit_full_expressions(ast_root, &mut |full_expr: &mut AstFullExpression| {
        visitor::visit_expressions_in_full_expression(full_expr, &mut |expr: &mut AstExpression| {
            if let AstExpression::BinaryOperation { node_id, op, left, right } = expr {
                match op {
                    // Mixed logical || and &&
                    //
                    AstBinaryOp::LogicalOr => {
                        let child_op = AstBinaryOp::LogicalAnd;
                        if is_binary_expr_with_op_missing_parens(child_op, left, metadata) {
                            emit_missing_parens_warning(*op, child_op, node_id, &left.node_id(), driver, metadata);
                        }

                        if is_binary_expr_with_op_missing_parens(child_op, right, metadata) {
                            emit_missing_parens_warning(*op, child_op, node_id, &right.node_id(), driver, metadata);
                        }
                    }
                    // Mixed bitwise operators (with any other kind of operator)
                    //
                    op if op.family() == AstBinaryOpFamily::Bitwise => {
                        if let Some(child_op) = is_binary_expr_missing_parens(left, metadata) {
                            emit_missing_parens_warning(*op, child_op, node_id, &left.node_id(), driver, metadata);
                        }

                        if let Some(child_op) = is_binary_expr_missing_parens(right, metadata) {
                            emit_missing_parens_warning(*op, child_op, node_id, &right.node_id(), driver, metadata);
                        }
                    }
                    _ => (),
                }
            }
        });
    });
}

/// Emit warnings about conditions using the result of an assignment without parentheses
pub fn warn_about_assignment_in_condition_missing_parens(
    ast_root: &mut parser::AstRoot,
    driver: &mut compiler_driver::Driver,
    metadata: &mut parser::AstMetadata,
) {
    visitor::visit_functions(ast_root, &mut |func: &mut AstFunction| {
        let block = func.body.as_mut().unwrap();
        visitor::visit_statements_in_block(block, &mut |stmt: &mut AstStatement| match stmt {
            AstStatement::If { controlling_expr, .. } if is_assignment_without_parens(controlling_expr, metadata) => {
                emit_assignment_used_in_condition_without_parens_warning(
                    &controlling_expr.node_id,
                    &controlling_expr.expr.node_id(),
                    driver,
                    metadata,
                );
            }
            AstStatement::While { controlling_expr, .. }
                if is_assignment_without_parens(controlling_expr, metadata) =>
            {
                emit_assignment_used_in_condition_without_parens_warning(
                    &controlling_expr.node_id,
                    &controlling_expr.expr.node_id(),
                    driver,
                    metadata,
                );
            }
            AstStatement::DoWhile { controlling_expr, .. }
                if is_assignment_without_parens(controlling_expr, metadata) =>
            {
                emit_assignment_used_in_condition_without_parens_warning(
                    &controlling_expr.node_id,
                    &controlling_expr.expr.node_id(),
                    driver,
                    metadata,
                );
            }
            AstStatement::For { controlling_expr, .. }
                if controlling_expr.as_ref().is_some_and(|expr| is_assignment_without_parens(expr, metadata)) =>
            {
                let controlling_expr = controlling_expr.as_ref().unwrap();
                emit_assignment_used_in_condition_without_parens_warning(
                    &controlling_expr.node_id,
                    &controlling_expr.expr.node_id(),
                    driver,
                    metadata,
                );
            }
            _ => (),
        });
    });
}

fn is_assignment_without_parens(full_expr: &AstFullExpression, metadata: &mut parser::AstMetadata) -> bool {
    if let AstExpression::Assignment { node_id, .. } = &full_expr.expr {
        return !metadata.expr_has_parens(*node_id);
    }

    false
}

fn emit_assignment_used_in_condition_without_parens_warning(
    full_expr_node_id: &AstNodeId,
    assignment_node_id: &AstNodeId,
    driver: &mut compiler_driver::Driver,
    metadata: &mut parser::AstMetadata,
) {
    let full_expr_loc = metadata.get_source_location(full_expr_node_id);
    let assignment_loc = metadata.get_source_location(assignment_node_id);
    Warning::assignment_in_condition_missing_parens(full_expr_loc, assignment_loc, driver);
}

fn emit_missing_parens_warning(
    parent_expr_op: AstBinaryOp,
    child_expr_op: AstBinaryOp,
    parent_expr_node_id: &AstNodeId,
    child_expr_node_id: &AstNodeId,
    driver: &mut compiler_driver::Driver,
    metadata: &mut parser::AstMetadata,
) {
    let parent_expr_loc = metadata.get_source_location(parent_expr_node_id);
    let child_expr_loc = metadata.get_source_location(child_expr_node_id);
    Warning::mixed_operators_missing_parens(child_expr_op, parent_expr_op, child_expr_loc, parent_expr_loc, driver);
}

fn is_binary_expr_with_op_missing_parens(
    find_op: AstBinaryOp,
    expr: &AstExpression,
    metadata: &mut parser::AstMetadata,
) -> bool {
    if let AstExpression::BinaryOperation { node_id, op, .. } = expr
        && *op == find_op
        && !metadata.expr_has_parens(*node_id)
    {
        return true;
    }

    false
}

fn is_binary_expr_missing_parens(expr: &AstExpression, metadata: &mut parser::AstMetadata) -> Option<AstBinaryOp> {
    if let AstExpression::BinaryOperation { node_id, op, .. } = expr
        && !metadata.expr_has_parens(*node_id)
    {
        return Some(*op);
    }

    None
}
