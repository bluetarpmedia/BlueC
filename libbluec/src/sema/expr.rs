// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `expr` module provides semantic analysis functionality for expressions.

use crate::compiler_driver::{Driver, Warning};
use crate::core::SourceLocation;
use crate::parser::{
    AstBinaryOp, AstBinaryOpFamily, AstExpression, AstExpressionFlag, AstFullExpression, AstFunction, AstMetadata,
    AstNodeId, AstRoot, AstStatement, AstType, AstUnaryOp,
};

use super::visitor;

/// Emit warnings about non-constant expressions with implicit arithmetic conversions.
///
/// The equivalent for constant expressions is handled by `constant_folding`.
pub fn warn_about_implicit_arithmetic_conversions(
    ast_root: &mut AstRoot,
    metadata: &mut AstMetadata,
    driver: &mut Driver,
) {
    let is_constant_expr =
        |node_id: AstNodeId| -> bool { metadata.is_expr_flag_set(node_id, AstExpressionFlag::IsConstant) };

    let emit_warning = |from_node_id: AstNodeId,
                        from_type: &AstType,
                        to_type: &AstType,
                        metadata: &AstMetadata,
                        driver: &mut Driver| {
        let is_int_to_float = from_type.is_integer() && to_type.is_floating_point();
        let is_bigger_int_to_smaller_fp = is_int_to_float && to_type.bits() < from_type.bits();
        let is_arithmetic_cast = from_type.is_arithmetic() && to_type.is_arithmetic() && !is_int_to_float;

        if is_bigger_int_to_smaller_fp || (is_arithmetic_cast && !from_type.fits_inside(to_type)) {
            let loc = metadata.get_source_location(&from_node_id);
            if metadata.is_expr_flag_set(from_node_id, AstExpressionFlag::PromotedToInt) {
                Warning::implicit_int_promotion_conversion(from_type, to_type, loc, driver);
            } else {
                Warning::implicit_arithmetic_conversion(from_type, to_type, loc, driver);
            }
        }
    };

    visitor::visit_full_expressions(ast_root, &mut |full_expr: &mut AstFullExpression| {
        visitor::visit_expressions_in_full_expression(full_expr, &mut |expr: &mut AstExpression| match expr {
            // Compound assignment where the rhs was promoted to 'int' but is then cast to the lhs. E.g.
            //      char ch = 'a';
            //      int  i  = 1;
            //      ch += i;
            //
            AstExpression::Assignment { op, lhs, rhs, .. }
                if op.is_compound_assignment() && !is_constant_expr(rhs.node_id()) =>
            {
                let lhs_type = metadata.get_node_type(&lhs.node_id());
                let rhs_type = metadata.get_node_type(&rhs.node_id());

                emit_warning(rhs.node_id(), rhs_type, lhs_type, metadata, driver);
            }

            // Implicit casts of arithmetic types
            //
            AstExpression::Cast { target_type, expr, is_implicit, .. }
                if *is_implicit && !is_constant_expr(expr.node_id()) =>
            {
                let expr_type = metadata.get_node_type(&expr.node_id());

                debug_assert!(target_type.is_resolved());
                let target_type = target_type.resolved_type.as_ref().unwrap();

                emit_warning(expr.node_id(), expr_type, target_type, metadata, driver);
            }

            _ => (),
        });
    });
}

/// Emit warnings about binary and compound assignment expressions with an invalid constant value operand.
///
/// E.g. divide by zero, shift by negative.
pub fn warn_about_expressions_with_invalid_constant_operands(
    ast_root: &mut AstRoot,
    metadata: &mut AstMetadata,
    driver: &mut Driver,
) {
    visitor::visit_full_expressions(ast_root, &mut |full_expr: &mut AstFullExpression| {
        visitor::visit_expressions_in_full_expression(full_expr, &mut |expr: &mut AstExpression| match expr {
            // Cannot divide by zero in '/' or '%' binary expression.
            AstExpression::BinaryOperation { node_id, op, rhs, .. }
                if op.is_div_or_rem() && rhs.is_integer_literal_with_value(0) =>
            {
                let op_loc = metadata.get_operator_sloc(node_id);
                warn_div_by_zero(op, op_loc, rhs.node_id(), metadata, driver);
            }

            // Cannot divide by zero in '/=' or '%=' compound assignment expression.
            AstExpression::Assignment { node_id, op, rhs, .. }
                if op.is_div_or_rem() && rhs.is_integer_literal_with_value(0) =>
            {
                let op_loc = metadata.get_source_location(node_id);
                warn_div_by_zero(&AstBinaryOp::try_from(*op).unwrap(), op_loc, rhs.node_id(), metadata, driver);
            }

            // Cannot shift by a negative constant value.
            // Cannot shift by a value >= bit width of the type.
            // Shift count of zero is a no-op.
            //
            AstExpression::BinaryOperation { node_id, op, lhs, rhs, .. } if op.is_shift() => {
                let promoted_to_int = metadata.is_expr_flag_set(*node_id, AstExpressionFlag::PromotedToInt);
                validate_shift(lhs.node_id(), rhs, promoted_to_int, metadata, driver);
            }

            // '<<=' or '>>='
            AstExpression::Assignment { op, rhs, computation_node_id, .. } if op.is_shift() => {
                let promoted_to_int = metadata.is_expr_flag_set(rhs.node_id(), AstExpressionFlag::PromotedToInt);
                validate_shift(*computation_node_id, rhs, promoted_to_int, metadata, driver);
            }

            _ => (),
        });
    });
}

/// Emit warnings about unused expression results, i.e. expression statements that have no side-effects.
pub fn warn_about_unused_expression_results(ast_root: &mut AstRoot, metadata: &mut AstMetadata, driver: &mut Driver) {
    let has_side_effects = |_expr: &AstExpression| -> bool {
        // Future: if `expr` is volatile, return true
        false
    };

    visitor::visit_functions(ast_root, &mut |func: &mut AstFunction| {
        let block = func.body.as_mut().unwrap();
        visitor::visit_statements_in_block(block, &mut |stmt: &mut AstStatement| {
            let AstStatement::Expression(full_expr) = stmt else {
                return;
            };

            match &full_expr.expr {
                // A literal as the root of the expression statement is unused.
                AstExpression::IntegerLiteral { node_id, .. } => {
                    let loc = metadata.get_source_location(node_id);
                    Warning::unused_value(loc, driver);
                }

                AstExpression::FloatLiteral { node_id, .. } => {
                    let loc = metadata.get_source_location(node_id);
                    Warning::unused_value(loc, driver);
                }

                // An identifier as the root of the expression statement is unused, unless the variable was declared
                // volatile (though we don't support that yet).
                //
                AstExpression::Identifier { node_id, .. } if !has_side_effects(&full_expr.expr) => {
                    let loc = metadata.get_source_location(node_id);
                    Warning::unused_value(loc, driver);
                }

                // Depending on the unary operator, a unary expression as the root in an expression statement may
                // mean its result is unused.
                AstExpression::UnaryOperation { node_id, op, .. } if !op.has_side_effects() => {
                    let loc = metadata.get_source_location(node_id);
                    Warning::unused_value(loc, driver);
                }

                // We treat AddressOf and Dereference as separate expressions but they're like UnaryOperation and
                // their operators have no side-effects.
                AstExpression::AddressOf { node_id, .. } => {
                    let loc = metadata.get_source_location(node_id);
                    Warning::unused_value(loc, driver);
                }

                AstExpression::Deref { node_id, .. } => {
                    let loc = metadata.get_source_location(node_id);
                    Warning::unused_value(loc, driver);
                }

                // Regardless of the binary operator, a binary expression as the root in an expression statement
                // means its result is unused. (We have AstExpression::Assignment for compound assignments.)
                AstExpression::BinaryOperation { node_id, op, .. } => {
                    let loc = metadata.get_source_location(node_id);
                    if op.family() == AstBinaryOpFamily::Relational {
                        Warning::unused_comparison(*op, loc, driver);
                    } else {
                        Warning::unused_value(loc, driver);
                    }
                }

                _ => (),
            }
        });
    });
}

/// Emit warnings about expressions with mixed logical or bitwise operators and missing parentheses.
pub fn warn_about_expressions_with_mixed_operators(
    ast_root: &mut AstRoot,
    metadata: &mut AstMetadata,
    driver: &mut Driver,
) {
    visitor::visit_full_expressions(ast_root, &mut |full_expr: &mut AstFullExpression| {
        visitor::visit_expressions_in_full_expression(full_expr, &mut |expr: &mut AstExpression| {
            if let AstExpression::BinaryOperation { node_id, op, lhs, rhs } = expr {
                match op {
                    // Mixed logical || and &&
                    //
                    AstBinaryOp::LogicalOr => {
                        let child_op = AstBinaryOp::LogicalAnd;
                        if is_binary_expr_with_op_missing_parens(child_op, lhs, metadata) {
                            emit_missing_parens_warning(*op, child_op, node_id, &lhs.node_id(), metadata, driver);
                        }

                        if is_binary_expr_with_op_missing_parens(child_op, rhs, metadata) {
                            emit_missing_parens_warning(*op, child_op, node_id, &rhs.node_id(), metadata, driver);
                        }
                    }
                    // Mixed bitwise operators (with any other kind of operator)
                    //
                    op if op.family() == AstBinaryOpFamily::Bitwise => {
                        if let Some(child_op) = is_binary_expr_missing_parens(lhs, metadata) {
                            emit_missing_parens_warning(*op, child_op, node_id, &lhs.node_id(), metadata, driver);
                        }

                        if let Some(child_op) = is_binary_expr_missing_parens(rhs, metadata) {
                            emit_missing_parens_warning(*op, child_op, node_id, &rhs.node_id(), metadata, driver);
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
    ast_root: &mut AstRoot,
    metadata: &mut AstMetadata,
    driver: &mut Driver,
) {
    visitor::visit_functions(ast_root, &mut |func: &mut AstFunction| {
        let block = func.body.as_mut().unwrap();
        visitor::visit_statements_in_block(block, &mut |stmt: &mut AstStatement| match stmt {
            AstStatement::If { controlling_expr, .. } if is_assignment_without_parens(controlling_expr, metadata) => {
                emit_assignment_used_in_condition_without_parens_warning(
                    &controlling_expr.node_id,
                    &controlling_expr.expr.node_id(),
                    metadata,
                    driver,
                );
            }
            AstStatement::While { controlling_expr, .. }
                if is_assignment_without_parens(controlling_expr, metadata) =>
            {
                emit_assignment_used_in_condition_without_parens_warning(
                    &controlling_expr.node_id,
                    &controlling_expr.expr.node_id(),
                    metadata,
                    driver,
                );
            }
            AstStatement::DoWhile { controlling_expr, .. }
                if is_assignment_without_parens(controlling_expr, metadata) =>
            {
                emit_assignment_used_in_condition_without_parens_warning(
                    &controlling_expr.node_id,
                    &controlling_expr.expr.node_id(),
                    metadata,
                    driver,
                );
            }
            AstStatement::For { controlling_expr, .. }
                if controlling_expr.as_ref().is_some_and(|expr| is_assignment_without_parens(expr, metadata)) =>
            {
                let controlling_expr = controlling_expr.as_ref().unwrap();
                emit_assignment_used_in_condition_without_parens_warning(
                    &controlling_expr.node_id,
                    &controlling_expr.expr.node_id(),
                    metadata,
                    driver,
                );
            }
            _ => (),
        });
    });
}

fn validate_shift(
    lhs_node_id: AstNodeId,
    rhs: &AstExpression,
    promoted_to_int: bool,
    metadata: &mut AstMetadata,
    driver: &mut Driver,
) {
    let loc = metadata.get_source_location(&rhs.node_id());

    if is_negative_integer_literal(rhs) {
        Warning::shift_count_negative(loc, driver);
    } else if rhs.is_integer_literal_with_value(0) {
        Warning::shift_count_zero(loc, driver);
    } else if let Some(lhs_type) = metadata.try_get_node_type(&lhs_node_id) {
        let lhs_type_bit_count = lhs_type.bits();

        if let AstExpression::IntegerLiteral { value, .. } = rhs
            && *value as usize >= lhs_type_bit_count
        {
            Warning::shift_count_overflow(lhs_type_bit_count, promoted_to_int, loc, driver);
        }
    }
}

fn is_assignment_without_parens(full_expr: &AstFullExpression, metadata: &mut AstMetadata) -> bool {
    if let AstExpression::Assignment { node_id, .. } = &full_expr.expr {
        return !metadata.is_expr_flag_set(*node_id, AstExpressionFlag::HasParens);
    }

    false
}

fn emit_assignment_used_in_condition_without_parens_warning(
    full_expr_node_id: &AstNodeId,
    assignment_node_id: &AstNodeId,
    metadata: &mut AstMetadata,
    driver: &mut Driver,
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
    metadata: &mut AstMetadata,
    driver: &mut Driver,
) {
    let parent_op_loc = metadata.get_operator_sloc(parent_expr_node_id);
    let child_op_loc = metadata.get_operator_sloc(child_expr_node_id);
    Warning::mixed_operators_missing_parens(child_expr_op, parent_expr_op, child_op_loc, parent_op_loc, driver);
}

fn warn_div_by_zero(
    op: &AstBinaryOp,
    op_loc: SourceLocation,
    rhs_node_id: AstNodeId,
    metadata: &mut AstMetadata,
    driver: &mut Driver,
) {
    let zero_expr_loc = metadata.get_source_location(&rhs_node_id);
    Warning::division_by_zero(op, op_loc, zero_expr_loc, driver);
}

fn is_binary_expr_with_op_missing_parens(
    find_op: AstBinaryOp,
    expr: &AstExpression,
    metadata: &mut AstMetadata,
) -> bool {
    if let AstExpression::BinaryOperation { node_id, op, .. } = expr
        && *op == find_op
        && !metadata.is_expr_flag_set(*node_id, AstExpressionFlag::HasParens)
    {
        return true;
    }

    false
}

fn is_binary_expr_missing_parens(expr: &AstExpression, metadata: &mut AstMetadata) -> Option<AstBinaryOp> {
    if let AstExpression::BinaryOperation { node_id, op, .. } = expr
        && !metadata.is_expr_flag_set(*node_id, AstExpressionFlag::HasParens)
    {
        return Some(*op);
    }

    None
}

fn is_negative_integer_literal(expr: &AstExpression) -> bool {
    if let AstExpression::UnaryOperation { op, expr, .. } = expr
        && *op == AstUnaryOp::Negate
        && expr.is_integer_literal()
    {
        true
    } else {
        false
    }
}
