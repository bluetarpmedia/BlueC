// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `literal_promotion` module provides functionality to find cast expressions wrapping numeric literals and
//! rewrite them to new numeric literals without the need for a cast.
//!
//! For example:
//!
//! ```c
//! long x = 1;              // User code. `1` is an integer literal of type 'int'.
//! long x = (long)1;        // Typechecking adds an implicit cast to 'long'.
//! long x = 1L;             // This sema pass replaces the cast with a new literal of type 'long'.
//! ```

use super::visitor;

use crate::ICE;
use crate::parser;
use crate::parser::{AstExpression, AstFullExpression, AstIntegerLiteralKind, AstType};

/// Rewrites casts of integer literals by promoting the integer literal to the desired type and then removing
/// the cast.
pub fn promote_integer_literals(ast_root: &mut parser::AstRoot, metadata: &mut parser::AstMetadata) {
    // Visit all the full expressions in the AST.
    //
    visitor::visit_full_expressions(ast_root, &mut |full_expr: &mut AstFullExpression| {
        // Visit each sub-expression in the full expression.
        //
        visitor::visit_expressions_in_full_expression(full_expr, &mut |expr: &mut AstExpression| {
            promote_integer_literals_in_expr(expr, metadata);
        });
    });
}

/// Find cast expressions containing an inner expression of an integer literal, and then try and replace the cast
/// with a promoted literal.
pub(super) fn promote_integer_literals_in_expr(expr: &mut AstExpression, metadata: &mut parser::AstMetadata) {
    if let AstExpression::Cast { target_type, expr: inner_expr, .. } = expr
        && matches!(inner_expr.as_ref(), AstExpression::IntegerLiteral { .. })
    {
        // If we can promote the integer literal to the desired type, then replace the cast expression
        // with the new promoted expression.
        if let Some(cast_to_type) = target_type.resolved_type.as_ref()
            && let Some(promoted) = try_promote_integer_literal(inner_expr.as_mut(), cast_to_type, metadata)
        {
            *expr = promoted;
        }
    }
}

fn try_promote_integer_literal(
    expr: &mut AstExpression,
    promote_to_type: &AstType,
    metadata: &mut parser::AstMetadata,
) -> Option<AstExpression> {
    let AstExpression::IntegerLiteral { node_id, literal, literal_base, value, kind } = expr else {
        ICE!("Expected an integer literal expression");
    };

    let data_type = kind.data_type();
    if data_type == *promote_to_type {
        return None;
    }

    if promote_to_type.is_integer() {
        let value_is_zero = *value == 0;

        let promotion_type_can_hold_value =
            promote_to_type.bits() >= data_type.bits() && promote_to_type.can_hold_value(*value);

        if value_is_zero || promotion_type_can_hold_value {
            let literal = literal.clone();
            let literal_base = *literal_base;
            let value = *value;

            metadata.set_node_type(*node_id, promote_to_type.clone());

            let (promote_to_type, _) = promote_to_type.clone().promote_if_rank_lower_than_int();

            let kind = AstIntegerLiteralKind::from(&promote_to_type);

            Some(AstExpression::IntegerLiteral { node_id: *node_id, literal, literal_base, value, kind })
        } else {
            None
        }
    } else if promote_to_type.is_floating_point() && promote_to_type.can_hold_value(*value) {
        let literal = literal.clone();
        let value = *value as f64;

        metadata.set_node_type(*node_id, promote_to_type.clone());

        Some(AstExpression::FloatLiteral {
            node_id: *node_id,
            literal,
            literal_base: 10,
            value,
            kind: parser::AstFloatLiteralKind::from(promote_to_type),
        })
    } else {
        None
    }
}
