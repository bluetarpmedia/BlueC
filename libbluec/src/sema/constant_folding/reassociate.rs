// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `reassociate` module provides functionality to reassociate literal terms in a binary expression in order
//! to rearrange the tree for constant folding.

use crate::parser::{AstBinaryOp, AstExpression, AstNodeId};

use super::super::type_check::TypeChecker;

/// Reassociates and normalizes associative/commutative binary expression trees of integer type.
///
/// Chains of the same operator (Add/Multiply) are rearranged to group/associate all constant literals together.
/// This allows the constant folder to subsequently evaluate and fold all of the literals.
///
/// E.g. an expression like `a + 1 + b + 2 + c + 3` is transformed into `(a + b + c) + (1 + 2 + 3)`.
///
/// Note: This function does not recurse into sub-expressions of different operator types, as it assumes it is called
/// by a visitor which visits every sub-expression in the full expression tree.
pub fn reassociate_binary_expr(expr: &mut AstExpression, chk: &mut TypeChecker) {
    let AstExpression::BinaryOperation { node_id: p_node_id, op: current_op, .. } = expr else {
        return;
    };

    let is_integral_commutative_associative =
        current_op.is_commutative_and_associative() && chk.get_data_type(*p_node_id).is_integer();

    // Our caller is the visitor which visits every sub-expression in the full expression, so we don't need to
    // recurse into all binary expressions here, since we'll get called again by the visitor.
    if !is_integral_commutative_associative {
        return;
    }

    let current_op = *current_op;

    // Take ownership of the given `expr` by replacing it with a dummy/null value (which will get dropped).
    let null_expr = AstExpression::new_int_literal(0);
    let owned_expr = std::mem::replace(expr, null_expr);

    // Flatten the terms in the expression tree if they have the same operator.
    let mut terms = Vec::new();
    let mut op_node_ids = Vec::new();
    flatten(owned_expr, current_op, &mut terms, &mut op_node_ids);

    // Reassociate each term individually
    for term in terms.iter_mut() {
        reassociate_binary_expr(term, chk);
    }

    // Partion literals and non-literal expressions
    let (literals, other): (Vec<_>, Vec<_>) = terms.into_iter().partition(|t| t.is_literal());

    // Can't both be empty
    debug_assert!(!(literals.is_empty() && other.is_empty()));

    // Rebuild the tree with literals on one side and everything else on the other.
    //      This allows our constant folder to subsequently fold all the literals.
    //
    if literals.is_empty() {
        *expr = rebuild_tree(other, current_op, &mut op_node_ids, chk);
    } else if other.is_empty() {
        *expr = rebuild_tree(literals, current_op, &mut op_node_ids, chk);
    } else {
        let lit_tree = rebuild_tree(literals, current_op, &mut op_node_ids, chk);
        let other_tree = rebuild_tree(other, current_op, &mut op_node_ids, chk);

        *expr = rebuild_tree(vec![other_tree, lit_tree], current_op, &mut op_node_ids, chk);
    }
}

/// Unpacks a binary expressions with associative operations in an expression tree into a flat list of terms.
///
/// This is an iterative implementation to avoid potential stack overflow with huge expressions.
fn flatten(expr: AstExpression, target_op: AstBinaryOp, terms: &mut Vec<AstExpression>, ops: &mut Vec<AstNodeId>) {
    let mut stack = vec![expr];

    while let Some(current_expr) = stack.pop() {
        if let AstExpression::BinaryOperation { node_id, op, lhs, rhs } = current_expr {
            if op == target_op {
                ops.push(node_id);

                // Push children onto the stack to be later popped by the outer loop.
                //      Push rhs first so that lhs is popped first; this preserves the original order in `terms`.
                stack.push(*rhs);
                stack.push(*lhs);
            } else {
                // This binary expression has a different operator, so treat it as a single term, rather than
                // trying to flatten it.
                terms.push(AstExpression::BinaryOperation { node_id, op, lhs, rhs });
            }
        } else {
            terms.push(current_expr);
        }
    }
}

/// Reconstructs a left-leaning binary expression tree from a list of terms.
///
/// This is an iterative implementation to avoid potential stack overflow with huge expressions.
fn rebuild_tree(
    terms: Vec<AstExpression>,
    op: AstBinaryOp,
    op_ids: &mut Vec<AstNodeId>,
    chk: &mut TypeChecker,
) -> AstExpression {
    assert!(!terms.is_empty());

    let mut term_iter = terms.into_iter();
    let mut current_expr = term_iter.next().unwrap(); // Asserted not empty above

    // Iterate over any remaining terms and attach them to the rhs
    for next_term in term_iter {
        let node_id = op_ids.pop().expect("Should have enough IDs");

        let lhs = current_expr;
        let rhs = next_term;

        chk.metadata.propagate_const_flag_from_children(&[lhs.node_id(), rhs.node_id()], node_id);

        current_expr = AstExpression::BinaryOperation { node_id, op, lhs: Box::new(lhs), rhs: Box::new(rhs) };
    }

    current_expr
}
