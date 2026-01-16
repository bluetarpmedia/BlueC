// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `utils` module provides utility functions to assist with type checking.

use super::super::constant_eval;
use super::TypeChecker;

use crate::ICE;
use crate::compiler_driver::Driver;
use crate::compiler_driver::errors::Error;
use crate::compiler_driver::warnings::Warning;
use crate::parser::{
    AstAssignmentOp, AstBinaryOp, AstExpression, AstFullExpression, AstIntegerLiteralKind, AstNodeId,
    AstStaticStorageInitializer, AstType, AstUnaryOp, AstVariableInitializer,
};

/// Takes ownership of the given `Vec` and returns a new `Vec` with all consecutive `ZeroBytes` items merged together.
pub fn combine_consecutive_zero_bytes(values: Vec<AstStaticStorageInitializer>) -> Vec<AstStaticStorageInitializer> {
    values.into_iter().fold(Vec::new(), |mut acc, this_value| {
        let prev_value = acc.last_mut();

        let merged = match (prev_value, &this_value) {
            (
                Some(AstStaticStorageInitializer::ZeroBytes(prev_size)),
                AstStaticStorageInitializer::ZeroBytes(this_size),
            ) => {
                *prev_size += this_size;
                true
            }
            _ => false,
        };

        if !merged {
            acc.push(this_value);
        }

        acc
    })
}

/// Gets a function's return type and parameter types from the given `AstType`.
pub fn extract_fn_return_and_param_types(fn_type: &AstType) -> (&AstType, &Vec<AstType>) {
    let AstType::Function { return_type, params } = fn_type else {
        ICE!("Expected AstType::Function");
    };

    (return_type, params)
}

/// Is the expression a modifiable l-value?
pub fn is_modifiable_lvalue(expr: &AstExpression, expr_type: &AstType) -> bool {
    // To be modifiable, the lvalue must designate an object (not a function or an array),
    // and the object cannot be const (future).
    expr.is_lvalue() && !(expr_type.is_function() || expr_type.is_array())
}

/// Takes ownership of the given boxed `AstExpression` by replacing it with a 'null' value.
pub fn take_boxed_expression(expr: &mut Box<AstExpression>) -> Box<AstExpression> {
    std::mem::replace(
        expr,
        Box::new(AstExpression::IntegerLiteral {
            node_id: AstNodeId::null(),
            literal: String::new(),
            literal_base: 10,
            value: 0,
            kind: AstIntegerLiteralKind::Int,
        }),
    )
}

/// Takes ownership of the given `AstExpression` by replacing it with a 'null' value.
pub fn take_expression(expr: &mut AstExpression) -> AstExpression {
    std::mem::replace(
        expr,
        AstExpression::IntegerLiteral {
            node_id: AstNodeId::null(),
            literal: String::new(),
            literal_base: 10,
            value: 0,
            kind: AstIntegerLiteralKind::Int,
        },
    )
}

/// Takes the first scalar initializer from the given variable initializer.
pub fn take_first_scalar_initializer(initializer: &mut AstVariableInitializer) -> AstVariableInitializer {
    match initializer {
        AstVariableInitializer::Scalar(full_expr) => {
            let expr = take_expression(&mut full_expr.expr);
            AstVariableInitializer::Scalar(AstFullExpression { node_id: full_expr.node_id, expr })
        }
        AstVariableInitializer::Aggregate { init, .. } => take_first_scalar_initializer(&mut init[0]),
    }
}

pub enum CommonTypeError {
    WarnDifferentPointerTypes { a_type: AstType, b_type: AstType },
    WarnPointerAndInteger { a_type: AstType, b_type: AstType },
    NoCommonType { a_type: AstType, b_type: AstType },
}

/// Gets the common `AstType` for the types of the two given expressions.
pub fn get_common_type(
    a: &AstExpression,
    b: &AstExpression,
    chk: &mut TypeChecker,
    driver: &mut Driver,
) -> Result<AstType, CommonTypeError> {
    let a_type = chk.get_data_type(&a.node_id());
    let b_type = chk.get_data_type(&b.node_id());

    // A function type should decay/implicitly convert to a function pointer.
    //
    let a_type = if a_type.is_function() { AstType::new_pointer_to(a_type) } else { a_type };
    let b_type = if b_type.is_function() { AstType::new_pointer_to(b_type) } else { b_type };

    // If either (or both) types are pointers then we handle the types differently, including detecting an
    // expression of integer type that evaluates to a null pointer constant.
    //
    if a_type.is_pointer() || b_type.is_pointer() {
        if a_type == b_type {
            return Ok(a_type);
        }

        // If both types are pointers but not the same (see above), ask the caller to emit a warning.
        if a_type.is_pointer() && b_type.is_pointer() {
            return Err(CommonTypeError::WarnDifferentPointerTypes { a_type, b_type });
        }

        if is_null_pointer_constant(a, &b_type, chk, driver) {
            return Ok(b_type);
        }

        if is_null_pointer_constant(b, &a_type, chk, driver) {
            return Ok(a_type);
        }

        // If one of the types is an integer (and not a null pointer constant) then ask the caller to emit a warning.
        if a_type.is_integer() || b_type.is_integer() {
            return Err(CommonTypeError::WarnPointerAndInteger { a_type, b_type });
        }

        return Err(CommonTypeError::NoCommonType { a_type, b_type });
    }

    // Get the common type and then promote to `int` if the common type is smaller, like _Bool, char or short.
    let common_type = AstType::get_common_type(&a_type, &b_type);
    let (common_type, _) = common_type.promote_if_rank_lower_than_int();
    Ok(common_type)
}

/// Does the given `expr` expression evaluate as a null pointer constant value (zero)?
pub fn is_null_pointer_constant(
    expr: &AstExpression,
    ptr_type: &AstType,
    chk: &mut TypeChecker,
    driver: &mut Driver,
) -> bool {
    match expr {
        AstExpression::IntegerLiteral { value, .. } => *value == 0,

        _ => {
            let mut ctx = constant_eval::ConstantEvalContext::from_type_checker(chk, driver);
            match constant_eval::evaluate_constant_expr(expr, &mut ctx) {
                Some(const_value) if const_value.is_zero() && const_value.get_ast_type().is_integer() => {
                    let loc = chk.metadata.get_source_span_as_loc(&expr.node_id()).unwrap();
                    Warning::expression_interpreted_as_null_ptr_constant(loc, ptr_type, driver);
                    true
                }
                _ => false,
            }
        }
    }
}

/// Emits an error that a binary operation has operands with invalid types.
pub fn error_invalid_binary_expression_operands(
    node_id: &AstNodeId,
    lhs_expr: &AstExpression,
    rhs_expr: &AstExpression,
    lhs_type: &AstType,
    rhs_type: &AstType,
    chk: &mut TypeChecker,
    driver: &mut Driver,
) {
    let op_loc = chk.metadata.get_source_span_as_loc(node_id).unwrap();
    let lhs_loc = chk.metadata.get_source_span_as_loc(&lhs_expr.node_id()).unwrap();
    let rhs_loc = chk.metadata.get_source_span_as_loc(&rhs_expr.node_id()).unwrap();

    Error::invalid_binary_expression_operands(op_loc, lhs_type, rhs_type, lhs_loc, rhs_loc, driver);
}

/// Emits a warning that two different pointer types are being compared.
pub fn warn_compare_different_pointer_types(
    expr_node_id: &AstNodeId,
    left: &AstExpression,
    right: &AstExpression,
    left_type: &AstType,
    right_type: &AstType,
    chk: &mut TypeChecker,
    driver: &mut Driver,
) {
    let loc = chk.metadata.get_source_span_as_loc(expr_node_id).unwrap();
    let a_loc = chk.metadata.get_source_span_as_loc(&left.node_id()).unwrap();
    let b_loc = chk.metadata.get_source_span_as_loc(&right.node_id()).unwrap();
    Warning::compare_different_pointer_types(left_type, right_type, loc, a_loc, b_loc, driver);
}

/// Emits a warning that a pointer and an integer are being compared.
pub fn warn_compare_pointer_and_integer(
    expr_node_id: &AstNodeId,
    left: &AstExpression,
    right: &AstExpression,
    left_type: &AstType,
    right_type: &AstType,
    chk: &mut TypeChecker,
    driver: &mut Driver,
) {
    let cmp_loc = chk.metadata.get_source_span_as_loc(expr_node_id).unwrap();
    let left_loc = chk.metadata.get_source_span_as_loc(&left.node_id()).unwrap();
    let right_loc = chk.metadata.get_source_span_as_loc(&right.node_id()).unwrap();

    let (ptr_type, int_type, ptr_loc, int_loc) = if left_type.is_pointer() {
        (left_type, right_type, left_loc, right_loc)
    } else {
        (right_type, left_type, right_loc, left_loc)
    };

    Warning::compare_pointer_and_integer(ptr_type, int_type, cmp_loc, ptr_loc, int_loc, driver);
}

/// Emits a warning that a ternary condition's consequent and alternative types do not match.
pub fn warn_conditional_type_mismatch(
    expr_node_id: &AstNodeId,
    left: &AstExpression,
    right: &AstExpression,
    left_type: &AstType,
    right_type: &AstType,
    chk: &mut TypeChecker,
    driver: &mut Driver,
) {
    let loc = chk.metadata.get_source_span_as_loc(expr_node_id).unwrap();
    let a_loc = chk.metadata.get_source_span_as_loc(&left.node_id()).unwrap();
    let b_loc = chk.metadata.get_source_span_as_loc(&right.node_id()).unwrap();
    Warning::conditional_type_mismatch(left_type, right_type, loc, a_loc, b_loc, driver);
}

/// Emits a warning that two pointers in an expression have different types.
pub fn warn_pointer_type_mismatch(
    expr_node_id: &AstNodeId,
    left: &AstExpression,
    right: &AstExpression,
    left_type: &AstType,
    right_type: &AstType,
    chk: &mut TypeChecker,
    driver: &mut Driver,
) {
    let loc = chk.metadata.get_source_span_as_loc(expr_node_id).unwrap();
    let a_loc = chk.metadata.get_source_span_as_loc(&left.node_id()).unwrap();
    let b_loc = chk.metadata.get_source_span_as_loc(&right.node_id()).unwrap();
    Warning::pointer_type_mismatch(left_type, right_type, loc, a_loc, b_loc, driver);
}

/// Is the given unary operator incompatible with pointer operands?
pub fn unary_operator_incompatible_with_ptr_operand(op: AstUnaryOp) -> bool {
    matches!(op, AstUnaryOp::Negate | AstUnaryOp::Plus | AstUnaryOp::BitwiseNot)
}

/// Is the given unary operator incompatible with an operand of function type?
pub fn unary_operator_incompatible_with_fn_operand(op: AstUnaryOp) -> bool {
    matches!(
        op,
        AstUnaryOp::Negate
            | AstUnaryOp::Plus
            | AstUnaryOp::BitwiseNot
            | AstUnaryOp::PrefixIncrement
            | AstUnaryOp::PrefixDecrement
            | AstUnaryOp::PostfixIncrement
            | AstUnaryOp::PostfixDecrement
    )
}

/// Is the given binary operator incompatible with a pointer operand?
pub fn binary_operator_incompatible_with_ptr_operand(op: AstBinaryOp) -> bool {
    matches!(
        op,
        AstBinaryOp::Multiply
            | AstBinaryOp::Divide
            | AstBinaryOp::Remainder
            | AstBinaryOp::BitwiseAnd
            | AstBinaryOp::BitwiseOr
            | AstBinaryOp::BitwiseXor
            | AstBinaryOp::LeftShift
            | AstBinaryOp::RightShift
    )
}

/// Is the given assignment operator incompatible with a pointer operand?
pub fn assignment_operator_incompatible_with_ptr_operand(op: AstAssignmentOp) -> bool {
    matches!(
        op,
        AstAssignmentOp::Multiply
            | AstAssignmentOp::Divide
            | AstAssignmentOp::Remainder
            | AstAssignmentOp::BitwiseAnd
            | AstAssignmentOp::BitwiseOr
            | AstAssignmentOp::BitwiseXor
            | AstAssignmentOp::LeftShift
            | AstAssignmentOp::RightShift
    )
}

/// Is the given binary operator incompatible with floating-point operands?
pub fn binary_operator_incompatible_with_fp_operand(op: AstBinaryOp) -> bool {
    matches!(
        op,
        AstBinaryOp::BitwiseAnd
            | AstBinaryOp::BitwiseOr
            | AstBinaryOp::BitwiseXor
            | AstBinaryOp::LeftShift
            | AstBinaryOp::RightShift
            | AstBinaryOp::Remainder
    )
}

/// Is the given assignment operator incompatible with floating-point operands?
pub fn assignment_operator_incompatible_with_fp_operand(op: AstAssignmentOp) -> bool {
    matches!(
        op,
        AstAssignmentOp::BitwiseAnd
            | AstAssignmentOp::BitwiseOr
            | AstAssignmentOp::BitwiseXor
            | AstAssignmentOp::LeftShift
            | AstAssignmentOp::RightShift
            | AstAssignmentOp::Remainder
    )
}
