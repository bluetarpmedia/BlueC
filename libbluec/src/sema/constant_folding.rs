// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `constant_folding` module provides functionality to fold constant expressions in the AST.

mod reassociate;
mod variable_initializer;

use crate::ICE;
use crate::compiler_driver::Driver;
use crate::parser::{
    AstConstantFp, AstConstantInteger, AstConstantValue, AstDeclaredType, AstExpression, AstExpressionFlag,
    AstFloatLiteralKind, AstFullExpression, AstIntegerLiteralKind, AstNodeId, AstRoot, AstType, AstUnaryOp,
    AstVariableDeclaration, AstVariableInitializer,
};

use super::constant_eval;
use super::type_check::TypeChecker;
use super::visitor;

// TODO: Warnings

/// Traverses the AST and folds constant expressions.
pub fn fold(ast_root: &mut AstRoot, chk: &mut TypeChecker, driver: &mut Driver) {
    // Visit constant expressions and fold them.
    //
    visitor::visit_full_expressions(ast_root, &mut |full_expr: &mut AstFullExpression| {
        // If the full expression itself is a constant expression then we can evaluate and fold it.
        if chk.metadata.is_expr_flag_set(full_expr.node_id, AstExpressionFlag::IsConstant) {
            fold_constant_expression(&mut full_expr.expr, chk, driver);
        }
        // Otherwise check if any of its sub-expressions are constant expressions and fold them.
        else {
            visitor::visit_expressions_in_full_expression(full_expr, &mut |expr: &mut AstExpression| {
                if chk.metadata.is_expr_flag_set(expr.node_id(), AstExpressionFlag::IsConstant) {
                    fold_constant_expression(expr, chk, driver);
                } else if expr.is_binary_expr() {
                    // If the binary expression is associative & commutative (Add/Multiply) then transform its operands
                    // to group literals together so that they can be folded. This function modifies `expr` in-place
                    // meaning that aftwerwards our visitor will visit its sub-expressions.
                    reassociate::reassociate_binary_expr(expr, chk);
                }
            });
        }
    });

    // Visit constant variable initializers for character arrays and fold them into string literals.
    //
    visitor::visit_variable_declarations(ast_root, &mut |var_decl: &mut AstVariableDeclaration| {
        if let Some(var_init) = var_decl.initializer.as_mut()
            && let AstVariableInitializer::Aggregate { .. } = var_init
        {
            fold_aggregate_variable_initializer(var_init, chk)
        }
    });
}

fn fold_aggregate_variable_initializer(var_init: &mut AstVariableInitializer, chk: &mut TypeChecker) {
    let AstVariableInitializer::Aggregate { node_id, init } = var_init else {
        return;
    };

    // If this initializer is an aggregate initializer for a character array type then fold it into a string literal.
    //      {'a', 'b', 'c', 'd'}   -->  "abcd"
    //
    let aggregate_type = chk.metadata.get_node_type(node_id);
    if chk.metadata.is_expr_flag_set(*node_id, AstExpressionFlag::IsConstant) && aggregate_type.is_character_array() {
        *var_init = variable_initializer::fold_character_array_variable_initializer(init, aggregate_type.clone(), chk);
        return;
    }

    // Otherwise recurse into the initializer to check if there are nested aggregate initializers.
    //      { {'t', 'e', 's', 't'}, {'f', 'o', 'u', 'r'} }
    //
    for ini in init {
        fold_aggregate_variable_initializer(ini, chk);
    }
}

/// Folds a constant expression.
fn fold_constant_expression(expr: &mut AstExpression, chk: &mut TypeChecker, driver: &mut Driver) {
    // If the expression is a literal then there's nothing further to evaluate or fold.
    if expr.is_literal() {
        return;
    }

    let Some(constant_value) = constant_eval::evaluate_constant_expr(expr, chk, driver) else {
        return;
    };

    if !constant_value.is_pointer_address_constant() {
        let data_type = chk.metadata.get_node_type(&expr.node_id()).clone();
        *expr = make_literal(&constant_value, data_type, chk);
    }
}

/// Creates a literal expression with the given constant value for the given data type.
fn make_literal(constant_value: &AstConstantValue, data_type: AstType, chk: &mut TypeChecker) -> AstExpression {
    match constant_value {
        AstConstantValue::Integer(constant_int) => match constant_int {
            AstConstantInteger::Char(value) => make_char_literal(*value as i32, data_type, chk),
            AstConstantInteger::UnsignedChar(value) => make_char_literal(*value as i32, data_type, chk),

            AstConstantInteger::Short(value) => make_short_literal(*value as i32, data_type, chk),
            AstConstantInteger::UnsignedShort(value) => make_short_literal(*value as i32, data_type, chk),

            AstConstantInteger::Int(value) => {
                make_signed_integer_literal(*value as i64, AstIntegerLiteralKind::Int, data_type, chk)
            }
            AstConstantInteger::LongLong(value) => {
                make_signed_integer_literal(*value, AstIntegerLiteralKind::LongLong, data_type, chk)
            }
            AstConstantInteger::UnsignedInt(value) => {
                make_unsigned_integer_literal(*value as u64, AstIntegerLiteralKind::UnsignedInt, data_type, chk)
            }
            AstConstantInteger::UnsignedLongLong(value) => {
                make_unsigned_integer_literal(*value, AstIntegerLiteralKind::UnsignedLongLong, data_type, chk)
            }
        },

        AstConstantValue::Fp(constant_fp) => match constant_fp {
            AstConstantFp::Float(value) => make_fp_literal(*value as f64, AstFloatLiteralKind::Float, data_type, chk),
            AstConstantFp::Double(value) => make_fp_literal(*value, AstFloatLiteralKind::Double, data_type, chk),
        },

        AstConstantValue::String { .. } => ICE!("Unexpected AstConstantValue::String constant value"),
        AstConstantValue::Pointer(..) => ICE!("Unexpected AstConstantValue::Pointer constant value"),
    }
}

fn make_char_literal(value: i32, data_type: AstType, chk: &mut TypeChecker) -> AstExpression {
    let is_negative = value < 0;

    let node_id = make_constant_expr_node_id(AstType::Int, chk); // Char literal has type 'int'
    let value = value.abs();
    let literal = value.to_string();

    let char_literal = AstExpression::CharLiteral { node_id, literal, value };

    let char_literal = if is_negative {
        let node_id = make_constant_expr_node_id(AstType::Int, chk);
        AstExpression::UnaryOperation { node_id, op: AstUnaryOp::Negate, expr: Box::new(char_literal) }
    } else {
        char_literal
    };

    // Cast the literal to the destination type
    if data_type == AstType::Int {
        char_literal
    } else {
        let node_id = make_constant_expr_node_id(data_type.clone(), chk);

        AstExpression::Cast {
            node_id,
            target_type: AstDeclaredType::resolved(&data_type),
            expr: Box::new(char_literal),
        }
    }
}

fn make_short_literal(value: i32, data_type: AstType, chk: &mut TypeChecker) -> AstExpression {
    let is_negative = value < 0;

    let node_id = make_constant_expr_node_id(AstType::Int, chk); // Integer literal has no 'short' type
    let value = value.abs();
    let literal = value.to_string();
    let kind = AstIntegerLiteralKind::Int;

    let int_literal = AstExpression::IntegerLiteral { node_id, literal, literal_base: 10, value: value as u64, kind };

    let int_literal = if is_negative {
        let node_id = make_constant_expr_node_id(AstType::Int, chk);
        AstExpression::UnaryOperation { node_id, op: AstUnaryOp::Negate, expr: Box::new(int_literal) }
    } else {
        int_literal
    };

    // Cast the literal to the destination type
    if data_type == AstType::Int {
        int_literal
    } else {
        let node_id = make_constant_expr_node_id(data_type.clone(), chk);
        AstExpression::Cast { node_id, target_type: AstDeclaredType::resolved(&data_type), expr: Box::new(int_literal) }
    }
}

fn make_signed_integer_literal(
    value: i64,
    kind: AstIntegerLiteralKind,
    data_type: AstType,
    chk: &mut TypeChecker,
) -> AstExpression {
    let is_negative = value < 0;

    let node_id = make_constant_expr_node_id(data_type.clone(), chk);

    let (value, literal) = if value == i64::MIN {
        (9223372036854775808_u64, "9223372036854775808".to_string())
    } else {
        (value.unsigned_abs(), value.to_string())
    };

    let int_literal = AstExpression::IntegerLiteral { node_id, literal, literal_base: 10, value, kind };

    if is_negative {
        let node_id = make_constant_expr_node_id(data_type, chk);
        AstExpression::UnaryOperation { node_id, op: AstUnaryOp::Negate, expr: Box::new(int_literal) }
    } else {
        int_literal
    }
}

fn make_unsigned_integer_literal(
    value: u64,
    kind: AstIntegerLiteralKind,
    data_type: AstType,
    chk: &mut TypeChecker,
) -> AstExpression {
    let node_id = make_constant_expr_node_id(data_type, chk);
    let literal = value.to_string();
    AstExpression::IntegerLiteral { node_id, literal, literal_base: 10, value, kind }
}

fn make_fp_literal(value: f64, kind: AstFloatLiteralKind, data_type: AstType, chk: &mut TypeChecker) -> AstExpression {
    let node_id = make_constant_expr_node_id(data_type, chk);
    let literal = value.to_string();
    AstExpression::FloatLiteral { node_id, literal, literal_base: 10, value, kind }
}

fn make_constant_expr_node_id(data_type: AstType, chk: &mut TypeChecker) -> AstNodeId {
    let node_id = AstNodeId::new();
    chk.metadata.set_expr_flag(node_id, AstExpressionFlag::IsConstant);
    chk.metadata.set_node_type(node_id, data_type);
    node_id
}
