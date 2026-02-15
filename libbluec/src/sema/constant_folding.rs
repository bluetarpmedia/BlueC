// Copyright 2025-2026 Neil Henderson
//
//! The `constant_folding` module provides functionality to fold constant expressions in the AST.

mod reassociate;
mod variable_initializer;

use crate::ICE;
use crate::compiler_driver::Driver;
use crate::core::SourceLocation;
use crate::parser::{
    AstConstantFp, AstConstantInteger, AstConstantValue, AstDeclaredType, AstExpression, AstExpressionFlag,
    AstExpressionKind, AstFloatLiteralKind, AstIntegerLiteralKind, AstNodeId, AstRoot, AstType, AstUnaryOp,
    AstVariableDeclaration, AstVariableInitializer,
};

use super::constant_eval;
use super::type_check::TypeChecker;
use super::visitor;

/// Traverses the AST and folds constant expressions.
pub fn fold(ast_root: &mut AstRoot, chk: &mut TypeChecker, driver: &mut Driver) {
    // Visit constant expressions and fold them.
    //
    visitor::visit_full_expressions(ast_root, &mut |full_expr: &mut AstExpression| {
        // If the full expression is an initializer for a static storage variable then we've already evaluated it
        // during type checking, and that may have emitted warning diagnostics. We don't want to duplicate warnings
        // by evaluating it again, and there's no need to actually fold the constant expression because we set the
        // evaluated result on the AST node. (See `AstVariableDeclaration::init_constant_eval`.)
        if chk.metadata.is_expr_flag_set(full_expr.id(), AstExpressionFlag::IsStaticStorageInit) {
            return;
        }

        // If the full expression itself is a constant expression then we can evaluate and fold the whole thing.
        if chk.metadata.is_expr_flag_set(full_expr.id(), AstExpressionFlag::IsConstant) {
            fold_constant_expression(full_expr, chk, driver);
            return;
        }

        // If the expression is a compound assignment expression and its `rhs` is a constant expression then emit
        // a warning if an implicit cast from the rhs to the lhs type would change the rhs value.
        if is_compound_assignment_with_constant_rhs(full_expr, chk) {
            // Evaluate the whole compound assignment expression. This expression is not a constant expression (since
            // it assigns to an l-value), but our constant expression evaluator is smart enough to still validate
            // the `rhs` constant expression and, if necessary, warn about the implicit cast to the lhs type.
            let mut eval = constant_eval::Eval::new(chk, driver);
            _ = eval.evaluate_expr(full_expr); // Ignore the result, we only want the diagnostics.
        }

        // Check if any of the full expression's sub-expressions are constant expressions and fold them.
        visitor::visit_sub_expressions(full_expr, &mut |expr: &mut AstExpression| {
            if chk.metadata.is_expr_flag_set(expr.id(), AstExpressionFlag::IsConstant) {
                fold_constant_expression(expr, chk, driver);
            } else if expr.is_binary_expr() {
                // If the binary expression is associative & commutative (Add/Multiply) then transform its operands
                // to group literals together so that they can be folded. This function modifies `expr` in-place
                // meaning that aftwerwards our visitor will visit its sub-expressions.
                reassociate::reassociate_binary_expr(expr, chk);
            }
        });

        // See `sema::semantic_analysis` where a subsequent pass analyzes sub-expressions to look for invalid
        // operations, e.g. divide by constant zero or shift by a negative constant value.
    });

    // Visit aggregate initializers and recurse into them to find constant initializers of character array types
    // so that we can fold them into string literals.
    //
    visitor::visit_variable_declarations(ast_root, &mut |var_decl: &mut AstVariableDeclaration| {
        if let Some(var_init) = var_decl.initializer.as_mut()
            && var_init.is_aggregate()
            && !chk.metadata.is_expr_flag_set(var_init.node_id(), AstExpressionFlag::IsStaticStorageInit)
        {
            visit_aggregate_variable_initializer(var_init, chk)
        }
    });
}

fn visit_aggregate_variable_initializer(var_init: &mut AstVariableInitializer, chk: &mut TypeChecker) {
    let AstVariableInitializer::Aggregate { node_id, init } = var_init else {
        return;
    };

    // If this initializer is an aggregate initializer for a character array type then fold it into a string literal.
    //      {'a', 'b', 'c', 'd'}   -->  "abcd"
    //
    let aggregate_type = chk.metadata.get_node_type(*node_id);
    if chk.metadata.is_expr_flag_set(*node_id, AstExpressionFlag::IsConstant) && aggregate_type.is_character_array() {
        *var_init = variable_initializer::fold_character_array_variable_initializer(init, aggregate_type.clone(), chk);
        return;
    }

    // Otherwise recurse into the initializer to check if there are nested aggregate initializers.
    //      { {'t', 'e', 's', 't'}, {'f', 'o', 'u', 'r'} }
    //
    for ini in init {
        if ini.is_aggregate() {
            visit_aggregate_variable_initializer(ini, chk);
        }
    }
}

/// Folds a constant expression.
fn fold_constant_expression(expr: &mut AstExpression, chk: &mut TypeChecker, driver: &mut Driver) {
    // If the expression is a literal then there's nothing further to evaluate or fold.
    if expr.is_literal() {
        return;
    }

    // Don't try and fold an implicit cast of a single-char CharLiteral (which has type 'int') to a character type,
    // since this would just be wasted effort, because we'd end up recreating the CharLiteral and the implicit cast.
    if let AstExpressionKind::Cast { target_type, inner, is_implicit } = expr.kind()
        && *is_implicit
        && target_type.is_resolved()
        && target_type.resolved_type.as_ref().unwrap().is_character()
        && inner.is_single_char_literal()
    {
        return;
    }

    let mut eval = constant_eval::Eval::new(chk, driver);
    let Some(constant_value) = eval.evaluate_expr(expr) else {
        return;
    };

    if !constant_value.is_pointer_address_constant() {
        let sloc = chk.metadata.get_source_location(expr.id()); // Sloc of expression before folding

        let data_type = chk.metadata.get_node_type(expr.id()).clone();
        *expr = make_literal(&constant_value, data_type, chk);

        chk.metadata.add_source_location(expr.id(), sloc); // Set sloc on the new literal that replaced the expr
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

    let is_multichar = false;
    let char_literal = AstExpression::new(node_id, AstExpressionKind::CharLiteral { literal, is_multichar, value });

    let char_literal = if is_negative {
        let node_id = make_constant_expr_node_id(AstType::Int, chk);
        AstExpression::new(
            node_id,
            AstExpressionKind::Unary { op: AstUnaryOp::Negate, operand: Box::new(char_literal) },
        )
    } else {
        char_literal
    };

    // Cast the literal to the destination type
    if data_type == AstType::Int {
        char_literal
    } else {
        let node_id = make_constant_expr_node_id(data_type.clone(), chk);

        AstExpression::new(
            node_id,
            AstExpressionKind::Cast {
                target_type: AstDeclaredType::resolved(&data_type),
                inner: Box::new(char_literal),
                is_implicit: true,
            },
        )
    }
}

fn make_short_literal(value: i32, data_type: AstType, chk: &mut TypeChecker) -> AstExpression {
    let is_negative = value < 0;

    let node_id = make_constant_expr_node_id(AstType::Int, chk); // Integer literal has no 'short' type
    let value = value.unsigned_abs();
    let literal = value.to_string();
    let kind = AstIntegerLiteralKind::Int;

    let int_literal = AstExpression::new(
        node_id,
        AstExpressionKind::IntegerLiteral { literal, literal_base: 10, value: value as u64, kind },
    );

    let int_literal = if is_negative {
        let node_id = make_constant_expr_node_id(AstType::Int, chk);
        AstExpression::new(node_id, AstExpressionKind::Unary { op: AstUnaryOp::Negate, operand: Box::new(int_literal) })
    } else {
        int_literal
    };

    // Cast the literal to the destination type
    if data_type == AstType::Int {
        int_literal
    } else {
        let node_id = make_constant_expr_node_id(data_type.clone(), chk);
        AstExpression::new(
            node_id,
            AstExpressionKind::Cast {
                target_type: AstDeclaredType::resolved(&data_type),
                inner: Box::new(int_literal),
                is_implicit: true,
            },
        )
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
        let value = value.unsigned_abs();
        (value, value.to_string())
    };

    let int_literal =
        AstExpression::new(node_id, AstExpressionKind::IntegerLiteral { literal, literal_base: 10, value, kind });

    if is_negative {
        let node_id = make_constant_expr_node_id(data_type, chk);
        AstExpression::new(node_id, AstExpressionKind::Unary { op: AstUnaryOp::Negate, operand: Box::new(int_literal) })
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
    AstExpression::new(node_id, AstExpressionKind::IntegerLiteral { literal, literal_base: 10, value, kind })
}

fn make_fp_literal(value: f64, kind: AstFloatLiteralKind, data_type: AstType, chk: &mut TypeChecker) -> AstExpression {
    let node_id = make_constant_expr_node_id(data_type, chk);
    let literal = value.to_string();
    AstExpression::new(node_id, AstExpressionKind::FloatLiteral { literal, literal_base: 10, value, kind })
}

fn is_compound_assignment_with_constant_rhs(expr: &AstExpression, chk: &TypeChecker) -> bool {
    if let AstExpressionKind::Assignment { op, rhs, .. } = expr.kind()
        && op.is_compound_assignment()
        && chk.metadata.is_expr_flag_set(rhs.id(), AstExpressionFlag::IsConstant)
    {
        true
    } else {
        false
    }
}

fn make_constant_expr_node_id(data_type: AstType, chk: &mut TypeChecker) -> AstNodeId {
    let node_id = AstNodeId::new();
    chk.metadata.set_expr_flag(node_id, AstExpressionFlag::IsConstant);
    chk.metadata.set_node_type(node_id, data_type);
    chk.metadata.add_source_location(node_id, SourceLocation::none());
    node_id
}
