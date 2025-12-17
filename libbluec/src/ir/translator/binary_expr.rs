// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `binary_epxr` module defines functions to translate AST binary expressions into the BlueTac IR.

use super::super::{BtBinaryOp, BtConstantValue, BtInstruction, BtUnaryOp, BtValue};
use super::expr;
use super::{BlueTacTranslator, EvalExpr};

use crate::ICE;
use crate::parser::{AstBinaryOp, AstExpression, AstType};

/// Translates a binary expression into BlueTac IR.
pub fn translate_binary_operation(
    translator: &mut BlueTacTranslator,
    expr: &AstExpression,
    instructions: &mut Vec<BtInstruction>,
) -> EvalExpr {
    let AstExpression::BinaryOperation { node_id, op: ast_op, left: left_expr, right: right_expr } = expr else {
        ICE!("Expected an AstExpression::BinaryOperation");
    };

    // Translate pointer arithmetic separately from other binary operations
    {
        let expr_type = translator.get_expression_type(expr);
        let left_type = translator.get_expression_type(left_expr);
        let right_type = translator.get_expression_type(right_expr);

        let is_ptr_subtract = left_type.is_pointer() && left_type == right_type && *ast_op == AstBinaryOp::Subtract;
        let is_ptr_int_arith = expr_type.is_pointer() && matches!(ast_op, AstBinaryOp::Add | AstBinaryOp::Subtract);

        if is_ptr_subtract {
            return translate_pointer_subtraction(translator, left_type.clone(), expr, instructions);
        } else if is_ptr_int_arith {
            return translate_pointer_integer_arithmetic(translator, expr, instructions);
        }
    }

    match ast_op {
        AstBinaryOp::LogicalAnd => translate_logical_and(translator, left_expr, right_expr, instructions),
        AstBinaryOp::LogicalOr => translate_logical_or(translator, left_expr, right_expr, instructions),
        _ => {
            // Translate the binary operator to IR.
            let op = translate_ast_binary_operator_to_ir(ast_op);

            let lhs_value = expr::translate_expression_to_value(translator, left_expr, instructions);
            let rhs_value = expr::translate_expression_to_value(translator, right_expr, instructions);

            let dst_data_type = translator.get_ast_type_from_node(node_id);
            let dst = translator.make_temp_variable(dst_data_type.clone());

            instructions.push(BtInstruction::Binary { op, src1: lhs_value.clone(), src2: rhs_value, dst: dst.clone() });

            EvalExpr::Value(dst)
        }
    }
}

fn translate_logical_and(
    translator: &mut BlueTacTranslator,
    ast_left_expr: &AstExpression,
    ast_right_expr: &AstExpression,
    instructions: &mut Vec<BtInstruction>,
) -> EvalExpr {
    let false_label = translator.label_maker.make_unique_label("and_false");
    let end_label = translator.label_maker.make_unique_label("and_end");

    // LogicalAnd evaluates to 'int' 1 or 0.
    let true_value = BtValue::Constant(BtConstantValue::Int32(1));
    let false_value = BtValue::Constant(BtConstantValue::Int32(0));

    let dst = translator.make_temp_variable(AstType::Int);

    let left_value = expr::translate_expression_to_value(translator, ast_left_expr, instructions);
    instructions.push(BtInstruction::JumpIfZero { condition: left_value, target: false_label.clone() });

    let right_value = expr::translate_expression_to_value(translator, ast_right_expr, instructions);
    instructions.push(BtInstruction::JumpIfZero { condition: right_value, target: false_label.clone() });

    instructions.push(BtInstruction::Copy { src: true_value, dst: dst.clone() });
    instructions.push(BtInstruction::Jump { target: end_label.clone() });

    instructions.push(BtInstruction::Label { id: false_label });
    instructions.push(BtInstruction::Copy { src: false_value, dst: dst.clone() });

    instructions.push(BtInstruction::Label { id: end_label });

    EvalExpr::Value(dst)
}

fn translate_logical_or(
    translator: &mut BlueTacTranslator,
    ast_left_expr: &AstExpression,
    ast_right_expr: &AstExpression,
    instructions: &mut Vec<BtInstruction>,
) -> EvalExpr {
    let true_label = translator.label_maker.make_unique_label("or_true");
    let end_label = translator.label_maker.make_unique_label("or_end");

    // LogicalOr evaluates to 'int' 1 or 0.
    let true_value = BtValue::Constant(BtConstantValue::Int32(1));
    let false_value = BtValue::Constant(BtConstantValue::Int32(0));

    let dst = translator.make_temp_variable(AstType::Int);

    let left_value = expr::translate_expression_to_value(translator, ast_left_expr, instructions);
    instructions.push(BtInstruction::JumpIfNotZero { condition: left_value, target: true_label.clone() });

    let right_value = expr::translate_expression_to_value(translator, ast_right_expr, instructions);
    instructions.push(BtInstruction::JumpIfNotZero { condition: right_value, target: true_label.clone() });

    instructions.push(BtInstruction::Copy { src: false_value, dst: dst.clone() });
    instructions.push(BtInstruction::Jump { target: end_label.clone() });

    instructions.push(BtInstruction::Label { id: true_label });
    instructions.push(BtInstruction::Copy { src: true_value, dst: dst.clone() });

    instructions.push(BtInstruction::Label { id: end_label });

    EvalExpr::Value(dst)
}

fn translate_pointer_subtraction(
    translator: &mut BlueTacTranslator,
    ptr_type: AstType,
    expr: &AstExpression,
    instructions: &mut Vec<BtInstruction>,
) -> EvalExpr {
    let AstExpression::BinaryOperation { node_id, op, left, right } = expr else {
        ICE!("Expected an AstExpression::BinaryOperation");
    };

    debug_assert!(*op == AstBinaryOp::Subtract);
    debug_assert!(translator.get_expression_type(left).is_pointer());
    debug_assert!(translator.get_expression_type(right).is_pointer());

    let lhs_value = expr::translate_expression_to_value(translator, left, instructions);
    let rhs_value = expr::translate_expression_to_value(translator, right, instructions);

    let dst_data_type = translator.get_ast_type_from_node(node_id).clone();
    let ptr_diff = translator.make_temp_variable(dst_data_type.clone());
    let dst = translator.make_temp_variable(dst_data_type);

    let op = BtBinaryOp::Subtract;
    instructions.push(BtInstruction::Binary { op, src1: lhs_value, src2: rhs_value, dst: ptr_diff.clone() });

    let AstType::Pointer(referent) = ptr_type else {
        ICE!("Expected AstType::Pointer");
    };
    let element_size = BtValue::Constant(BtConstantValue::Int64(referent.bits() as i64 / 8));

    let op = BtBinaryOp::Divide;
    instructions.push(BtInstruction::Binary { op, src1: ptr_diff, src2: element_size, dst: dst.clone() });

    EvalExpr::Value(dst)
}

fn translate_pointer_integer_arithmetic(
    translator: &mut BlueTacTranslator,
    expr: &AstExpression,
    instructions: &mut Vec<BtInstruction>,
) -> EvalExpr {
    let AstExpression::BinaryOperation { op, left, right, .. } = expr else {
        ICE!("Expected an AstExpression::BinaryOperation");
    };

    let expr_type = translator.get_expression_type(expr).clone();

    let scale = if expr_type.is_function_pointer() {
        8
    } else {
        let AstType::Pointer(referent) = &expr_type else {
            ICE!("Expected an AstType::Pointer");
        };

        referent.bits() / 8
    };

    let left_type = translator.get_expression_type(left);
    let right_type = translator.get_expression_type(right);

    debug_assert!(
        left_type.is_pointer() && right_type.is_integer() || right_type.is_pointer() && left_type.is_integer()
    );

    let (ptr_expr, int_expr) = if left_type.is_pointer() { (left, right) } else { (right, left) };
    let int_type = if left_type.is_integer() { left_type.clone() } else { right_type.clone() };

    let dst_ptr = translator.make_temp_variable(expr_type);

    let src_ptr = expr::translate_expression_to_value(translator, ptr_expr, instructions);
    let index = expr::translate_expression_to_value(translator, int_expr, instructions);

    if *op == AstBinaryOp::Subtract {
        let negated = translator.make_temp_variable(int_type);
        instructions.push(BtInstruction::Unary { op: BtUnaryOp::Negate, src: index, dst: negated.clone() });
        instructions.push(BtInstruction::AddPtr { src_ptr, index: negated, scale, dst_ptr: dst_ptr.clone() });
    } else {
        instructions.push(BtInstruction::AddPtr { src_ptr, index, scale, dst_ptr: dst_ptr.clone() });
    }

    EvalExpr::Value(dst_ptr)
}

#[rustfmt::skip]
fn translate_ast_binary_operator_to_ir(op: &AstBinaryOp) -> BtBinaryOp {
    match op {
        AstBinaryOp::Add                   => BtBinaryOp::Add,
        AstBinaryOp::Subtract              => BtBinaryOp::Subtract,
        AstBinaryOp::Multiply              => BtBinaryOp::Multiply,
        AstBinaryOp::Divide                => BtBinaryOp::Divide,
        AstBinaryOp::Remainder             => BtBinaryOp::Remainder,
        AstBinaryOp::BitwiseAnd            => BtBinaryOp::BitwiseAnd,
        AstBinaryOp::BitwiseXor            => BtBinaryOp::BitwiseXor,
        AstBinaryOp::BitwiseOr             => BtBinaryOp::BitwiseOr,
        AstBinaryOp::LeftShift             => BtBinaryOp::LeftShift,
        AstBinaryOp::RightShift            => BtBinaryOp::RightShift,
        AstBinaryOp::EqualTo               => BtBinaryOp::EqualTo,
        AstBinaryOp::NotEqualTo            => BtBinaryOp::NotEqualTo,
        AstBinaryOp::LessThan              => BtBinaryOp::LessThan,
        AstBinaryOp::GreaterThan           => BtBinaryOp::GreaterThan,
        AstBinaryOp::LessThanOrEqualTo     => BtBinaryOp::LessThanOrEqualTo,
        AstBinaryOp::GreaterThanOrEqualTo  => BtBinaryOp::GreaterThanOrEqualTo,

        AstBinaryOp::LogicalAnd |
        AstBinaryOp::LogicalOr             => ICE!("LogicalAnd & LogicalOr are transformed to Jumps"),
    }
}
