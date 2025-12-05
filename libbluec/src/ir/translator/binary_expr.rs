// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `binary_epxr` module defines functions to translate AST binary expressions into the BlueTac IR.

use super::super::{BtBinaryOp, BtConstantValue, BtInstruction, BtValue};
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

    match ast_op {
        AstBinaryOp::LogicalAnd => translate_logical_and(translator, left_expr, right_expr, instructions),
        AstBinaryOp::LogicalOr => translate_logical_or(translator, left_expr, right_expr, instructions),
        _ => {
            // Translate the binary operator to IR.
            //      A compound assignment operator is translated to its non-assignment versions,
            //      e.g. `+=` becomes `+`.
            //
            let op = translate_ast_binary_operator_to_ir(ast_op);

            if ast_op.is_compound_assignment() {
                translate_compound_assignment(translator, op, left_expr, right_expr, instructions)
            } else {
                let lhs_value = expr::translate_expression_to_value(translator, left_expr, instructions);
                let rhs_value = expr::translate_expression_to_value(translator, right_expr, instructions);

                let dst_data_type = translator.get_ast_type_from_node(node_id);
                let dst = translator.make_temp_variable(dst_data_type.clone());

                instructions.push(BtInstruction::Binary {
                    op,
                    src1: lhs_value.clone(),
                    src2: rhs_value,
                    dst: dst.clone(),
                });
                EvalExpr::Value(dst)
            }
        }
    }
}

fn translate_compound_assignment(
    translator: &mut BlueTacTranslator,
    op: BtBinaryOp,
    left_expr: &AstExpression,
    right_expr: &AstExpression,
    instructions: &mut Vec<BtInstruction>,
) -> EvalExpr {
    let left_type = translator.get_expression_type(left_expr).clone();
    let right_type = translator.get_expression_type(right_expr).clone();

    let lhs = expr::translate_expression(translator, left_expr, instructions);
    let rhs_value = expr::translate_expression_to_value(translator, right_expr, instructions);

    let (lhs_value, lhs_object) = match lhs {
        EvalExpr::Value(value) => (value, None),

        EvalExpr::Dereferenced(object) => {
            let deref_value = translator.make_temp_variable(left_type.clone());
            instructions.push(BtInstruction::Load { src_ptr: object.clone(), dst: deref_value.clone() });
            (deref_value, Some(object))
        }
    };

    // We need to perform the underlying operation first with the common type of the lvalue variable and the rvalue
    // expression, except for two cases. Compound shift assignment operations always evaluate to their lhs lvalue type,
    // and if the lhs is a pointer type then the common type is the lhs type. (Sema has already validated that the
    // types are compatible.)
    //
    let is_shift = matches!(op, BtBinaryOp::LeftShift | BtBinaryOp::RightShift);
    let is_ptr = left_type.is_pointer();

    let common_data_type =
        if is_shift || is_ptr { left_type.clone() } else { AstType::get_common_type(&left_type, &right_type) };

    // If necessary, cast the lvalue and/or rvalue into the common type.
    let casted_src1 = if left_type != common_data_type {
        expr::add_cast_to_temp_var(translator, lhs_value.clone(), &left_type, &common_data_type, instructions)
    } else {
        lhs_value.clone()
    };

    let casted_src2 = if right_type != common_data_type {
        expr::add_cast_to_temp_var(translator, rhs_value.clone(), &right_type, &common_data_type, instructions)
    } else {
        rhs_value.clone()
    };

    // Perform the operation
    let op_result = translator.make_temp_variable(common_data_type.clone());
    instructions.push(BtInstruction::Binary { op, src1: casted_src1, src2: casted_src2, dst: op_result.clone() });

    // Copy the result back into the lvalue variable, with a cast if necessary.
    expr::copy_value_with_optional_cast(op_result, lhs_value.clone(), &common_data_type, &left_type, instructions);

    if let Some(lhs_object) = lhs_object {
        instructions.push(BtInstruction::Store { src: lhs_value.clone(), dst_ptr: lhs_object });
    }

    EvalExpr::Value(lhs_value)
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

        // Note that we translate the compound assignment operators to their non-assignment versions,
        // because we generate IR that performs the assignment afterwards.
        AstBinaryOp::AdditionAssignment    => BtBinaryOp::Add,
        AstBinaryOp::SubtractionAssignment => BtBinaryOp::Subtract,
        AstBinaryOp::MultiplyAssignment    => BtBinaryOp::Multiply,
        AstBinaryOp::DivideAssignment      => BtBinaryOp::Divide,
        AstBinaryOp::RemainderAssignment   => BtBinaryOp::Remainder,
        AstBinaryOp::BitwiseAndAssignment  => BtBinaryOp::BitwiseAnd,
        AstBinaryOp::BitwiseOrAssignment   => BtBinaryOp::BitwiseOr,
        AstBinaryOp::BitwiseXorAssignment  => BtBinaryOp::BitwiseXor,
        AstBinaryOp::LeftShiftAssignment   => BtBinaryOp::LeftShift,
        AstBinaryOp::RightShiftAssignment  => BtBinaryOp::RightShift,

        AstBinaryOp::LogicalAnd |
        AstBinaryOp::LogicalOr             => ICE!("LogicalAnd & LogicalOr are transformed to Jumps"),

        #[allow(unreachable_patterns)]
        _ => ICE!("Unexpected AstBinaryOp '{op}'"),
    }
}
