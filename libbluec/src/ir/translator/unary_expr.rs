// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `unary_epxr` module defines functions to translate AST unary expressions into the BlueTac IR.

use super::super::{BtBinaryOp, BtConstantValue, BtInstruction, BtUnaryOp, BtValue};
use super::{BlueTacTranslator, EvalExpr};
use super::expr;

use crate::ICE;
use crate::parser::{AstExpression, AstFloatLiteralKind, AstType, AstUnaryOp};
use crate::sema::type_conversion;

/// Translates a unary expression into BlueTac IR.
pub fn translate_unary_operation(
    translator: &mut BlueTacTranslator,
    expr: &AstExpression,
    instructions: &mut Vec<BtInstruction>,
) -> EvalExpr {
    let AstExpression::UnaryOperation { node_id: unary_expr_node_id, op: ast_op, expr: ast_unary_expr } = expr else {
        ICE!("Expected an AstExpression::UnaryOperation");
    };

    // If the unary operator is Negate, and the subexpression is an integer or floating-point literal, then
    // we apply the negation ourselves and return a `BtValue::Constant` value.
    //
    // That saves us emitting unnecessary instructions, and also allows us to handle edge cases like
    // i32::MIN (-2147483648) which is parsed as Negate(2147483648_u64).
    let is_negate = *ast_op == AstUnaryOp::Negate;

    if is_negate {
        if let AstExpression::FloatLiteral { value, kind, .. } = **ast_unary_expr {
            let val = match kind {
                AstFloatLiteralKind::Float => BtValue::Constant(BtConstantValue::Float32(-value as f32)),

                AstFloatLiteralKind::Double | AstFloatLiteralKind::LongDouble => {
                    BtValue::Constant(BtConstantValue::Float64(-value))
                }
            };

            return EvalExpr::Value(val);
        } else if let AstExpression::IntegerLiteral { value, kind, .. } = **ast_unary_expr {
            // We're negating so the literal has to fit inside a 64-bit signed integer range.
            let int64 = type_conversion::convert_u64_to_i64(value);

            // Apply the negate unary operator
            //      Remember, we can't negate i64::MIN because it would overflow.
            let negated_int64 = if int64 == i64::MIN { 0 } else { -int64 };

            let literal_too_big_for_32bits = kind.data_type().bits() > 32;

            // Handle special case of i32::MIN.
            //      In this case, we have a unary negate operator and a LongIntegerLiteral(2147483648).
            //      We'll convert to an Int32 value. The unary operation expression itself will still have
            //      the type of 'long'.
            //
            let val = if literal_too_big_for_32bits && negated_int64 == i32::MIN as i64 {
                BtValue::Constant(BtConstantValue::Int32(negated_int64 as i32))
            } else if literal_too_big_for_32bits {
                BtValue::Constant(BtConstantValue::Int64(negated_int64))
            } else {
                BtValue::Constant(BtConstantValue::Int32(type_conversion::convert_i64_to_i32(negated_int64)))
            };

            return EvalExpr::Value(val);
        }
    }

    let is_assign = matches!(
        ast_op,
        AstUnaryOp::PrefixIncrement
            | AstUnaryOp::PrefixDecrement
            | AstUnaryOp::PostfixIncrement
            | AstUnaryOp::PostfixDecrement
    );

    let unary_expr_data_type = translator.get_ast_type_from_node(unary_expr_node_id).clone(); // The type of the unary operation (not its inner expr)

    if is_assign {
        let is_postfix = matches!(ast_op, AstUnaryOp::PostfixIncrement | AstUnaryOp::PostfixDecrement);

        let eval_expr = expr::translate_expression(translator, ast_unary_expr, instructions);

        let (src_value, object) = match eval_expr {
            EvalExpr::Value(value) => (value, None),

            EvalExpr::Dereferenced(object) => {
                let deref_value = translator.make_temp_variable(unary_expr_data_type.clone());
                instructions.push(BtInstruction::Load { src_ptr: object.clone(), dst: deref_value.clone() });
                (deref_value, Some(object))
            }
        };

        let (old_value, new_value) =
            translate_assign_incr_decr(translator, ast_op, unary_expr_data_type, src_value, instructions);

        if let Some(object) = object {
            instructions.push(BtInstruction::Store { src: new_value.clone(), dst_ptr: object });
        }

        if is_postfix { EvalExpr::Value(old_value.expect("Expect old value")) } else { EvalExpr::Value(new_value) }
    } else {
        let src = expr::translate_expression_to_value(translator, ast_unary_expr, instructions);
        let dst = translator.make_temp_variable(unary_expr_data_type.clone());

        let op = translate_ast_unary_operator_to_ir(ast_op);
        instructions.push(BtInstruction::Unary { op, src, dst: dst.clone() });
        EvalExpr::Value(dst)
    }
}

fn translate_assign_incr_decr(
    translator: &mut BlueTacTranslator,
    op: &AstUnaryOp,
    op_type: AstType,
    src: BtValue,
    instructions: &mut Vec<BtInstruction>,
) -> (Option<BtValue>, BtValue) {
    let tmp = translator.make_temp_variable(op_type.clone());

    match op {
        AstUnaryOp::PrefixIncrement => {
            let new_value = translate_prefix_increment(translator, src, tmp, instructions);
            (None, new_value)
        }

        AstUnaryOp::PrefixDecrement => {
            let new_value = translate_prefix_decrement(translator, src, tmp, instructions);
            (None, new_value)
        }

        AstUnaryOp::PostfixIncrement => {
            let old_value = translator.make_temp_variable(op_type);
            instructions.push(BtInstruction::Copy { src: src.clone(), dst: old_value.clone() });
            let new_value = translate_prefix_increment(translator, src, tmp, instructions);
            (Some(old_value), new_value)
        }

        AstUnaryOp::PostfixDecrement => {
            let old_value = translator.make_temp_variable(op_type);
            instructions.push(BtInstruction::Copy { src: src.clone(), dst: old_value.clone() });
            let new_value = translate_prefix_decrement(translator, src, tmp, instructions);
            (Some(old_value), new_value)
        }

        _ => ICE!("Expected a pre/post-fix increment/decrement operator instead of '{op}'"),
    }
}

// Input:   ++src
// Output:  tmp = src + 1
//          src = tmp
fn translate_prefix_increment(
    translator: &mut BlueTacTranslator,
    src: BtValue,
    tmp: BtValue,
    instructions: &mut Vec<BtInstruction>,
) -> BtValue {
    let src_type = src.get_type(&translator.symbols);
    let incr_value = BtValue::Constant(BtConstantValue::one(src_type));

    instructions.push(BtInstruction::Binary {
        op: BtBinaryOp::Add,
        src1: src.clone(),
        src2: incr_value,
        dst: tmp.clone(),
    });

    instructions.push(BtInstruction::Copy { src: tmp.clone(), dst: src.clone() });

    src
}

// Input:   --src
// Output:  tmp = src - 1
//          src = tmp
fn translate_prefix_decrement(
    translator: &mut BlueTacTranslator,
    src: BtValue,
    tmp: BtValue,
    instructions: &mut Vec<BtInstruction>,
) -> BtValue {
    let src_type = src.get_type(&translator.symbols);
    let decr_value = BtValue::Constant(BtConstantValue::one(src_type));

    instructions.push(BtInstruction::Binary {
        op: BtBinaryOp::Subtract,
        src1: src.clone(),
        src2: decr_value,
        dst: tmp.clone(),
    });

    instructions.push(BtInstruction::Copy { src: tmp.clone(), dst: src.clone() });

    src
}

fn translate_ast_unary_operator_to_ir(op: &AstUnaryOp) -> BtUnaryOp {
    match op {
        AstUnaryOp::BitwiseNot => BtUnaryOp::BitwiseNot,
        AstUnaryOp::LogicalNot => BtUnaryOp::LogicalNot,
        AstUnaryOp::Negate => BtUnaryOp::Negate,
        AstUnaryOp::Plus => BtUnaryOp::Plus,

        #[allow(unreachable_patterns)]
        _ => ICE!("Unexpected AstUnaryOp '{op}'"),
    }
}
