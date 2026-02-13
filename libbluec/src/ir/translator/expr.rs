// Copyright 2025-2026 Neil Henderson
//
//! The `expr` module defines functions to translate AST expressions into the BlueTac IR.

use crate::ICE;
use crate::core::SourceLocation;
use crate::parser::{AstAssignmentOp, AstExpression, AstExpressionKind, AstFloatLiteralKind, AstType, AstUniqueName};
use crate::sema::symbol_table::SymbolAttributes;
use crate::sema::type_conversion;

use super::super::{BtBinaryOp, BtConstantValue, BtInstruction, BtType, BtUnaryOp, BtValue};
use super::{BlueTacTranslator, EvalExpr};
use super::{binary_expr, unary_expr, utils};

/// Translates an AST full expression into BlueTac IR and performs lvalue-to-rvalue conversion on the result, if
/// necessary.
pub fn translate_full_expression(
    translator: &mut BlueTacTranslator,
    expr: &AstExpression,
    instructions: &mut Vec<BtInstruction>,
) -> BtValue {
    translate_expression_to_value(translator, expr, instructions)
}

/// Translates an AST full expression into BlueTac IR and discards the result.
pub fn translate_full_expression_without_result(
    translator: &mut BlueTacTranslator,
    expr: &AstExpression,
    instructions: &mut Vec<BtInstruction>,
) {
    _ = translate_expression(translator, expr, instructions);
}

/// Translates an AST expression into BlueTac IR.
pub fn translate_expression(
    translator: &mut BlueTacTranslator,
    expr: &AstExpression,
    instructions: &mut Vec<BtInstruction>,
) -> EvalExpr {
    let expr_id = expr.id();

    match expr.kind() {
        AstExpressionKind::CharLiteral { value, .. } => {
            EvalExpr::Value(BtValue::Constant(BtConstantValue::Int32(*value)))
        }
        AstExpressionKind::StringLiteral { .. } => translate_string_literal(translator, expr),
        AstExpressionKind::IntegerLiteral { .. } => translate_integer_literal(translator, expr),
        AstExpressionKind::FloatLiteral { .. } => translate_float_literal(expr),

        AstExpressionKind::Ident { unique_name, .. } => {
            let expr_data_type = translator.get_expression_type(expr);

            // If a function designator of function type has decayed (been implicitly cast) to a function pointer
            // by the typechecker then we need to emit an instruction to take its address.
            //
            if expr_data_type.is_function_pointer()
                && let Some(symbol) = translator.symbols.get(unique_name)
                && symbol.data_type.is_function()
            {
                let src = BtValue::Variable(unique_name.to_string());
                let dst = translator.make_temp_variable(expr_data_type.clone());

                instructions.push(BtInstruction::StoreAddress { src, dst_ptr: dst.clone() });

                return EvalExpr::Value(dst);
            }

            EvalExpr::Value(BtValue::Variable(unique_name.to_string()))
        }

        AstExpressionKind::Cast { target_type, inner, .. } => {
            let expr_value = translate_expression_to_value(translator, inner, instructions);
            let expr_data_type = translator.get_expression_type(inner);

            let dst_type = target_type.resolved_type.as_ref().unwrap();

            // No need to cast if the expression's type matches the cast-to type.
            if expr_data_type == dst_type {
                return EvalExpr::Value(expr_value);
            }

            // If we can transform a constant value's type to match the cast-to `dst_type` then we can avoid
            // performing the cast.
            if expr_value.is_constant()
                && dst_type.is_arithmetic()
                && let Some(transformed_constant) = try_transform_constant_value_type(&expr_value, &dst_type.into())
            {
                EvalExpr::Value(transformed_constant)
            } else {
                let src_type = expr_data_type.clone();
                let casted = add_cast_to_temp_var(translator, expr_value, &src_type, dst_type, instructions);
                EvalExpr::Value(casted)
            }
        }

        AstExpressionKind::Deref { pointer, .. } => {
            let expr_value = translate_expression_to_value(translator, pointer, instructions);
            EvalExpr::Dereferenced(expr_value)
        }

        AstExpressionKind::AddressOf { target } => {
            let expr_result = translate_expression(translator, target, instructions);

            match expr_result {
                EvalExpr::Value(value) => {
                    let dst_data_type = translator.get_ast_type_from_node(expr_id);
                    debug_assert!(dst_data_type.is_pointer());

                    let dst = translator.make_temp_variable(dst_data_type.clone());
                    instructions.push(BtInstruction::StoreAddress { src: value, dst_ptr: dst.clone() });
                    EvalExpr::Value(dst)
                }
                EvalExpr::Dereferenced(object) => EvalExpr::Value(object),
            }
        }

        AstExpressionKind::Subscript { .. } => translate_subscript(translator, expr, instructions),

        AstExpressionKind::Assignment { .. } => translate_assignment(translator, expr, instructions),

        AstExpressionKind::Unary { .. } => unary_expr::translate_unary_operation(translator, expr, instructions),

        AstExpressionKind::Binary { .. } => binary_expr::translate_binary_operation(translator, expr, instructions),

        AstExpressionKind::Conditional { condition, consequent, alternative, .. } => {
            let dst_data_type = translator.get_expression_type(consequent); // Consequent and alternatives have same type
            let dst = translator.make_temp_variable(dst_data_type.clone());

            let alternative_label = translator.label_maker.make_unique_label("ternary_alt");
            let end_label = translator.label_maker.make_unique_label("ternary_end");

            // Condition
            let condition_value = translate_expression_to_value(translator, condition, instructions);

            instructions
                .push(BtInstruction::JumpIfZero { condition: condition_value, target: alternative_label.clone() });

            // Consequent
            let consequent_value = translate_expression_to_value(translator, consequent, instructions);

            instructions.push(BtInstruction::Copy { src: consequent_value, dst: dst.clone() });

            instructions.push(BtInstruction::Jump { target: end_label.clone() });

            // Alternative label
            instructions.push(BtInstruction::Label { id: alternative_label });

            // Alternative
            let alternative_value = translate_expression_to_value(translator, alternative, instructions);

            instructions.push(BtInstruction::Copy { src: alternative_value, dst: dst.clone() });

            // End label
            instructions.push(BtInstruction::Label { id: end_label });

            EvalExpr::Value(dst)
        }

        AstExpressionKind::FunctionCall { designator, args, .. } => {
            let dst_data_type = translator.get_ast_type_from_node(expr_id);
            let dst = translator.make_temp_variable(dst_data_type.clone());

            // Evaluate designator
            let designator = translate_expression_to_value(translator, designator, instructions);

            // Evaluate arguments
            let values =
                args.iter().map(|arg_expr| translate_expression_to_value(translator, arg_expr, instructions)).collect();

            instructions.push(BtInstruction::FunctionCall { designator, args: values, dst: dst.clone() });

            EvalExpr::Value(dst)
        }
    }
}

/// Translates an expression and, if necessary, performs lvalue-to-rvalue conversion to convert an lvalue expression
/// that identifies an object to its value.
pub fn translate_expression_to_value(
    translator: &mut BlueTacTranslator,
    expr: &AstExpression,
    instructions: &mut Vec<BtInstruction>,
) -> BtValue {
    let expr_result = translate_expression(translator, expr, instructions);

    match expr_result {
        EvalExpr::Value(value) => value,

        EvalExpr::Dereferenced(object) => {
            let rvalue_type = translator.get_ast_type_from_node(expr.id());

            // Dereferencing an object of function type is a no-op.
            if rvalue_type.is_function() {
                object
            } else {
                let rvalue = translator.make_temp_variable(rvalue_type.clone());
                instructions.push(BtInstruction::Load { src_ptr: object, dst: rvalue.clone() });
                rvalue
            }
        }
    }
}

/// Attempt to transform a constant value's type to the given `dst_type`, but only if the value can be converted without
/// loss.
fn try_transform_constant_value_type(value: &BtValue, dst_type: &BtType) -> Option<BtValue> {
    let BtValue::Constant(constant_value) = value else {
        ICE!("Expected a BtValue::Constant");
    };

    debug_assert!(dst_type.is_arithmetic());

    match constant_value {
        BtConstantValue::Int8(value) => utils::try_lossless_convert_integer(*value, dst_type),
        BtConstantValue::Int16(value) => utils::try_lossless_convert_integer(*value, dst_type),
        BtConstantValue::Int32(value) => utils::try_lossless_convert_integer(*value, dst_type),
        BtConstantValue::Int64(value) => utils::try_lossless_convert_integer(*value, dst_type),
        BtConstantValue::UInt8(value) => utils::try_lossless_convert_integer(*value, dst_type),
        BtConstantValue::UInt16(value) => utils::try_lossless_convert_integer(*value, dst_type),
        BtConstantValue::UInt32(value) => utils::try_lossless_convert_integer(*value, dst_type),
        BtConstantValue::UInt64(value) => utils::try_lossless_convert_integer(*value, dst_type),
        _ => None,
    }
}

fn translate_string_literal(translator: &mut BlueTacTranslator, expr: &AstExpression) -> EvalExpr {
    let literal_node_id = expr.id();

    let AstExpressionKind::StringLiteral { ascii, .. } = expr.kind() else {
        ICE!("Expected an AstExpression::StringLiteral");
    };

    // Concatenate the array of ascii chars/escape sequences.
    let mut constant_string = ascii.join("");

    // If the expression type is a character array then append NULL chars, if necessary, to match the array length.
    let data_type = translator.get_ast_type_from_node(literal_node_id).clone();
    if let AstType::Array { element_type, count } = &data_type
        && element_type.is_character()
        && *count > ascii.len()
    {
        let mut append_zero_count = *count - ascii.len();
        while append_zero_count > 0 {
            constant_string.push_str("\\000");
            append_zero_count -= 1;
        }
    }

    // Add the string to the constant table (or retrieve its existing index, if it already exists).
    let constant_idx = translator.constants.add_string(&constant_string);

    // Add the constant string to the symbol table (it may already exist).
    let const_name = translator.constants.make_const_symbol_name(constant_idx);
    let attrs = SymbolAttributes::constant(SourceLocation::none());
    _ = translator.symbols.add(AstUniqueName::new(const_name.clone()), data_type, attrs);

    EvalExpr::Value(BtValue::Variable(const_name))
}

fn translate_integer_literal(translator: &mut BlueTacTranslator, expr: &AstExpression) -> EvalExpr {
    let literal_node_id = expr.id();

    let AstExpressionKind::IntegerLiteral { value, .. } = expr.kind() else {
        ICE!("Expected an AstExpression::IntegerLiteral");
    };

    let literal_type = translator.get_ast_type_from_node(literal_node_id);

    // There are no warnings emitted here; if these are a narrowing conversion then the parser/sema has already
    // warned about them.
    //
    let val = match literal_type {
        AstType::Char | AstType::SignedChar => {
            BtValue::Constant(BtConstantValue::Int8(type_conversion::convert_u64_to_i8(*value)))
        }
        AstType::Short => BtValue::Constant(BtConstantValue::Int16(type_conversion::convert_u64_to_i16(*value))),
        AstType::Int => BtValue::Constant(BtConstantValue::Int32(type_conversion::convert_u64_to_i32(*value))),
        AstType::Long | AstType::LongLong => {
            BtValue::Constant(BtConstantValue::Int64(type_conversion::convert_u64_to_i64(*value)))
        }
        AstType::UnsignedChar => BtValue::Constant(BtConstantValue::UInt8(type_conversion::convert_u64_to_u8(*value))),
        AstType::UnsignedShort => {
            BtValue::Constant(BtConstantValue::UInt16(type_conversion::convert_u64_to_u16(*value)))
        }
        AstType::UnsignedInt => BtValue::Constant(BtConstantValue::UInt32(type_conversion::convert_u64_to_u32(*value))),
        AstType::UnsignedLong | AstType::UnsignedLongLong => BtValue::Constant(BtConstantValue::UInt64(*value)),

        _ => ICE!("Invalid AstType '{literal_type}' for integer literal"),
    };

    EvalExpr::Value(val)
}

fn translate_float_literal(expr: &AstExpression) -> EvalExpr {
    let AstExpressionKind::FloatLiteral { value, kind, .. } = expr.kind() else {
        ICE!("Expected an AstExpression::FloatLiteral");
    };

    let val = match kind {
        AstFloatLiteralKind::Float => BtValue::Constant(BtConstantValue::Float32(*value as f32)),
        AstFloatLiteralKind::Double | AstFloatLiteralKind::LongDouble => {
            BtValue::Constant(BtConstantValue::Float64(*value))
        }
    };

    EvalExpr::Value(val)
}

fn translate_subscript(
    translator: &mut BlueTacTranslator,
    expr: &AstExpression,
    instructions: &mut Vec<BtInstruction>,
) -> EvalExpr {
    let AstExpressionKind::Subscript { expr1, expr2, .. } = expr.kind() else {
        ICE!("Expected an AstExpression::Subscript");
    };

    let expr1_type = translator.get_expression_type(expr1);
    let expr2_type = translator.get_expression_type(expr2);

    debug_assert!(
        expr1_type.is_pointer() && expr2_type.is_integer() || expr2_type.is_pointer() && expr1_type.is_integer()
    );

    let (ptr_type, ptr_expr, int_expr) =
        if expr1_type.is_pointer() { (expr1_type.clone(), expr1, expr2) } else { (expr2_type.clone(), expr2, expr1) };

    let AstType::Pointer(referent) = &ptr_type else {
        ICE!("Expected an AstType::Pointer");
    };

    let src_ptr = translate_expression_to_value(translator, ptr_expr, instructions);
    let index = translate_expression_to_value(translator, int_expr, instructions);

    let scale = referent.bits() / 8;

    let dst_ptr = translator.make_temp_variable(ptr_type);

    instructions.push(BtInstruction::AddPtr { src_ptr, index, scale, dst_ptr: dst_ptr.clone() });

    EvalExpr::Dereferenced(dst_ptr)
}

fn translate_assignment(
    translator: &mut BlueTacTranslator,
    expr: &AstExpression,
    instructions: &mut Vec<BtInstruction>,
) -> EvalExpr {
    let AstExpressionKind::Assignment { computation_node_id, op, lhs, rhs, .. } = expr.kind() else {
        ICE!("Expected an AstExpression::Assignment");
    };

    let lhs_type = translator.get_expression_type(lhs).clone();
    let rhs_type = translator.get_expression_type(rhs).clone();

    let lhs = translate_expression(translator, lhs, instructions);
    let rhs_value = translate_expression_to_value(translator, rhs, instructions);

    if op.is_compound_assignment() {
        // lvalue-to-rvalue conversion for `lhs`.
        let (lhs_value, lhs_object) = match lhs {
            EvalExpr::Value(value) => (value, None),

            EvalExpr::Dereferenced(object) => {
                let deref_value = translator.make_temp_variable(lhs_type.clone());
                instructions.push(BtInstruction::Load { src_ptr: object.clone(), dst: deref_value.clone() });
                (deref_value, Some(object))
            }
        };

        // Get the computation type for the compound binary operation (e.g. common type of `lhs + rhs` for `+=`).
        let computation_type = translator.get_ast_type_from_node(*computation_node_id).clone();

        let op_result = translator.make_temp_variable(computation_type.clone());

        // Pointer arithmetic
        if let AstType::Pointer(referent) = &lhs_type {
            let src_ptr = lhs_value.clone();
            let index = rhs_value;

            let scale = if lhs_type.is_function_pointer() { 8 } else { referent.bits() / 8 };

            if *op == AstAssignmentOp::Subtraction {
                let negated = translator.make_temp_variable(rhs_type);
                instructions.push(BtInstruction::Unary { op: BtUnaryOp::Negate, src: index, dst: negated.clone() });
                instructions.push(BtInstruction::AddPtr { src_ptr, index: negated, scale, dst_ptr: op_result.clone() });
            } else {
                instructions.push(BtInstruction::AddPtr { src_ptr, index, scale, dst_ptr: op_result.clone() });
            }
        } else {
            // If necessary, cast the lhs and/or rhs to the computation type.
            let casted_src1 = if lhs_type != computation_type {
                add_cast_to_temp_var(translator, lhs_value.clone(), &lhs_type, &computation_type, instructions)
            } else {
                lhs_value.clone()
            };

            let casted_src2 = if rhs_type != computation_type {
                add_cast_to_temp_var(translator, rhs_value, &rhs_type, &computation_type, instructions)
            } else {
                rhs_value
            };

            // Perform the binary operation
            let binary_op = translate_ast_compound_assignment_operator_to_ir_binary_op(op);
            instructions.push(BtInstruction::Binary {
                op: binary_op,
                src1: casted_src1,
                src2: casted_src2,
                dst: op_result.clone(),
            });
        }

        // Copy the result back into the lhs variable, with a cast if necessary.
        copy_value_with_optional_cast(op_result, lhs_value.clone(), &computation_type, &lhs_type, instructions);

        if let Some(lhs_object) = lhs_object {
            instructions.push(BtInstruction::Store { src: lhs_value.clone(), dst_ptr: lhs_object });
        }

        EvalExpr::Value(lhs_value)
    } else {
        match lhs {
            EvalExpr::Value(lhs_value) => {
                instructions.push(BtInstruction::Copy { src: rhs_value, dst: lhs_value.clone() });
                EvalExpr::Value(lhs_value)
            }
            EvalExpr::Dereferenced(lhs_obj) => {
                instructions.push(BtInstruction::Store { src: rhs_value, dst_ptr: lhs_obj.clone() });
                EvalExpr::Value(lhs_obj)
            }
        }
    }
}

pub fn add_cast_to_temp_var(
    translator: &mut BlueTacTranslator,
    src: BtValue,
    src_type: &AstType,
    dst_type: &AstType,
    instructions: &mut Vec<BtInstruction>,
) -> BtValue {
    let dst = translator.make_temp_variable(dst_type.clone());

    copy_value_with_optional_cast(src, dst.clone(), src_type, dst_type, instructions);

    dst
}

pub fn copy_value_with_optional_cast(
    src: BtValue,
    dst: BtValue,
    src_type: &AstType,
    dst_type: &AstType,
    instructions: &mut Vec<BtInstruction>,
) {
    let same_types = src_type == dst_type;
    let same_bit_count = src_type.bits() == dst_type.bits();

    // Either or both 'src' and 'dst' are pointers
    //
    if src_type.is_pointer() || dst_type.is_pointer() {
        if same_types || same_bit_count {
            instructions.push(BtInstruction::Copy { src, dst });
        } else if src_type.is_integer() {
            instructions.push(BtInstruction::SignExtend { src, dst });
        } else if dst_type.is_integer() {
            instructions.push(BtInstruction::Truncate { src, dst });
        } else {
            ICE!("Unexpected types '{src_type}' and '{dst_type}' in cast");
        }

        return;
    }

    // Floating-point -> Floating-point
    //
    if src_type.is_floating_point() && dst_type.is_floating_point() {
        // 'long double' is an alias for 'double' so they are different AstTypes but have the same bit count.
        let instr = if same_types || same_bit_count {
            BtInstruction::Copy { src, dst }
        } else {
            BtInstruction::ConvertFp { src, dst }
        };

        instructions.push(instr);
    }
    // Floating-point -> Signed/Unsigned Integer
    //
    else if src_type.is_floating_point() {
        let instr = match dst_type {
            dt if dt.is_signed_integer() => BtInstruction::FpToSignedInteger { src, dst },
            dt if dt.is_unsigned_integer() => BtInstruction::FpToUnsignedInteger { src, dst },
            _ => ICE!("Unhandled conversion '{src_type}' -> '{dst_type}'"),
        };

        instructions.push(instr);
    }
    // Signed/Unsigned Integer -> Floating-point
    //
    else if dst_type.is_floating_point() {
        let instr = match src_type {
            st if st.is_signed_integer() => BtInstruction::SignedIntegerToFp { src, dst },
            st if st.is_unsigned_integer() => BtInstruction::UnsignedIntegerToFp { src, dst },
            _ => ICE!("Unhandled conversion '{src_type}' -> '{dst_type}'"),
        };

        instructions.push(instr);
    }
    // Integer -> Integer
    //
    else {
        debug_assert!(src_type.is_integer() && dst_type.is_integer());

        let instr = match (src_type, dst_type) {
            (_, _) if same_types || same_bit_count => BtInstruction::Copy { src, dst },
            (s, d) if d.bits() < s.bits() => BtInstruction::Truncate { src, dst },
            (s, _) if s.is_signed_integer() => BtInstruction::SignExtend { src, dst },
            (s, _) if s.is_unsigned_integer() => BtInstruction::ZeroExtend { src, dst },
            _ => ICE!("Unhandled conversion '{src_type}' -> '{dst_type}'"),
        };

        instructions.push(instr);
    }
}

#[rustfmt::skip]
fn translate_ast_compound_assignment_operator_to_ir_binary_op(op: &AstAssignmentOp) -> BtBinaryOp {
    match op {
        AstAssignmentOp::Addition    => BtBinaryOp::Add,
        AstAssignmentOp::Subtraction => BtBinaryOp::Subtract,
        AstAssignmentOp::Multiply    => BtBinaryOp::Multiply,
        AstAssignmentOp::Divide      => BtBinaryOp::Divide,
        AstAssignmentOp::Remainder   => BtBinaryOp::Remainder,
        AstAssignmentOp::BitwiseAnd  => BtBinaryOp::BitwiseAnd,
        AstAssignmentOp::BitwiseOr   => BtBinaryOp::BitwiseOr,
        AstAssignmentOp::BitwiseXor  => BtBinaryOp::BitwiseXor,
        AstAssignmentOp::LeftShift   => BtBinaryOp::LeftShift,
        AstAssignmentOp::RightShift  => BtBinaryOp::RightShift,

        _ => ICE!("Unexpected AstAssignmentOp '{op}'"),
    }
}
