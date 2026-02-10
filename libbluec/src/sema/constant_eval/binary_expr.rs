// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `binary_expr` module provides functionality to evaluate binary expressions at compile-time.

use crate::ICE;
use crate::compiler_driver::Warning;
use crate::parser::{AstAddressConstant, AstBinaryOp, AstNodeId, AstType};

use super::ConstantValue;
use super::Eval;

/// Evaluates a constant binary expression.
pub fn evaluate_binary_op(
    expr_node_id: AstNodeId,
    rhs_node_id: AstNodeId,
    op: &AstBinaryOp,
    lhs: ConstantValue,
    rhs: ConstantValue,
    eval: &mut Eval,
) -> Option<ConstantValue> {
    // Check for pointer arithmetic first.
    //
    if lhs.is_pointer() && rhs.is_integer() {
        return evaluate_pointer_integer_arithmetic(*op, lhs, rhs);
    } else if lhs.is_integer() && rhs.is_pointer() {
        return evaluate_pointer_integer_arithmetic(*op, rhs, lhs);
    } else if lhs.is_pointer() && rhs.is_pointer() {
        return evaluate_pointer_arithmetic(*op, lhs, rhs);
    } else if lhs.is_pointer() || rhs.is_pointer() {
        return None;
    }

    if let ConstantValue::Int { .. } = lhs {
        let ConstantValue::Int { .. } = rhs else {
            ICE!("Expected other to be ConstantValue::Int");
        };

        evaluate_integer_binary_op(expr_node_id, rhs_node_id, op, lhs, rhs, eval)
    } else if let ConstantValue::Float { value: lhs, size } = lhs {
        let ConstantValue::Float { value: rhs, size: rhs_size } = rhs else {
            ICE!("Expected other to be ConstantValue::Float");
        };

        debug_assert!(size == rhs_size);
        debug_assert!(size == 32 || size == 64);

        evaluate_fp_binary_op(expr_node_id, op, lhs, rhs, size, eval)
    } else {
        None
    }
}

fn evaluate_integer_binary_op(
    expr_node_id: AstNodeId,
    rhs_node_id: AstNodeId,
    op: &AstBinaryOp,
    lhs: ConstantValue,
    rhs: ConstantValue,
    eval: &mut Eval,
) -> Option<ConstantValue> {
    let int_type = lhs.get_ast_type();

    let ConstantValue::Int { value: lhs, signed, size } = lhs else {
        ICE!("Expected ConstantValue::Int");
    };

    let ConstantValue::Int { value: rhs, signed: rhs_signed, size: rhs_size } = rhs else {
        ICE!("Expected ConstantValue::Int");
    };

    debug_assert!(signed == rhs_signed);
    debug_assert!(size == rhs_size);

    match op {
        // Arithmetic
        AstBinaryOp::Add => arithmetic_int(expr_node_id, rhs_node_id, op, int_type, lhs, rhs, eval),
        AstBinaryOp::Subtract => arithmetic_int(expr_node_id, rhs_node_id, op, int_type, lhs, rhs, eval),
        AstBinaryOp::Multiply => arithmetic_int(expr_node_id, rhs_node_id, op, int_type, lhs, rhs, eval),
        AstBinaryOp::Divide => arithmetic_int(expr_node_id, rhs_node_id, op, int_type, lhs, rhs, eval),
        AstBinaryOp::Remainder => arithmetic_int(expr_node_id, rhs_node_id, op, int_type, lhs, rhs, eval),

        // Bitwise
        AstBinaryOp::BitwiseAnd => Some(ConstantValue::Int { value: lhs & rhs, signed, size }),
        AstBinaryOp::BitwiseOr => Some(ConstantValue::Int { value: lhs | rhs, signed, size }),
        AstBinaryOp::BitwiseXor => Some(ConstantValue::Int { value: lhs ^ rhs, signed, size }),

        AstBinaryOp::LeftShift => match rhs {
            0 => {
                if eval.emit_diagnostics {
                    let loc = eval.chk.metadata.get_source_location(&rhs_node_id);
                    Warning::shift_count_zero(loc, eval.driver);
                }
                Some(ConstantValue::Int { value: lhs, signed, size })
            }
            n if n < 0 => {
                if eval.emit_diagnostics {
                    let loc = eval.chk.metadata.get_source_location(&rhs_node_id);
                    Warning::shift_count_negative(loc, eval.driver);
                }
                None
            }
            n => {
                if eval.emit_diagnostics && n >= int_type.bits() as i128 {
                    let loc = eval.chk.metadata.get_source_location(&rhs_node_id);
                    Warning::shift_count_overflow(int_type.bits(), false, loc, eval.driver);
                }
                lhs.checked_shl(rhs as u32).map(|value| ConstantValue::Int { value, signed, size })
            }
        },
        AstBinaryOp::RightShift => match rhs {
            0 => {
                if eval.emit_diagnostics {
                    let loc = eval.chk.metadata.get_source_location(&rhs_node_id);
                    Warning::shift_count_zero(loc, eval.driver);
                }
                Some(ConstantValue::Int { value: lhs, signed, size })
            }
            n if n < 0 => {
                if eval.emit_diagnostics {
                    let loc = eval.chk.metadata.get_source_location(&rhs_node_id);
                    Warning::shift_count_negative(loc, eval.driver);
                }
                None
            }
            n => {
                if eval.emit_diagnostics && n >= int_type.bits() as i128 {
                    let loc = eval.chk.metadata.get_source_location(&rhs_node_id);
                    Warning::shift_count_overflow(int_type.bits(), false, loc, eval.driver);
                }
                lhs.checked_shr(rhs as u32).map(|value| ConstantValue::Int { value, signed, size })
            }
        },

        // Logical
        AstBinaryOp::LogicalAnd => Some(ConstantValue::make_int((lhs != 0 && rhs != 0) as i32)),
        AstBinaryOp::LogicalOr => Some(ConstantValue::make_int((lhs != 0 || rhs != 0) as i32)),

        // Comparison
        AstBinaryOp::EqualTo => Some(ConstantValue::make_int((lhs == rhs) as i32)),
        AstBinaryOp::NotEqualTo => Some(ConstantValue::make_int((lhs != rhs) as i32)),
        AstBinaryOp::LessThan => Some(ConstantValue::make_int((lhs < rhs) as i32)),
        AstBinaryOp::LessThanOrEqualTo => Some(ConstantValue::make_int((lhs <= rhs) as i32)),
        AstBinaryOp::GreaterThan => Some(ConstantValue::make_int((lhs > rhs) as i32)),
        AstBinaryOp::GreaterThanOrEqualTo => Some(ConstantValue::make_int((lhs >= rhs) as i32)),
    }
}

fn evaluate_fp_binary_op(
    expr_node_id: AstNodeId,
    op: &AstBinaryOp,
    lhs: f64,
    rhs: f64,
    size: usize,
    eval: &mut Eval,
) -> Option<ConstantValue> {
    let mut arithmetic = |lhs: f64, rhs: f64, size: usize| {
        if size == 32 {
            arithmetic_fp(expr_node_id, op, lhs as f32, rhs as f32, eval)
        } else {
            arithmetic_fp(expr_node_id, op, lhs, rhs, eval)
        }
    };

    match op {
        // Arithmetic
        AstBinaryOp::Add => arithmetic(lhs, rhs, size),
        AstBinaryOp::Subtract => arithmetic(lhs, rhs, size),
        AstBinaryOp::Multiply => arithmetic(lhs, rhs, size),
        AstBinaryOp::Divide => arithmetic(lhs, rhs, size),
        AstBinaryOp::Remainder => None,

        // Bitwise
        AstBinaryOp::BitwiseAnd
        | AstBinaryOp::BitwiseOr
        | AstBinaryOp::BitwiseXor
        | AstBinaryOp::LeftShift
        | AstBinaryOp::RightShift => None,

        // Logical
        AstBinaryOp::LogicalAnd => Some(ConstantValue::make_int((lhs != 0.0 && rhs != 0.0) as i32)),
        AstBinaryOp::LogicalOr => Some(ConstantValue::make_int((lhs != 0.0 || rhs != 0.0) as i32)),

        // Comparison
        AstBinaryOp::EqualTo => Some(ConstantValue::make_int((lhs == rhs) as i32)),
        AstBinaryOp::NotEqualTo => Some(ConstantValue::make_int((lhs != rhs) as i32)),
        AstBinaryOp::LessThan => Some(ConstantValue::make_int((lhs < rhs) as i32)),
        AstBinaryOp::LessThanOrEqualTo => Some(ConstantValue::make_int((lhs <= rhs) as i32)),
        AstBinaryOp::GreaterThan => Some(ConstantValue::make_int((lhs > rhs) as i32)),
        AstBinaryOp::GreaterThanOrEqualTo => Some(ConstantValue::make_int((lhs >= rhs) as i32)),
    }
}

trait FloatOps: Copy + ToString {
    fn ast_type() -> AstType;
    fn overflowing_add(self, rhs: Self) -> (Self, bool);
    fn overflowing_sub(self, rhs: Self) -> (Self, bool);
    fn overflowing_mul(self, rhs: Self) -> (Self, bool);
    fn overflowing_div(self, rhs: Self) -> (Self, bool);
}

// Helper macro that evaluates a floating-point binary arithmetic operation and checks for overflow.
macro_rules! overflowing_fp_binary_op {
    ($lhs:expr, $rhs:expr, $op:tt) => {{
        let new_value = $lhs $op $rhs; // Evaluate the binary operation once
        let overflowed = new_value.is_infinite() && $lhs.is_finite() && $rhs.is_finite();
        (new_value, overflowed)
    }};
}

impl FloatOps for f32 {
    fn ast_type() -> AstType {
        AstType::Float
    }

    fn overflowing_add(self, rhs: Self) -> (Self, bool) {
        overflowing_fp_binary_op!(self, rhs, +)
    }

    fn overflowing_sub(self, rhs: Self) -> (Self, bool) {
        overflowing_fp_binary_op!(self, rhs, -)
    }

    fn overflowing_mul(self, rhs: Self) -> (Self, bool) {
        overflowing_fp_binary_op!(self, rhs, *)
    }

    fn overflowing_div(self, rhs: Self) -> (Self, bool) {
        overflowing_fp_binary_op!(self, rhs, /)
    }
}

impl FloatOps for f64 {
    fn ast_type() -> AstType {
        AstType::Double
    }

    fn overflowing_add(self, rhs: Self) -> (Self, bool) {
        overflowing_fp_binary_op!(self, rhs, +)
    }

    fn overflowing_sub(self, rhs: Self) -> (Self, bool) {
        overflowing_fp_binary_op!(self, rhs, -)
    }

    fn overflowing_mul(self, rhs: Self) -> (Self, bool) {
        overflowing_fp_binary_op!(self, rhs, *)
    }

    fn overflowing_div(self, rhs: Self) -> (Self, bool) {
        overflowing_fp_binary_op!(self, rhs, /)
    }
}

fn arithmetic_fp<T>(expr_node_id: AstNodeId, op: &AstBinaryOp, lhs: T, rhs: T, eval: &mut Eval) -> Option<ConstantValue>
where
    T: FloatOps,
    ConstantValue: From<T>,
{
    let fp_type = T::ast_type();

    let (new_value, overflowed) = match op {
        AstBinaryOp::Add => lhs.overflowing_add(rhs),
        AstBinaryOp::Subtract => lhs.overflowing_sub(rhs),
        AstBinaryOp::Multiply => lhs.overflowing_mul(rhs),
        AstBinaryOp::Divide => lhs.overflowing_div(rhs),
        _ => ICE!("Unexpected AstBinaryOp"),
    };

    if eval.emit_diagnostics && overflowed {
        let new_value = new_value.to_string();
        let loc = eval.chk.metadata.get_source_location(&expr_node_id);
        Warning::floating_point_overflow(&fp_type, &new_value, loc, eval.driver);
    }

    Some(ConstantValue::from(new_value))
}

fn arithmetic_int(
    expr_node_id: AstNodeId,
    rhs_node_id: AstNodeId,
    op: &AstBinaryOp,
    int_type: AstType,
    lhs_value: i128,
    rhs_value: i128,
    eval: &mut Eval,
) -> Option<ConstantValue> {
    // Emit a warning if performing division/remainder by zero.
    //
    if eval.emit_diagnostics && matches!(op, AstBinaryOp::Divide | AstBinaryOp::Remainder) && rhs_value == 0 {
        let op_loc = eval.chk.metadata.get_operator_sloc(&expr_node_id);
        let rhs_loc = eval.chk.metadata.get_source_location(&rhs_node_id);
        Warning::division_by_zero(op, op_loc, rhs_loc, eval.driver);
    }

    if int_type.is_signed_integer() {
        // Helper macro to emit an integer overflow warning if `overflowed` is true.
        macro_rules! warn_if_int_overflow {
            ($overflowed:expr, $new_value:expr) => {
                if eval.emit_diagnostics && $overflowed {
                    let new_value = $new_value.to_string();
                    let loc = eval.chk.metadata.get_source_location(&expr_node_id);
                    Warning::integer_overflow(&int_type, &new_value, loc, eval.driver);
                }
            };
        }

        // Helper macro to map the AstBinaryOp to the appropriate `overflowing_xyz` function.
        macro_rules! overflowing_op {
            ($op:expr, $op_type:ty) => {
                match $op {
                    AstBinaryOp::Add => {
                        let (value, overflowed) = (lhs_value as $op_type).overflowing_add(rhs_value as $op_type);
                        warn_if_int_overflow!(overflowed, value);
                        Some(value).map(ConstantValue::from)
                    }
                    AstBinaryOp::Subtract => {
                        let (value, overflowed) = (lhs_value as $op_type).overflowing_sub(rhs_value as $op_type);
                        warn_if_int_overflow!(overflowed, value);
                        Some(value).map(ConstantValue::from)
                    }
                    AstBinaryOp::Multiply => {
                        let (value, overflowed) = (lhs_value as $op_type).overflowing_mul(rhs_value as $op_type);
                        warn_if_int_overflow!(overflowed, value);
                        Some(value).map(ConstantValue::from)
                    }
                    AstBinaryOp::Divide => {
                        if rhs_value == 0 {
                            None
                        } else {
                            let (value, overflowed) = (lhs_value as $op_type).overflowing_div(rhs_value as $op_type);
                            warn_if_int_overflow!(overflowed, value);
                            Some(value).map(ConstantValue::from)
                        }
                    }
                    AstBinaryOp::Remainder => {
                        if rhs_value == 0 {
                            None
                        } else {
                            let (value, overflowed) = (lhs_value as $op_type).overflowing_rem(rhs_value as $op_type);
                            warn_if_int_overflow!(overflowed, value);
                            Some(value).map(ConstantValue::from)
                        }
                    }
                    _ => ICE!("Unexpected AstBinaryOp"),
                }
            };
        }

        match int_type {
            AstType::Char | AstType::SignedChar => overflowing_op!(op, i8),
            AstType::Short => overflowing_op!(op, i16),
            AstType::Int => overflowing_op!(op, i32),
            AstType::Long | AstType::LongLong => overflowing_op!(op, i64),
            _ => ICE!("Unexpected integer type '{int_type}'"),
        }
    } else {
        // Helper macro to map the AstBinaryOp to the appropriate `wrapping_xyz` function.
        macro_rules! wrapping_op {
            ($op:expr, $op_type:ty) => {
                match $op {
                    AstBinaryOp::Add => Some((lhs_value as $op_type).wrapping_add(rhs_value as $op_type)),
                    AstBinaryOp::Subtract => Some((lhs_value as $op_type).wrapping_sub(rhs_value as $op_type)),
                    AstBinaryOp::Multiply => Some((lhs_value as $op_type).wrapping_mul(rhs_value as $op_type)),
                    AstBinaryOp::Divide => {
                        if rhs_value == 0 {
                            None
                        } else {
                            Some((lhs_value as $op_type).wrapping_div(rhs_value as $op_type))
                        }
                    }
                    AstBinaryOp::Remainder => {
                        if rhs_value == 0 {
                            None
                        } else {
                            Some((lhs_value as $op_type).wrapping_rem(rhs_value as $op_type))
                        }
                    }
                    _ => ICE!("Unexpected AstBinaryOp"),
                }
            };
        }

        match int_type {
            AstType::UnsignedChar => wrapping_op!(op, i8).map(ConstantValue::from),
            AstType::UnsignedShort => wrapping_op!(op, i16).map(ConstantValue::from),
            AstType::UnsignedInt => wrapping_op!(op, i32).map(ConstantValue::from),
            AstType::UnsignedLong | AstType::UnsignedLongLong => wrapping_op!(op, i64).map(ConstantValue::from),
            _ => ICE!("Unexpected integer type '{int_type}'"),
        }
    }
}

fn evaluate_pointer_integer_arithmetic(
    op: AstBinaryOp,
    ptr_value: ConstantValue,
    int_value: ConstantValue,
) -> Option<ConstantValue> {
    let valid_op = matches!(op, AstBinaryOp::Add | AstBinaryOp::Subtract);
    if !valid_op {
        return None;
    }

    let ConstantValue::Pointer(ty, address_constant) = ptr_value else {
        return None;
    };

    let ConstantValue::Int { value, .. } = int_value else {
        return None;
    };

    let AstType::Pointer(referent) = &ty else {
        return None;
    };

    if let AstAddressConstant::AddressOfObject { object, byte_offset } = address_constant {
        let referent_bytes = (referent.bits() / 8) as i32;

        let new_byte_offset = if op == AstBinaryOp::Add {
            byte_offset + (referent_bytes * value as i32)
        } else {
            byte_offset - (referent_bytes * value as i32)
        };

        let new_addr_const = AstAddressConstant::AddressOfObject { object, byte_offset: new_byte_offset };
        Some(ConstantValue::Pointer(ty, new_addr_const))
    } else {
        None
    }
}

fn evaluate_pointer_arithmetic(op: AstBinaryOp, lhs: ConstantValue, rhs: ConstantValue) -> Option<ConstantValue> {
    let valid_op = matches!(op, AstBinaryOp::Subtract);
    if !valid_op {
        return None;
    }

    let ConstantValue::Pointer(lhs_type, lhs_address_constant) = lhs else {
        return None;
    };

    let ConstantValue::Pointer(rhs_type, rhs_address_constant) = rhs else {
        return None;
    };

    if lhs_type != rhs_type {
        return None;
    }

    let AstAddressConstant::AddressOfObject { object: lhs_object, byte_offset: lhs_offset } = lhs_address_constant
    else {
        return None;
    };

    let AstAddressConstant::AddressOfObject { object: rhs_object, byte_offset: rhs_offset } = rhs_address_constant
    else {
        return None;
    };

    if lhs_object != rhs_object {
        return None;
    }

    let AstType::Pointer(referent) = &lhs_type else {
        return None;
    };

    let referent_bytes = (referent.bits() / 8) as i32;
    debug_assert!(referent_bytes > 0);

    let diff_bytes = lhs_offset - rhs_offset;
    let ptr_diff = diff_bytes / referent_bytes;

    Some(ConstantValue::make_int(ptr_diff))
}
