// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `constant_eval` module provides functionality to evaluate expressions at compile-time.

use super::type_check::checker::TypeChecker;
use super::type_resolution;

use crate::ICE;
use crate::parser::{
    AstBinaryOp, AstConstantFp, AstConstantInteger, AstConstantPtrInitializer, AstConstantValue, AstDeclaredType,
    AstExpression, AstFloatLiteralKind, AstFullExpression, AstIntegerLiteralKind, AstStorageDuration, AstType,
    AstUnaryOp,
};

use std::convert::TryFrom;

// TODO: sizeof, _Alignof

/// Evaluates an AST full expression node at compile-time and produces value, or `None` if it cannot be evaluated.
///
/// There is no undefined behaviour when evaluating a constant expression. If UB would have occurred then `None` is
/// returned.
pub fn evaluate_constant_full_expr(
    full_expr: &AstFullExpression,
    chk: Option<&TypeChecker>,
) -> Option<AstConstantValue> {
    evaluate_constant_expr(&full_expr.expr, chk)
}

/// Evaluates an AST fl expression node at compile-time and produces value, or `None` if it cannot be evaluated.
///
/// There is no undefined behaviour when evaluating a constant expression. If UB would have occurred then `None` is
/// returned.
pub fn evaluate_constant_expr(expr: &AstExpression, chk: Option<&TypeChecker>) -> Option<AstConstantValue> {
    let value = evaluate_const_expr_recursively(expr, chk)?;

    match value {
        ConstantValue::Pointer(ty, init) => Some(AstConstantValue::Pointer(ty, init)),

        ConstantValue::Int { value, signed, size } => {
            let try_i64 = |value: i128| {
                if let Ok(i64) = i64::try_from(value) {
                    Some(AstConstantValue::Integer(AstConstantInteger::LongLong(i64)))
                } else {
                    None
                }
            };

            let try_i32 = |value: i128| {
                if let Ok(i32) = i32::try_from(value) {
                    Some(AstConstantValue::Integer(AstConstantInteger::Int(i32)))
                } else {
                    None
                }
            };

            let try_i16 = |value: i128| {
                if let Ok(i16) = i16::try_from(value) {
                    Some(AstConstantValue::Integer(AstConstantInteger::Short(i16)))
                } else {
                    None
                }
            };

            let try_u64 = |value: i128| {
                if let Ok(u64) = u64::try_from(value) {
                    Some(AstConstantValue::Integer(AstConstantInteger::UnsignedLongLong(u64)))
                } else {
                    None
                }
            };

            let try_u32 = |value: i128| {
                if let Ok(u32) = u32::try_from(value) {
                    Some(AstConstantValue::Integer(AstConstantInteger::UnsignedInt(u32)))
                } else {
                    None
                }
            };

            let try_u16 = |value: i128| {
                if let Ok(u16) = u16::try_from(value) {
                    Some(AstConstantValue::Integer(AstConstantInteger::UnsignedShort(u16)))
                } else {
                    None
                }
            };

            match (signed, size) {
                (true, 64) => try_i64(value),
                (true, 32) => try_i32(value).or_else(|| try_i64(value)),
                (true, _) => try_i16(value).or_else(|| try_i32(value)).or_else(|| try_i64(value)),

                (false, 64) => try_u64(value),
                (false, 32) => try_u32(value).or_else(|| try_u64(value)),
                (false, _) => try_u16(value).or_else(|| try_u32(value)).or_else(|| try_u64(value)),
            }
        }

        ConstantValue::Float { value, size } => match size {
            32 => Some(AstConstantValue::Fp(AstConstantFp::Float(value as f32))),
            64 => Some(AstConstantValue::Fp(AstConstantFp::Double(value))),
            _ => unreachable!("Floating-point type should be 32-bit or 64-bit"),
        },
    }
}

fn evaluate_const_expr_recursively(expr: &AstExpression, chk: Option<&TypeChecker>) -> Option<ConstantValue> {
    match expr {
        AstExpression::Variable { .. } => None, // Future: C23 allows `constexpr` variables.
        AstExpression::FunctionCall { .. } => None, // Future: `constexpr` functions.
        AstExpression::Deref { .. } => None,
        AstExpression::AddressOf { node_id, expr } => {
            if let Some(chk) = chk
                && let AstExpression::Variable { unique_name, .. } = expr.as_ref()
            {
                let symbol = chk.symbols.get(unique_name).expect("Variable should exist in symbol table");

                if symbol.storage_duration() == AstStorageDuration::Static {
                    let ptr_type = chk.get_data_type(node_id);

                    let init = AstConstantPtrInitializer::AddressConstant {
                        symbol: unique_name.clone(),
                    };

                    Some(ConstantValue::Pointer(ptr_type, init))
                } else {
                    None
                }
            } else {
                None
            }
        }
        AstExpression::Assignment { .. } => None,
        AstExpression::Cast { target_type, expr, .. } => evaluate_cast(target_type, expr, chk),
        AstExpression::IntegerLiteral { value, kind, .. } => match kind {
            AstIntegerLiteralKind::Int => Some(ConstantValue::make_int(*value as i32)),
            AstIntegerLiteralKind::Long | AstIntegerLiteralKind::LongLong => {
                Some(ConstantValue::make_long(*value as i64))
            }
            AstIntegerLiteralKind::UnsignedInt => Some(ConstantValue::make_uint(*value as u32)),
            AstIntegerLiteralKind::UnsignedLong | AstIntegerLiteralKind::UnsignedLongLong => {
                Some(ConstantValue::make_ulong(*value))
            }
        },
        AstExpression::FloatLiteral { value, kind, .. } => match kind {
            AstFloatLiteralKind::Float => Some(ConstantValue::make_float(*value as f32)),
            AstFloatLiteralKind::Double | AstFloatLiteralKind::LongDouble => Some(ConstantValue::make_double(*value)),
        },
        AstExpression::UnaryOperation { op, expr, .. } => evaluate_unary_operation(op, expr, chk),
        AstExpression::BinaryOperation { op, left, right, .. } => evaluate_binary_operation(op, left, right, chk),
        AstExpression::Conditional { expr, consequent, alternative, .. } => {
            evaluate_conditional(expr, consequent, alternative, chk)
        }
    }
}

fn evaluate_cast(
    declared_type: &AstDeclaredType,
    expr: &AstExpression,
    chk: Option<&TypeChecker>,
) -> Option<ConstantValue> {
    let value = evaluate_const_expr_recursively(expr, chk)?;

    let ast_type = type_resolution::resolve_declared_type(declared_type, None, None).ok()?;

    value.cast_to(&ast_type)
}

fn evaluate_unary_operation(op: &AstUnaryOp, expr: &AstExpression, chk: Option<&TypeChecker>) -> Option<ConstantValue> {
    let value = evaluate_const_expr_recursively(expr, chk)?;
    match op {
        AstUnaryOp::Negate => Some(value.negate()),
        AstUnaryOp::Plus => Some(value),
        AstUnaryOp::BitwiseNot => value.bitwise_not(),
        AstUnaryOp::LogicalNot => match value {
            v if v.is_zero() => Some(ConstantValue::make_int(1)),
            _ => Some(ConstantValue::make_int(0)),
        },
        _ => None,
    }
}

fn evaluate_binary_operation(
    op: &AstBinaryOp,
    left: &AstExpression,
    right: &AstExpression,
    chk: Option<&TypeChecker>,
) -> Option<ConstantValue> {
    let lhs = evaluate_const_expr_recursively(left, chk)?;
    let rhs = evaluate_const_expr_recursively(right, chk)?;

    let common_type = lhs.get_common_type(&rhs)?;
    let lhs = lhs.cast_to(&common_type)?;
    let rhs = rhs.cast_to(&common_type)?;

    lhs.binary_op(op, rhs)
}

fn evaluate_conditional(
    expr: &AstExpression,
    consequent: &AstExpression,
    alternative: &AstExpression,
    chk: Option<&TypeChecker>,
) -> Option<ConstantValue> {
    // We need do determine the common type of both `consequent` and `alternative` expressions, in case they have
    // different types, so that we can cast them to the common type.
    //      E.g. `11 < 2 ? 1.0 : 2.0f`  -->  cast both to 'double'
    //
    // So we evaluate both expressions in order to get their types. It's safe to evaluate both consequent and
    // alternative at compile-time because there can be no side-effects, even though strictly only the `true`
    // case expression should be evaluated.
    //
    let consequent = evaluate_const_expr_recursively(consequent, chk)?;
    let alternative = evaluate_const_expr_recursively(alternative, chk)?;

    let common_type = consequent.get_common_type(&alternative)?;
    let consequent = consequent.cast_to(&common_type)?;
    let alternative = alternative.cast_to(&common_type)?;

    match evaluate_const_expr_recursively(expr, chk) {
        v if v.as_ref().is_some_and(|v| v.is_zero()) => Some(alternative),
        Some(_) => Some(consequent),
        None => None,
    }
}

/// `ConstantValue` is used internally as the data type for evaluating constant expressions.
///
/// All integer values, regardless of signedness or size, are stored in an `i128`, and all floating-point
/// values are stored in an `f64`.
#[derive(Debug, Clone, PartialEq)]
enum ConstantValue {
    Int { value: i128, signed: bool, size: usize },
    Float { value: f64, size: usize },
    Pointer(AstType, AstConstantPtrInitializer),
}

impl ConstantValue {
    /// Creates a 'short' integer constant value.
    pub fn make_short(value: i16) -> Self {
        ConstantValue::Int { value: value as i128, signed: true, size: 16 }
    }

    /// Creates an 'int' constant value.
    pub fn make_int(value: i32) -> Self {
        ConstantValue::Int { value: value as i128, signed: true, size: 32 }
    }

    /// Creates a 'long' integer constant value.
    pub fn make_long(value: i64) -> Self {
        ConstantValue::Int { value: value as i128, signed: true, size: 64 }
    }

    /// Creates an 'unsigned short' integer constant value.
    pub fn make_ushort(value: u16) -> Self {
        ConstantValue::Int { value: value as i128, signed: false, size: 16 }
    }

    /// Creates an 'unsigned int' constant value.
    pub fn make_uint(value: u32) -> Self {
        ConstantValue::Int { value: value as i128, signed: false, size: 32 }
    }

    /// Creates an 'unsigned long' integer constant value.
    pub fn make_ulong(value: u64) -> Self {
        ConstantValue::Int { value: value as i128, signed: false, size: 64 }
    }

    /// Creates a 'float' 32-bit floating-point constant value.
    pub fn make_float(value: f32) -> Self {
        ConstantValue::Float { value: value as f64, size: 32 }
    }

    /// Creates a 'double' 64-bit floating-point constant value.
    pub fn make_double(value: f64) -> Self {
        ConstantValue::Float { value, size: 64 }
    }

    /// Consumes the constant value and returns a `Some<ConstantValue>` version casted to the given `AstType`,
    /// or returns `None` if the cast is not possible.
    pub fn cast_to(self, data_type: &AstType) -> Option<Self> {
        macro_rules! make_cast {
            ($value:expr) => {
                match data_type {
                    AstType::Short => Some(Self::make_short($value as i16)),
                    AstType::UnsignedShort => Some(Self::make_ushort($value as u16)),
                    AstType::Int => Some(Self::make_int($value as i32)),
                    AstType::UnsignedInt => Some(Self::make_uint($value as u32)),
                    AstType::Long | AstType::LongLong => Some(Self::make_long($value as i64)),
                    AstType::UnsignedLong | AstType::UnsignedLongLong => Some(Self::make_ulong($value as u64)),
                    AstType::Float => Some(Self::make_float($value as f32)),
                    AstType::Double => Some(Self::make_double($value as f64)),
                    AstType::Pointer(_) => {
                        let init = AstConstantPtrInitializer::CastExpression($value as u64);
                        Some(ConstantValue::Pointer(data_type.clone(), init))
                    }
                    _ => None,
                }
            };
        }

        match self {
            ConstantValue::Int { value, .. } => make_cast!(value),
            ConstantValue::Float { value, .. } => make_cast!(value),
            ConstantValue::Pointer(_, init) => Some(ConstantValue::Pointer(data_type.clone(), init)),
        }
    }

    /// Is the current value equal to zero?
    pub fn is_zero(&self) -> bool {
        match self {
            ConstantValue::Int { value, .. } => *value == 0,
            ConstantValue::Float { value, .. } => *value == 0.0,
            _ => false,
        }
    }

    /// Is the current value of an integer type?
    pub fn is_integer(&self) -> bool {
        matches!(self, ConstantValue::Int { .. })
    }

    /// Is the current value of a pointer type?
    pub fn is_pointer(&self) -> bool {
        matches!(self, ConstantValue::Pointer(..))
    }

    /// Gets the `AstType` if this value is a pointer type, or returns `None`.
    pub fn get_pointer_type(&self) -> Option<AstType> {
        if let ConstantValue::Pointer(ty, _) = self { Some(ty.clone()) } else { None }
    }

    /// Gets the common `AstType` for this value and the given other constant value.
    pub fn get_common_type(&self, other: &ConstantValue) -> Option<AstType> {
        // If either is floating-point then the common type is floating-point.
        //
        let this_is_fp = matches!(self, ConstantValue::Float { .. });
        let other_is_fp = matches!(other, ConstantValue::Float { .. });

        if self.is_pointer() || other.is_pointer() {
            if self == other {
                return self.get_pointer_type();
            }

            if self.is_pointer() && other.is_integer() {
                return self.get_pointer_type();
            }

            if other.is_pointer() && self.is_integer() {
                return other.get_pointer_type();
            }

            return None;
        }

        if this_is_fp || other_is_fp {
            let mut common_fp_size = 0;

            if let ConstantValue::Float { size, .. } = self {
                common_fp_size = *size;
            }

            if let ConstantValue::Float { size, .. } = other
                && *size > common_fp_size
            {
                common_fp_size = *size;
            }

            return match common_fp_size {
                32 => Some(AstType::Float),
                64 => Some(AstType::Double),
                _ => ICE!("Floating-point size should be 32 or 64"),
            };
        }

        let ConstantValue::Int { signed: this_signed, size: this_size, .. } = self else {
            ICE!("Expected ConstantValue::Int");
        };

        let ConstantValue::Int { signed: other_signed, size: other_size, .. } = other else {
            ICE!("Expected ConstantValue::Int");
        };

        let common_signed = *this_signed && *other_signed;
        let common_size = std::cmp::max(*this_size, *other_size);

        let ty = match (common_signed, common_size) {
            (true, 16) => AstType::Short,
            (true, 32) => AstType::Int,
            (true, 64) => AstType::Long,
            (false, 16) => AstType::UnsignedShort,
            (false, 32) => AstType::UnsignedInt,
            (false, 64) => AstType::UnsignedLong,
            _ => ICE!("Did not handle '{common_signed}' '{common_size}'"),
        };

        Some(ty)
    }

    /// Negates the value.
    pub fn negate(self) -> Self {
        match self {
            ConstantValue::Int { value, size, .. } => ConstantValue::Int { value: -value, signed: true, size },
            ConstantValue::Float { value, size } => ConstantValue::Float { value: -value, size },
            _ => ICE!("Cannot negate"),
        }
    }

    /// Performs a bitwise not / complement.
    pub fn bitwise_not(self) -> Option<Self> {
        match self {
            ConstantValue::Int { value, signed, size } => Some(ConstantValue::Int { value: !value, signed, size }),
            ConstantValue::Float { .. } => None,
            _ => ICE!("Cannot apply bitwise not"),
        }
    }

    /// Performs the given binary operation with the current value and the given other value.
    pub fn binary_op(self, op: &AstBinaryOp, other: ConstantValue) -> Option<Self> {
        if let ConstantValue::Int { value: lhs, signed, size } = self {
            if other.is_pointer() {
                // TODO
                return None;
            }

            let ConstantValue::Int { value: rhs, .. } = other else {
                ICE!("Expected other to be ConstantValue::Int");
            };

            match op {
                // Arithmetic
                AstBinaryOp::Add => lhs.checked_add(rhs).map(|value| ConstantValue::Int { value, signed, size }),
                AstBinaryOp::Subtract => lhs.checked_sub(rhs).map(|value| ConstantValue::Int { value, signed, size }),
                AstBinaryOp::Multiply => lhs.checked_mul(rhs).map(|value| ConstantValue::Int { value, signed, size }),
                AstBinaryOp::Divide => lhs.checked_div(rhs).map(|value| ConstantValue::Int { value, signed, size }),
                AstBinaryOp::Remainder => lhs.checked_rem(rhs).map(|value| ConstantValue::Int { value, signed, size }),

                // Bitwise
                AstBinaryOp::BitwiseAnd => Some(lhs & rhs).map(|value| ConstantValue::Int { value, signed, size }),
                AstBinaryOp::BitwiseOr => Some(lhs | rhs).map(|value| ConstantValue::Int { value, signed, size }),
                AstBinaryOp::BitwiseXor => Some(lhs ^ rhs).map(|value| ConstantValue::Int { value, signed, size }),
                AstBinaryOp::LeftShift => match rhs {
                    0 => Some(ConstantValue::Int { value: lhs, signed, size }),
                    n if n < 0 => None,
                    _ => lhs.checked_shl(rhs as u32).map(|value| ConstantValue::Int { value, signed, size }),
                },
                AstBinaryOp::RightShift => match rhs {
                    0 => Some(ConstantValue::Int { value: lhs, signed, size }),
                    n if n < 0 => None,
                    _ => lhs.checked_shr(rhs as u32).map(|value| ConstantValue::Int { value, signed, size }),
                },

                // Logical
                AstBinaryOp::LogicalAnd => Some(Self::make_int((lhs != 0 && rhs != 0) as i32)),
                AstBinaryOp::LogicalOr => Some(Self::make_int((lhs != 0 || rhs != 0) as i32)),

                // Comparison
                AstBinaryOp::EqualTo => Some(Self::make_int((lhs == rhs) as i32)),
                AstBinaryOp::NotEqualTo => Some(Self::make_int((lhs != rhs) as i32)),
                AstBinaryOp::LessThan => Some(Self::make_int((lhs < rhs) as i32)),
                AstBinaryOp::LessThanOrEqualTo => Some(Self::make_int((lhs <= rhs) as i32)),
                AstBinaryOp::GreaterThan => Some(Self::make_int((lhs > rhs) as i32)),
                AstBinaryOp::GreaterThanOrEqualTo => Some(Self::make_int((lhs >= rhs) as i32)),

                _ => None,
            }
        } else if let ConstantValue::Float { value: lhs, size: lhs_size } = self {
            if other.is_pointer() {
                return None;
            }

            let ConstantValue::Float { value: rhs, size: rhs_size } = other else {
                ICE!("Expected other to be ConstantValue::Float");
            };

            debug_assert!(lhs_size == rhs_size);

            let make_fp = |value: f64, size: usize| ConstantValue::Float { value, size };

            match op {
                // Arithmetic
                AstBinaryOp::Add => Some(make_fp(lhs + rhs, lhs_size)),
                AstBinaryOp::Subtract => Some(make_fp(lhs - rhs, lhs_size)),
                AstBinaryOp::Multiply => Some(make_fp(lhs * rhs, lhs_size)),
                AstBinaryOp::Divide => Some(make_fp(lhs / rhs, lhs_size)),
                AstBinaryOp::Remainder => None,

                // Bitwise
                AstBinaryOp::BitwiseAnd
                | AstBinaryOp::BitwiseOr
                | AstBinaryOp::BitwiseXor
                | AstBinaryOp::LeftShift
                | AstBinaryOp::RightShift => None,

                // Logical
                AstBinaryOp::LogicalAnd => Some(Self::make_int((lhs != 0.0 && rhs != 0.0) as i32)),
                AstBinaryOp::LogicalOr => Some(Self::make_int((lhs != 0.0 || rhs != 0.0) as i32)),

                // Comparison
                AstBinaryOp::EqualTo => Some(Self::make_int((lhs == rhs) as i32)),
                AstBinaryOp::NotEqualTo => Some(Self::make_int((lhs != rhs) as i32)),
                AstBinaryOp::LessThan => Some(Self::make_int((lhs < rhs) as i32)),
                AstBinaryOp::LessThanOrEqualTo => Some(Self::make_int((lhs <= rhs) as i32)),
                AstBinaryOp::GreaterThan => Some(Self::make_int((lhs > rhs) as i32)),
                AstBinaryOp::GreaterThanOrEqualTo => Some(Self::make_int((lhs >= rhs) as i32)),

                _ => None,
            }
        } else if let ConstantValue::Pointer(..) = self {
            // TODO
            None

            /*if other.is_pointer() {
                return None;
            }

            if !matches!(other, ConstantValue::Int { value, .. }) {
                return None;
            }

            let valid_op = matches!(op, AstBinaryOp::Add | AstBinaryOp::Subtract);
            if !valid_op {
                return None;
            }

            let init = match init {
                AstConstantPtrInitializer::NullPointerConstant => {
                    return None;
                }
                AstConstantPtrInitializer::CastExpression(_) => todo!(),
                AstConstantPtrInitializer::AddressConstant { object } => todo!(),
            };

            Some(ConstantValue::Pointer(ty, init))*/
        } else {
            ICE!("Unhandled ConstantValue");
        }
    }
}
