// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `constant_eval` module provides functionality to evaluate expressions at compile-time.

use std::convert::TryFrom;

use crate::ICE;
use crate::compiler_driver::Driver;
use crate::compiler_driver::Warning;
use crate::parser::{
    AstAddressConstant, AstBinaryOp, AstConstantFp, AstConstantInteger, AstConstantValue, AstDeclaredType,
    AstExpression, AstFloatLiteralKind, AstFullExpression, AstIntegerLiteralKind, AstMetadata, AstNodeId,
    AstStorageDuration, AstType, AstUnaryOp, AstUniqueName,
};

use super::constant_table::ConstantTable;
use super::symbol_table::{SymbolAttributes, SymbolTable};
use super::type_check::checker::TypeChecker;
use super::type_resolution;

// Future: sizeof, _Alignof

/// The context to use for constant expression evaluation.
pub struct ConstantEvalContext<'a, 'b> {
    metadata: &'a AstMetadata,
    symbols: &'a mut SymbolTable,
    constants: &'a mut ConstantTable,
    driver: &'b mut Driver,
}

impl<'a, 'b> ConstantEvalContext<'a, 'b> {
    /// Creates a new constant evaluation context.
    pub fn new(
        metadata: &'a AstMetadata,
        symbols: &'a mut SymbolTable,
        constants: &'a mut ConstantTable,
        driver: &'b mut Driver,
    ) -> Self {
        Self { metadata, symbols, constants, driver }
    }

    /// Creates the constant evaluation context from the given type checker.
    pub fn from_type_checker(chk: &'a mut TypeChecker, driver: &'b mut Driver) -> Self {
        Self { metadata: &chk.metadata, symbols: &mut chk.symbols, constants: &mut chk.constants, driver }
    }

    /// Gets a node's data type
    pub fn get_data_type(&self, node_id: &AstNodeId) -> AstType {
        match self.metadata.get_node_type(node_id) {
            Some(data_type) => data_type.clone(),
            None => ICE!("Data type not found for node {node_id}"),
        }
    }
}

/// Evaluates an AST full expression node at compile-time and produces value, or `None` if it cannot be evaluated.
///
/// There is no undefined behaviour when evaluating a constant expression. If UB would have occurred then `None` is
/// returned.
pub fn evaluate_constant_full_expr(
    full_expr: &AstFullExpression,
    mut ctx: ConstantEvalContext,
) -> Option<AstConstantValue> {
    evaluate_constant_expr(&full_expr.expr, &mut ctx)
}

/// Evaluates an AST expression node at compile-time and produces value, or `None` if it cannot be evaluated.
///
/// There is no undefined behaviour when evaluating a constant expression. If UB would have occurred then `None` is
/// returned.
pub fn evaluate_constant_expr(expr: &AstExpression, ctx: &mut ConstantEvalContext) -> Option<AstConstantValue> {
    let value = evaluate_const_expr_recursively(expr, ctx)?;

    match value {
        ConstantValue::Pointer(ty, init) => Some(AstConstantValue::Pointer(ty, init)),

        ConstantValue::Int { value: val, signed, size } => {
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

            let try_i8 = |value: i128| {
                if let Ok(i8) = i8::try_from(value) {
                    Some(AstConstantValue::Integer(AstConstantInteger::Char(i8)))
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

            let try_u8 = |value: i128| {
                if let Ok(u8) = u8::try_from(value) {
                    Some(AstConstantValue::Integer(AstConstantInteger::UnsignedChar(u8)))
                } else {
                    None
                }
            };

            match (signed, size) {
                (true, 64) => try_i64(val),
                (true, 32) => try_i32(val).or_else(|| try_i64(val)),
                (true, 16) => try_i16(val).or_else(|| try_i32(val)).or_else(|| try_i64(val)),
                (true, _) => try_i8(val).or_else(|| try_i16(val).or_else(|| try_i32(val)).or_else(|| try_i64(val))),

                (false, 64) => try_u64(val),
                (false, 32) => try_u32(val).or_else(|| try_u64(val)),
                (false, 16) => try_u16(val).or_else(|| try_u32(val)).or_else(|| try_u64(val)),
                (false, _) => try_u8(val).or_else(|| try_u16(val).or_else(|| try_u32(val)).or_else(|| try_u64(val))),
            }
        }

        ConstantValue::Float { value, size } => match size {
            32 => Some(AstConstantValue::Fp(AstConstantFp::Float(value as f32))),
            64 => Some(AstConstantValue::Fp(AstConstantFp::Double(value))),
            _ => unreachable!("Floating-point type should be 32-bit or 64-bit"),
        },

        ConstantValue::String(ascii) => Some(AstConstantValue::String { ascii }),
    }
}

fn evaluate_const_expr_recursively(expr: &AstExpression, ctx: &mut ConstantEvalContext) -> Option<ConstantValue> {
    match expr {
        // These expressions cannot be used in compile-time constant expressions.
        AstExpression::Identifier { .. } => None, // Future: C23 allows `constexpr` variables.
        AstExpression::FunctionCall { .. } => None, // Future: `constexpr` functions.
        AstExpression::Deref { .. } => None,
        AstExpression::Assignment { .. } => None,
        AstExpression::Subscript { .. } => None,

        // Literals
        AstExpression::CharLiteral { value, .. } => Some(ConstantValue::make_int(*value)),
        AstExpression::StringLiteral { ascii, .. } => Some(ConstantValue::String(ascii.clone())),
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

        // Other expressions
        AstExpression::AddressOf { .. } => evaluate_address_of(expr, ctx),
        AstExpression::Cast { target_type, expr, .. } => evaluate_cast(target_type, expr, ctx),
        AstExpression::UnaryOperation { op, expr, .. } => evaluate_unary_operation(op, expr, ctx),
        AstExpression::BinaryOperation { op, left, right, .. } => evaluate_binary_operation(op, left, right, ctx),
        AstExpression::Conditional { expr, consequent, alternative, .. } => {
            evaluate_conditional(expr, consequent, alternative, ctx)
        }
    }
}

fn evaluate_address_of(expr: &AstExpression, ctx: &mut ConstantEvalContext) -> Option<ConstantValue> {
    let AstExpression::AddressOf { node_id, expr } = expr else {
        ICE!("Expected AstExpression::AddressOf");
    };

    let ptr_type = ctx.get_data_type(node_id);

    let init = evaluate_address_constant(expr, ctx)?;

    Some(ConstantValue::Pointer(ptr_type, init))
}

fn evaluate_address_constant(expr: &AstExpression, ctx: &mut ConstantEvalContext) -> Option<AstAddressConstant> {
    match expr {
        // A static storage pointer can be initialized with the address of an object of static storage duration,
        // or a function designator.
        //
        AstExpression::Identifier { unique_name, .. } => {
            let symbol = ctx.symbols.get(unique_name).expect("Symbol should exist");

            if symbol.data_type.is_function() {
                Some(AstAddressConstant::AddressOfFunction(unique_name.to_string()))
            } else if symbol.storage_duration() == AstStorageDuration::Static {
                Some(AstAddressConstant::AddressOfObject { object: unique_name.to_string(), byte_offset: 0 })
            } else {
                None
            }
        }

        AstExpression::AddressOf { expr, .. } => evaluate_address_constant(expr, ctx),

        // A static storage pointer can be initialized with the address of an array element.
        //
        AstExpression::Subscript { node_id, expr1, expr2, .. } => {
            let expr1_t = ctx.get_data_type(&expr1.node_id());
            let expr2_t = ctx.get_data_type(&expr2.node_id());
            debug_assert!(expr1_t.is_pointer() || expr2_t.is_pointer());

            let (ptr_expr, int_expr) = if expr1_t.is_pointer() { (expr1, expr2) } else { (expr2, expr1) };

            let subscript_identifier = evaluate_address_constant(ptr_expr, ctx)?;
            let subscript_index_value = evaluate_constant_expr(int_expr, ctx)?;

            if !matches!(subscript_index_value, AstConstantValue::Integer(_)) {
                return None;
            }

            let subscript_index_value = subscript_index_value.cast_to(&AstType::Int);

            let AstConstantValue::Integer(subscript_index) = subscript_index_value else {
                return None;
            };

            let AstConstantInteger::Int(subscript_index) = subscript_index else {
                return None;
            };

            if let AstAddressConstant::AddressOfObject { object, .. } = subscript_identifier {
                let Some(symbol) = ctx.symbols.get(AstUniqueName::new(object.clone())) else {
                    ICE!("Symbol should exist for '{object}'");
                };

                // Validate the subscript index. It's valid to create a pointer to 1-past the last element in
                // the array, but no further.
                //          int arr[4];
                //          int *p1 = &arr[4]; // OK (unless we dereference it)
                //          int *p2 = &arr[5]; // Warn
                //
                if let AstType::Array { count, .. } = symbol.data_type
                    && (subscript_index < 0 || (subscript_index > 0 && subscript_index as usize > count))
                {
                    let loc = ctx.metadata.get_source_location(&int_expr.node_id());
                    Warning::array_index_out_of_bounds(subscript_index, &symbol.data_type, loc, ctx.driver);
                }

                let subscript_expr_type = ctx.get_data_type(node_id);
                let element_bytes = (subscript_expr_type.bits() / 8) as i32;
                let byte_offset = subscript_index * element_bytes;

                Some(AstAddressConstant::AddressOfObject { object, byte_offset })
            } else {
                None
            }
        }

        // A static storage pointer can be initialized with a string literal.
        //
        AstExpression::StringLiteral { node_id: literal_node_id, ascii, .. } => {
            // Add the string literal to the constant table (may already exist).
            let mut constant_string = ascii.join("");
            constant_string.push_str("\\000"); // Append NULL
            let constant_idx = ctx.constants.add_string(&constant_string);

            // Add the constant string to the symbol table (may already exist).
            let loc = ctx.metadata.get_source_location(literal_node_id);
            let attrs = SymbolAttributes::constant(loc);
            let const_name = ctx.constants.make_const_symbol_name(constant_idx);
            let lit_data_type = ctx.get_data_type(literal_node_id);
            let unique_name = AstUniqueName::new(const_name.clone());
            _ = ctx.symbols.add(unique_name, lit_data_type, attrs);

            Some(AstAddressConstant::AddressOfObject { object: const_name, byte_offset: 0 })
        }

        _ => None,
    }
}

fn evaluate_cast(
    declared_type: &AstDeclaredType,
    expr: &AstExpression,
    ctx: &mut ConstantEvalContext,
) -> Option<ConstantValue> {
    let value = evaluate_const_expr_recursively(expr, ctx)?;

    let ast_type = type_resolution::resolve_declared_type(declared_type, None, None).ok()?;

    value.cast_to(&ast_type)
}

fn evaluate_unary_operation(
    op: &AstUnaryOp,
    expr: &AstExpression,
    ctx: &mut ConstantEvalContext,
) -> Option<ConstantValue> {
    let value = evaluate_const_expr_recursively(expr, ctx)?;
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
    ctx: &mut ConstantEvalContext,
) -> Option<ConstantValue> {
    let lhs = evaluate_const_expr_recursively(left, ctx)?;
    let rhs = evaluate_const_expr_recursively(right, ctx)?;

    let common_type = lhs.get_common_type(&rhs)?;

    if common_type.is_pointer() {
        lhs.binary_op(op, rhs)
    } else {
        let lhs = lhs.cast_to(&common_type)?;
        let rhs = rhs.cast_to(&common_type)?;

        lhs.binary_op(op, rhs)
    }
}

fn evaluate_conditional(
    expr: &AstExpression,
    consequent: &AstExpression,
    alternative: &AstExpression,
    ctx: &mut ConstantEvalContext,
) -> Option<ConstantValue> {
    // We need do determine the common type of both `consequent` and `alternative` expressions, in case they have
    // different types, so that we can cast them to the common type.
    //      E.g. `11 < 2 ? 1.0 : 2.0f`  -->  cast both to 'double'
    //
    // So we evaluate both expressions in order to get their types. It's safe to evaluate both consequent and
    // alternative at compile-time because there can be no side-effects, even though strictly only the `true`
    // case expression should be evaluated.
    //
    let consequent = evaluate_const_expr_recursively(consequent, ctx)?;
    let alternative = evaluate_const_expr_recursively(alternative, ctx)?;

    let common_type = consequent.get_common_type(&alternative)?;
    let consequent = consequent.cast_to(&common_type)?;
    let alternative = alternative.cast_to(&common_type)?;

    match evaluate_const_expr_recursively(expr, ctx) {
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
    Pointer(AstType, AstAddressConstant),
    String(Vec<String>),
}

impl ConstantValue {
    /// Creates a 'char' integer constant value.
    pub fn make_char(value: i8) -> Self {
        ConstantValue::Int { value: value as i128, signed: true, size: 8 }
    }

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

    /// Creates an 'unsigned char' integer constant value.
    pub fn make_uchar(value: u8) -> Self {
        ConstantValue::Int { value: value as i128, signed: false, size: 8 }
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
    pub fn cast_to(self, cast_to_type: &AstType) -> Option<Self> {
        macro_rules! make_cast {
            ($value:expr) => {
                match cast_to_type {
                    AstType::Char | AstType::SignedChar => Some(Self::make_char($value as i8)),
                    AstType::UnsignedChar => Some(Self::make_uchar($value as u8)),
                    AstType::Short => Some(Self::make_short($value as i16)),
                    AstType::UnsignedShort => Some(Self::make_ushort($value as u16)),
                    AstType::Int => Some(Self::make_int($value as i32)),
                    AstType::UnsignedInt => Some(Self::make_uint($value as u32)),
                    AstType::Long | AstType::LongLong => Some(Self::make_long($value as i64)),
                    AstType::UnsignedLong | AstType::UnsignedLongLong => Some(Self::make_ulong($value as u64)),
                    AstType::Float => Some(Self::make_float($value as f32)),
                    AstType::Double => Some(Self::make_double($value as f64)),
                    AstType::Pointer(_) => {
                        let init = if $value as u64 == 0 {
                            AstAddressConstant::NullPointer
                        } else {
                            AstAddressConstant::CastExpression($value as u64)
                        };

                        Some(ConstantValue::Pointer(cast_to_type.clone(), init))
                    }
                    _ => None,
                }
            };
        }

        match self {
            ConstantValue::Int { value, .. } => make_cast!(value),
            ConstantValue::Float { value, .. } => make_cast!(value),
            ConstantValue::Pointer(_, init) => Some(ConstantValue::Pointer(cast_to_type.clone(), init)),
            ConstantValue::String(_) => {
                if let AstType::Pointer(referent) = cast_to_type
                    && (referent.as_ref() == &AstType::Char || referent.as_ref() == &AstType::UnsignedChar)
                {
                    Some(self)
                } else {
                    None
                }
            }
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
            if self.is_pointer() == other.is_pointer() && self.get_pointer_type() == other.get_pointer_type() {
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
            (true, 8) => AstType::Char,
            (true, 16) => AstType::Short,
            (true, 32) => AstType::Int,
            (true, 64) => AstType::Long,
            (false, 8) => AstType::UnsignedChar,
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
        // Check for pointer arithmetic first.
        //
        if self.is_pointer() && other.is_integer() {
            return evaluate_pointer_integer_arithmetic(*op, self, other);
        } else if self.is_integer() && other.is_pointer() {
            return evaluate_pointer_integer_arithmetic(*op, other, self);
        } else if self.is_pointer() && other.is_pointer() {
            return evaluate_pointer_arithmetic(*op, self, other);
        } else if self.is_pointer() || other.is_pointer() {
            return None;
        }

        if let ConstantValue::Int { value: lhs, signed, size } = self {
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
            }
        } else if let ConstantValue::Float { value: lhs, size: lhs_size } = self {
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
            }
        } else {
            None
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
