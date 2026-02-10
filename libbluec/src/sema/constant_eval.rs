// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `constant_eval` module provides functionality to evaluate expressions at compile-time.

mod binary_expr;

use std::convert::TryFrom;

use crate::ICE;
use crate::compiler_driver::{Driver, Warning};
use crate::core::SourceLocation;
use crate::parser::{
    AstAddressConstant, AstConstantFp, AstConstantInteger, AstConstantValue, AstExpression, AstExpressionFlag,
    AstFloatLiteralKind, AstFullExpression, AstIntegerLiteralKind, AstStorageDuration, AstType, AstUnaryOp,
    AstUniqueName,
};

use super::symbol_table::SymbolAttributes;
use super::type_check::TypeChecker;
use super::type_resolution;

// Future: sizeof, _Alignof

/// A constant expression evaluator.
pub struct Eval<'a, 'b> {
    chk: &'a mut TypeChecker,
    driver: &'b mut Driver,
    emit_diagnostics: bool,
    root_expression_sloc: SourceLocation,
}

impl<'a, 'b> Eval<'a, 'b> {
    /// Creates a constant expression evaluator.
    pub fn new(chk: &'a mut TypeChecker, driver: &'b mut Driver) -> Self {
        Self { chk, driver, emit_diagnostics: true, root_expression_sloc: SourceLocation::none() }
    }

    /// Sets whether the evaluator should emit warning/error diagnostics during evaluation.
    #[cfg(test)]
    pub fn set_diagnostics_enabled(&mut self, enable: bool) {
        self.emit_diagnostics = enable;
    }

    /// Evaluates an AST full expression node at compile-time and returns `Some` with an [AstConstantValue], or `None`
    /// if the expression cannot be evaluated.
    ///
    /// There is no undefined behaviour when evaluating a constant expression. If UB would have occurred then `None` is
    /// returned.
    pub fn evaluate_full_expr(&mut self, full_expr: &AstFullExpression) -> Option<AstConstantValue> {
        if self.emit_diagnostics {
            self.root_expression_sloc = self.chk.metadata.get_source_location(&full_expr.node_id);
        }
        evaluate_constant_expr(&full_expr.expr, self)
    }

    /// Evaluates an AST expression node at compile-time and returns `Some` with an [AstConstantValue], or `None` if the
    /// expression cannot be evaluated.
    ///
    /// There is no undefined behaviour when evaluating a constant expression. If UB would have occurred then `None` is
    /// returned.
    pub fn evaluate_expr(&mut self, expr: &AstExpression) -> Option<AstConstantValue> {
        if self.emit_diagnostics {
            self.root_expression_sloc = self.chk.metadata.get_source_location(&expr.node_id());
        }
        evaluate_constant_expr(expr, self)
    }
}

/// Evaluates an AST expression node at compile-time and returns `Some` with an [AstConstantValue], or `None` if the
/// expression cannot be evaluated.
///
/// There is no undefined behaviour when evaluating a constant expression. If UB would have occurred then `None` is
/// returned.
fn evaluate_constant_expr(expr: &AstExpression, eval: &mut Eval) -> Option<AstConstantValue> {
    let value = evaluate_const_expr_recursively(expr, eval)?;

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

fn evaluate_const_expr_recursively(expr: &AstExpression, eval: &mut Eval) -> Option<ConstantValue> {
    match expr {
        // A compound assignment expression is not itself a constant expression (because it assigns to an l-value).
        // But if the `rhs` expression is a constant expression then we still want to validate the implicit cast from
        // the `rhs` expression to the `lhs` l-value.
        // (The sema type checker does not add the implicit cast to the type-checked AST, and instead we add the cast
        // in IR lowering after translating the compound assignment expression.)
        //
        // E.g.
        //      int i = 0;
        //      i += 1.1f    // We want a warning to be emitted.
        //
        // So we evaluate the `rhs` constant expression here and then perform the implicit cast ourselves to a
        // temporary, which means any diagnostics will be emitted for us.
        //
        AstExpression::Assignment { op, lhs, rhs, .. }
            if op.is_compound_assignment()
                && eval.chk.metadata.is_expr_flag_set(rhs.node_id(), AstExpressionFlag::IsConstant) =>
        {
            if let Some(rhs_value) = evaluate_const_expr_recursively(rhs, eval) {
                let lhs_type = eval.chk.get_data_type(&lhs.node_id());

                // We want any diagnostics to point to the `rhs` expression.
                let orig_sloc = std::mem::replace(
                    &mut eval.root_expression_sloc,
                    eval.chk.metadata.get_source_location(&rhs.node_id()),
                );

                _ = rhs_value.cast_to(&lhs_type, true, eval); // Ignore the result, we only want the diagnostics

                eval.root_expression_sloc = orig_sloc;
            }

            None // Not a constant expression
        }

        // These expressions cannot be used in compile-time constant expressions.
        AstExpression::Identifier { .. } => None, // Future: C23 allows `constexpr` variables.
        AstExpression::FunctionCall { .. } => None, // Future: `constexpr` functions.
        AstExpression::Deref { .. } => None,
        AstExpression::Subscript { .. } => None,
        AstExpression::Assignment { .. } => None,

        // Literals
        AstExpression::CharLiteral { value, .. } => Some(ConstantValue::from(*value)),
        AstExpression::StringLiteral { ascii, .. } => Some(ConstantValue::String(ascii.clone())),
        AstExpression::IntegerLiteral { value, kind, .. } => match kind {
            AstIntegerLiteralKind::Int => Some(ConstantValue::from(*value as i32)),
            AstIntegerLiteralKind::Long | AstIntegerLiteralKind::LongLong => Some(ConstantValue::from(*value as i64)),
            AstIntegerLiteralKind::UnsignedInt => Some(ConstantValue::from(*value as u32)),
            AstIntegerLiteralKind::UnsignedLong | AstIntegerLiteralKind::UnsignedLongLong => {
                Some(ConstantValue::from(*value))
            }
        },
        AstExpression::FloatLiteral { value, kind, .. } => match kind {
            AstFloatLiteralKind::Float => Some(ConstantValue::from(*value as f32)),
            AstFloatLiteralKind::Double | AstFloatLiteralKind::LongDouble => Some(ConstantValue::from(*value)),
        },

        // Other expressions
        AstExpression::AddressOf { .. } => evaluate_address_of(expr, eval),
        AstExpression::Cast { .. } => evaluate_cast(expr, eval),
        AstExpression::UnaryOperation { op, expr, .. } => evaluate_unary_operation(op, expr, eval),
        AstExpression::BinaryOperation { .. } => evaluate_binary_operation(expr, eval),
        AstExpression::Conditional { expr, consequent, alternative, .. } => {
            evaluate_conditional(expr, consequent, alternative, eval)
        }
    }
}

fn evaluate_address_of(expr: &AstExpression, eval: &mut Eval) -> Option<ConstantValue> {
    let AstExpression::AddressOf { node_id, expr } = expr else {
        ICE!("Expected AstExpression::AddressOf");
    };

    let ptr_type = eval.chk.get_data_type(node_id);

    let init = evaluate_address_constant(expr, eval)?;

    Some(ConstantValue::Pointer(ptr_type, init))
}

fn evaluate_address_constant(expr: &AstExpression, eval: &mut Eval) -> Option<AstAddressConstant> {
    match expr {
        // A static storage pointer can be initialized with the address of an object of static storage duration,
        // or a function designator.
        //
        AstExpression::Identifier { unique_name, .. } => {
            let symbol = eval.chk.symbols.get(unique_name).expect("Symbol should exist");

            if symbol.data_type.is_function() {
                Some(AstAddressConstant::AddressOfFunction(unique_name.to_string()))
            } else if symbol.storage_duration() == AstStorageDuration::Static {
                Some(AstAddressConstant::AddressOfObject { object: unique_name.to_string(), byte_offset: 0 })
            } else {
                None
            }
        }

        AstExpression::AddressOf { expr, .. } => evaluate_address_constant(expr, eval),

        // A static storage pointer can be initialized with the address of an array element.
        //
        AstExpression::Subscript { node_id, expr1, expr2, .. } => {
            let expr1_t = eval.chk.get_data_type(&expr1.node_id());
            let expr2_t = eval.chk.get_data_type(&expr2.node_id());
            debug_assert!(expr1_t.is_pointer() || expr2_t.is_pointer());

            let (ptr_expr, int_expr) = if expr1_t.is_pointer() { (expr1, expr2) } else { (expr2, expr1) };

            let subscript_identifier = evaluate_address_constant(ptr_expr, eval)?;
            let subscript_index_value = evaluate_constant_expr(int_expr, eval)?;

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
                let Some(symbol) = eval.chk.symbols.get(AstUniqueName::new(object.clone())) else {
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
                    let loc = eval.chk.metadata.get_source_location(&int_expr.node_id());
                    Warning::array_index_out_of_bounds(subscript_index, &symbol.data_type, loc, eval.driver);
                }

                let subscript_expr_type = eval.chk.get_data_type(node_id);
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
            let constant_idx = eval.chk.constants.add_string(&constant_string);

            // Add the constant string to the symbol table (may already exist).
            let loc = eval.chk.metadata.get_source_location(literal_node_id);
            let attrs = SymbolAttributes::constant(loc);
            let const_name = eval.chk.constants.make_const_symbol_name(constant_idx);
            let lit_data_type = eval.chk.get_data_type(literal_node_id);
            let unique_name = AstUniqueName::new(const_name.clone());
            _ = eval.chk.symbols.add(unique_name, lit_data_type, attrs);

            Some(AstAddressConstant::AddressOfObject { object: const_name, byte_offset: 0 })
        }

        _ => None,
    }
}

fn evaluate_cast(expr: &AstExpression, eval: &mut Eval) -> Option<ConstantValue> {
    let AstExpression::Cast { target_type, expr, is_implicit, .. } = expr else {
        ICE!("Expected AstExpression::Cast");
    };

    let value = evaluate_const_expr_recursively(expr, eval)?;

    let ast_type = type_resolution::resolve_declared_type(target_type, eval.chk, eval.driver).ok()?;

    value.cast_to(&ast_type, *is_implicit, eval)
}

fn evaluate_unary_operation(op: &AstUnaryOp, expr: &AstExpression, eval: &mut Eval) -> Option<ConstantValue> {
    let value = evaluate_const_expr_recursively(expr, eval)?;

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

fn evaluate_binary_operation(expr: &AstExpression, eval: &mut Eval) -> Option<ConstantValue> {
    let AstExpression::BinaryOperation { node_id, op, left, right } = expr else {
        ICE!("Expected AstExpression::BinaryOperation");
    };

    let mut lhs = evaluate_const_expr_recursively(left, eval)?;
    let mut rhs = evaluate_const_expr_recursively(right, eval)?;

    let common_type = lhs.get_common_type(&rhs)?;
    if !common_type.is_pointer() {
        lhs = lhs.cast_to(&common_type, true, eval)?;
        rhs = rhs.cast_to(&common_type, true, eval)?;
    }

    binary_expr::evaluate_binary_op(*node_id, right.node_id(), op, lhs, rhs, eval)
}

fn evaluate_conditional(
    expr: &AstExpression,
    consequent: &AstExpression,
    alternative: &AstExpression,
    eval: &mut Eval,
) -> Option<ConstantValue> {
    // We need do determine the common type of both `consequent` and `alternative` expressions, in case they have
    // different types, so that we can cast them to the common type.
    //      E.g. `11 < 2 ? 1.0 : 2.0f`  -->  cast both to 'double'
    //
    // So we evaluate both expressions in order to get their types. It's safe to evaluate both consequent and
    // alternative at compile-time because there can be no side-effects, even though strictly only the `true`
    // case expression should be evaluated.
    //
    let consequent = evaluate_const_expr_recursively(consequent, eval)?;
    let alternative = evaluate_const_expr_recursively(alternative, eval)?;

    let common_type = consequent.get_common_type(&alternative)?;
    let consequent = consequent.cast_to(&common_type, true, eval)?;
    let alternative = alternative.cast_to(&common_type, true, eval)?;

    match evaluate_const_expr_recursively(expr, eval) {
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

macro_rules! impl_from_int {
    ($($t:ty => ($signed:expr, $size:expr)),* $(,)?) => {
        $(
            impl From<$t> for ConstantValue {
                fn from(value: $t) -> Self {
                    ConstantValue::Int {
                        value: value as i128,
                        signed: $signed,
                        size: $size,
                    }
                }
            }
        )*
    };
}

impl_from_int!(
    i8  => (true, 8),
    i16 => (true, 16),
    i32 => (true, 32),
    i64 => (true, 64),
    u8  => (false, 8),
    u16 => (false, 16),
    u32 => (false, 32),
    u64 => (false, 64),
);

impl From<f32> for ConstantValue {
    fn from(value: f32) -> Self {
        ConstantValue::Float { value: value as f64, size: 32 }
    }
}

impl From<f64> for ConstantValue {
    fn from(value: f64) -> Self {
        ConstantValue::Float { value, size: 64 }
    }
}

impl ConstantValue {
    /// Creates a 32-bit integer constant value.
    ///
    /// Can also use `ConstantValue::from(i32)` but creating an 'int' is quite common in the evaluator.
    pub fn make_int(value: i32) -> Self {
        ConstantValue::Int { value: value as i128, signed: true, size: 32 }
    }

    /// Consumes this constant value and returns a `Some<ConstantValue>` version casted to the given `AstType`, or
    /// returns `None` if the cast is not possible.
    pub fn cast_to(self, target_type: &AstType, is_implicit_cast: bool, eval: &mut Eval) -> Option<Self> {
        // Skip the cast if the constant value already has the target type.
        if self.get_ast_type() == *target_type {
            return Some(self);
        }

        // Helper macro that performs a cast to an integer type and emits a warning if the conversion changes
        // the value.
        macro_rules! cast_to_integer {
            ($old_value:expr, $old_type:expr, $new_type_rs:ty) => {{
                let new_value = $old_value as $new_type_rs;

                // Roundtrip to check if the cast has changed the constant value.
                //
                if eval.emit_diagnostics {
                    if is_implicit_cast && $old_type.is_integer() && $old_value as i128 != new_value as i128 {
                        warn_constant_conversion(&$old_type, target_type, $old_value as i128, new_value as i128, eval);
                    } else if $old_type.is_floating_point() && $old_value as f64 != new_value as f64 {
                        let old_value = $old_value as f64;
                        let new_value = new_value as f64;

                        if is_implicit_cast {
                            warn_constant_conversion(&$old_type, target_type, old_value, new_value, eval);
                        } else {
                            // Float-to-int conversion even with an explicit cast is UB because it has changed the
                            // value (i.e. truncated part of the float value).
                            warn_constant_conversion_float_int_explicit_cast(&$old_type, target_type, eval);
                        }
                    }
                }

                Some(Self::from(new_value))
            }};
        }

        // Helper macro that performs a cast to a floating-point type and emits a warning if the implicit conversion
        // changes the value / causes precision loss.
        macro_rules! cast_to_fp {
            ($value:expr, $old_type:expr, $new_type_rs:ty) => {{
                let new_value = $value as $new_type_rs;

                if eval.emit_diagnostics && is_implicit_cast {
                    if $old_type.is_integer() {
                        let roundtripped = new_value as i128;
                        if roundtripped != $value as i128 {
                            let old_val = $value.to_string();
                            let new_val = new_value.to_string();
                            let loc = eval.root_expression_sloc;
                            Warning::constant_conversion(&$old_type, target_type, &old_val, &new_val, loc, eval.driver);
                        }
                    } else if $old_type.is_floating_point() {
                        let roundtripped = new_value as f64;
                        if roundtripped != $value as f64 {
                            let loc = eval.root_expression_sloc;
                            Warning::implicit_arithmetic_conversion(&$old_type, target_type, loc, eval.driver);
                        }
                    } else {
                        ICE!("Unexpected cast to floating-point from {}", $old_type);
                    };
                }

                Some(Self::from(new_value))
            }};
        }

        // Helper macro to avoid duplicating the match expression when casting from `self` (see below).
        macro_rules! cast_value_to {
            ($value:expr, $old_type:expr) => {
                match target_type {
                    // Cast to signed integer type
                    AstType::Char | AstType::SignedChar => cast_to_integer!($value, $old_type, i8),
                    AstType::Short => cast_to_integer!($value, $old_type, i16),
                    AstType::Int => cast_to_integer!($value, $old_type, i32),
                    AstType::Long | AstType::LongLong => cast_to_integer!($value, $old_type, i64),

                    // Cast to unsigned integer type
                    AstType::UnsignedChar => cast_to_integer!($value, $old_type, u8),
                    AstType::UnsignedShort => cast_to_integer!($value, $old_type, u16),
                    AstType::UnsignedInt => cast_to_integer!($value, $old_type, u32),
                    AstType::UnsignedLong | AstType::UnsignedLongLong => cast_to_integer!($value, $old_type, u64),

                    // Cast to floating-point type
                    AstType::Float => cast_to_fp!($value, $old_type, f32),
                    AstType::Double => cast_to_fp!($value, $old_type, f64),

                    // Cast to pointer type
                    AstType::Pointer(_) => {
                        let init = if $value as u64 == 0 {
                            AstAddressConstant::NullPointer
                        } else {
                            AstAddressConstant::CastExpression($value as u64)
                        };

                        Some(ConstantValue::Pointer(target_type.clone(), init))
                    }

                    _ => None,
                }
            };
        }

        // Cast from...
        match self {
            ConstantValue::Int { value, .. } => cast_value_to!(value, self.get_ast_type()),
            ConstantValue::Float { value, .. } => cast_value_to!(value, self.get_ast_type()),
            ConstantValue::Pointer(_, init) => Some(ConstantValue::Pointer(target_type.clone(), init)),
            ConstantValue::String(_) => {
                if let AstType::Pointer(referent) = target_type
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

    /// Gets the [AstType] of this constant value.
    pub fn get_ast_type(&self) -> AstType {
        match self {
            ConstantValue::Int { signed, size, .. } => match (signed, size) {
                (true, 8) => AstType::Char,
                (true, 16) => AstType::Short,
                (true, 32) => AstType::Int,
                (true, 64) => AstType::Long,
                (false, 8) => AstType::UnsignedChar,
                (false, 16) => AstType::UnsignedShort,
                (false, 32) => AstType::UnsignedInt,
                (false, 64) => AstType::UnsignedLong,
                _ => ICE!("No integer AstType for '{signed}' '{size}'"),
            },
            ConstantValue::Float { size, .. } => match size {
                32 => AstType::Float,
                64 => AstType::Double,
                _ => ICE!("No floating-point AstType for '{size}'"),
            },
            ConstantValue::Pointer(ast_type, ..) => ast_type.clone(),
            ConstantValue::String(..) => AstType::new_pointer_to(AstType::Char),
        }
    }

    /// Gets the [AstType] if this value is a pointer type, or returns `None`.
    pub fn get_pointer_type(&self) -> Option<AstType> {
        if let ConstantValue::Pointer(ty, _) = self { Some(ty.clone()) } else { None }
    }

    /// Gets the common [AstType] for this constant value and the given other constant value.
    pub fn get_common_type(&self, other: &ConstantValue) -> Option<AstType> {
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

        let this_type = self.get_ast_type();
        let other_type = other.get_ast_type();

        Some(AstType::get_common_type(&this_type, &other_type))
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
}

fn warn_constant_conversion<T: std::fmt::Display>(
    old_type: &AstType,
    new_type: &AstType,
    old_value: T,
    new_value: T,
    eval: &mut Eval,
) {
    let old_value = old_value.to_string();
    let new_value = new_value.to_string();
    let loc = eval.root_expression_sloc;
    Warning::constant_conversion(old_type, new_type, &old_value, &new_value, loc, eval.driver);
}

fn warn_constant_conversion_float_int_explicit_cast(old_type: &AstType, new_type: &AstType, eval: &mut Eval) {
    let loc = eval.root_expression_sloc;
    Warning::constant_conversion_float_int_explicit_cast(old_type, new_type, loc, eval.driver);
}
