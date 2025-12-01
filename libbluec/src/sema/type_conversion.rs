// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `type_conversion` module provides functionality to convert between data types.

use crate::{internal_error, parser::AstType};

/// Casts a `i128` value to the given integer data type, and returns the new value as either `u64` or `i64`.
pub fn cast_i128_to_integer_type(value: i128, data_type: &AstType) -> (Option<u64>, Option<i64>) {
    match data_type {
        AstType::Short => (None, Some((value as i16) as i64)),
        AstType::Int => (None, Some((value as i32) as i64)),
        AstType::Long | AstType::LongLong => (None, Some(value as i64)),
        AstType::UnsignedShort => (Some((value as u16) as u64), None),
        AstType::UnsignedInt => (Some((value as u32) as u64), None),
        AstType::UnsignedLong | AstType::UnsignedLongLong => (Some(value as u64), None),
        _ => internal_error::ICE("'cast_u64_to_integer_type' expects an integer data type to cast to"),
    }
}

/// Converts a `u64` value to an `i64`.
///
/// This is suitable for implementing a C cast from unsigned 64-bit to signed 64-bit:
/// ```c
/// int64_t result = (int64_t)some_unsigned_64bit_value;
/// ```
///
/// This performs a lossy cast if the `u64` value exceeds `i64::MAX`
/// (i.e., values above 9_223_372_036_854_775_807). In such cases, the result
/// will wrap around and become negative due to two's complement representation.
pub fn convert_u64_to_i64(value: u64) -> i64 {
    value as i64
}

/// Converts a `u64` value to an `i32`.
///
/// This is suitable for implementing a C cast from unsigned 64-bit to signed 32-bit:
/// ```c
/// int32_t result = (int32_t)some_unsigned_64bit_value;
/// ```
///
/// This conversion truncates the upper 32 bits of the `u64` value. If the resulting
/// `i32` value exceeds `i32::MAX`, it will wrap around and become negative.
pub fn convert_u64_to_i32(value: u64) -> i32 {
    value as i32
}

/// Converts a `u64` value to a `u32`.
///
/// This is suitable for implementing a C cast from unsigned 64-bit to unsigned 32-bit:
/// ```c
/// unsigned int result = (unsigned int)some_unsigned_64bit_value;
/// ```
///
/// This conversion truncates the upper 32 bits of the `u64` value.
pub fn convert_u64_to_u32(value: u64) -> u32 {
    value as u32
}

/// Converts an `i64` value to an `i32`.
///
/// This is suitable for implementing a C cast from signed 64-bit to signed 32-bit:
/// ```c
/// int32_t result = (int32_t)some_signed_64bit_value;
/// ```
pub fn convert_i64_to_i32(value: i64) -> i32 {
    value as i32
}

/// Converts an `i64` value to an `i16`.
///
/// This is suitable for implementing a C cast from signed 64-bit to signed 16-bit:
/// ```c
/// short result = (short)some_signed_64bit_value;
/// ```
pub fn convert_i64_to_i16(value: i64) -> i16 {
    value as i16
}

/// Converts an `i32` value to an `i16`.
///
/// This is suitable for implementing a C cast from signed 32-bit to signed 16-bit:
/// ```c
/// short result = (short)some_signed_32bit_value;
/// ```
pub fn convert_i32_to_i16(value: i32) -> i16 {
    value as i16
}
