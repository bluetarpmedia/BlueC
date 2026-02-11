// Copyright 2025-2026 Neil Henderson
//
//! The `utils` module defines utility functions for the BlueTac translator.

use super::{BtConstantValue, BtType, BtValue};

/// Attempts to convert a constant integer value to the given [BtType], but only if the destination [BtType] can hold
/// the value.
pub fn try_lossless_convert_integer<T>(value: T, dst_type: &BtType) -> Option<BtValue>
where
    T: Copy
        + TryInto<i8>
        + TryInto<i16>
        + TryInto<i32>
        + TryInto<i64>
        + TryInto<u8>
        + TryInto<u16>
        + TryInto<u32>
        + TryInto<u64>,
{
    debug_assert!(dst_type.is_arithmetic());

    match dst_type {
        BtType::Int8 => Some(BtValue::Constant(BtConstantValue::Int8(<T as TryInto<i8>>::try_into(value).ok()?))),
        BtType::Int16 => Some(BtValue::Constant(BtConstantValue::Int16(<T as TryInto<i16>>::try_into(value).ok()?))),
        BtType::Int32 => Some(BtValue::Constant(BtConstantValue::Int32(<T as TryInto<i32>>::try_into(value).ok()?))),
        BtType::Int64 => Some(BtValue::Constant(BtConstantValue::Int64(<T as TryInto<i64>>::try_into(value).ok()?))),

        BtType::UInt8 => Some(BtValue::Constant(BtConstantValue::UInt8(<T as TryInto<u8>>::try_into(value).ok()?))),
        BtType::UInt16 => Some(BtValue::Constant(BtConstantValue::UInt16(<T as TryInto<u16>>::try_into(value).ok()?))),
        BtType::UInt32 => Some(BtValue::Constant(BtConstantValue::UInt32(<T as TryInto<u32>>::try_into(value).ok()?))),
        BtType::UInt64 => Some(BtValue::Constant(BtConstantValue::UInt64(<T as TryInto<u64>>::try_into(value).ok()?))),

        BtType::Float32 => {
            // f32 exactly represents all integers in [-2^24, +2^24] (+/- 16,777,216), so convert our input to
            // an i32 first.
            //
            let value_i32 = <T as TryInto<i32>>::try_into(value).ok()?;

            if (-(1 << 24)..=(1 << 24)).contains(&value_i32) {
                Some(BtValue::Constant(BtConstantValue::Float32(value_i32 as f32)))
            } else {
                None
            }
        }

        BtType::Float64 => {
            // f64 exactly represents all integers in [-2^53, +2^53] (+/- 9,007,199,254,740,992), so convert our input
            // to an i64 first.
            //
            let value_i64 = <T as TryInto<i64>>::try_into(value).ok()?;

            if (-(1 << 53)..=(1 << 53)).contains(&value_i64) {
                Some(BtValue::Constant(BtConstantValue::Float64(value_i64 as f64)))
            } else {
                None
            }
        }

        _ => None,
    }
}
