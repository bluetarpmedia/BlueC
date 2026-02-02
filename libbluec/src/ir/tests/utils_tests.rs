// Copyright 2025 Neil Henderson, Blue Tarp Media.

use super::super::translator::utils::*;
use super::super::{BtType, BtValue};

#[test]
fn test_try_lossless_convert_integer() {
    //
    // Invalid cases
    //
    assert!(try_lossless_convert_integer(-1, &BtType::UInt8).is_none());
    assert!(try_lossless_convert_integer(-1, &BtType::UInt16).is_none());
    assert!(try_lossless_convert_integer(-1, &BtType::UInt32).is_none());
    assert!(try_lossless_convert_integer(-1, &BtType::UInt64).is_none());

    assert!(try_lossless_convert_integer(-129, &BtType::Int8).is_none());
    assert!(try_lossless_convert_integer(128, &BtType::Int8).is_none());

    assert!(try_lossless_convert_integer(-32769, &BtType::Int16).is_none());
    assert!(try_lossless_convert_integer(32768, &BtType::Int16).is_none());

    assert!(try_lossless_convert_integer(-2147483649_i64, &BtType::Int32).is_none());
    assert!(try_lossless_convert_integer(2147483648_i64, &BtType::Int32).is_none());

    assert!(try_lossless_convert_integer(9223372036854775808_u64, &BtType::Int64).is_none());

    assert!(try_lossless_convert_integer(-(1 << 24) - 1, &BtType::Float32).is_none());
    assert!(try_lossless_convert_integer((1 << 24) + 1, &BtType::Float32).is_none());
    assert!(try_lossless_convert_integer(-(1_i64 << 53) - 1, &BtType::Float64).is_none());
    assert!(try_lossless_convert_integer((1_i64 << 53) + 1, &BtType::Float64).is_none());

    //
    // Valid cases
    //
    assert_eq!(try_lossless_convert_integer(0, &BtType::Int8), Some(BtValue::new_constant_i8(0)));
    assert_eq!(try_lossless_convert_integer(0, &BtType::Int16), Some(BtValue::new_constant_i16(0)));
    assert_eq!(try_lossless_convert_integer(0, &BtType::Int32), Some(BtValue::new_constant_i32(0)));
    assert_eq!(try_lossless_convert_integer(0, &BtType::Int64), Some(BtValue::new_constant_i64(0)));

    assert_eq!(try_lossless_convert_integer(0, &BtType::UInt8), Some(BtValue::new_constant_u8(0)));
    assert_eq!(try_lossless_convert_integer(0, &BtType::UInt16), Some(BtValue::new_constant_u16(0)));
    assert_eq!(try_lossless_convert_integer(0, &BtType::UInt32), Some(BtValue::new_constant_u32(0)));
    assert_eq!(try_lossless_convert_integer(0, &BtType::UInt64), Some(BtValue::new_constant_u64(0)));

    assert_eq!(try_lossless_convert_integer(-1, &BtType::Int8), Some(BtValue::new_constant_i8(-1)));
    assert_eq!(try_lossless_convert_integer(-1, &BtType::Int16), Some(BtValue::new_constant_i16(-1)));
    assert_eq!(try_lossless_convert_integer(-1, &BtType::Int32), Some(BtValue::new_constant_i32(-1)));
    assert_eq!(try_lossless_convert_integer(-1, &BtType::Int64), Some(BtValue::new_constant_i64(-1)));

    assert_eq!(try_lossless_convert_integer(128, &BtType::UInt8), Some(BtValue::new_constant_u8(128)));
    assert_eq!(try_lossless_convert_integer(32768, &BtType::UInt16), Some(BtValue::new_constant_u16(32768)));
    assert_eq!(
        try_lossless_convert_integer(2147483648_i64, &BtType::UInt32),
        Some(BtValue::new_constant_u32(2147483648))
    );
    assert_eq!(
        try_lossless_convert_integer(9223372036854775808_u64, &BtType::UInt64),
        Some(BtValue::new_constant_u64(9223372036854775808))
    );

    assert_eq!(try_lossless_convert_integer(0, &BtType::Float32), Some(BtValue::new_constant_f32(0.0)));
    assert_eq!(try_lossless_convert_integer(-1, &BtType::Float32), Some(BtValue::new_constant_f32(-1.0)));
    assert_eq!(try_lossless_convert_integer(1, &BtType::Float32), Some(BtValue::new_constant_f32(1.0)));

    assert_eq!(try_lossless_convert_integer(0, &BtType::Float64), Some(BtValue::new_constant_f64(0.0)));
    assert_eq!(try_lossless_convert_integer(-1, &BtType::Float64), Some(BtValue::new_constant_f64(-1.0)));
    assert_eq!(try_lossless_convert_integer(1, &BtType::Float64), Some(BtValue::new_constant_f64(1.0)));

    assert_eq!(
        try_lossless_convert_integer(-(1 << 24), &BtType::Float32),
        Some(BtValue::new_constant_f32(-16777216.0))
    );
    assert_eq!(try_lossless_convert_integer(1 << 24, &BtType::Float32), Some(BtValue::new_constant_f32(16777216.0)));
    assert_eq!(
        try_lossless_convert_integer(-(1_i64 << 53), &BtType::Float64),
        Some(BtValue::new_constant_f64(-9007199254740992.0))
    );
    assert_eq!(
        try_lossless_convert_integer(1_i64 << 53, &BtType::Float64),
        Some(BtValue::new_constant_f64(9007199254740992.0))
    );
}
