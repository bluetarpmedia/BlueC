// Copyright 2025-2026 Neil Henderson

use super::super::BtConstantValue;

#[test]
fn is_truthy() {
    assert!(BtConstantValue::Int8(1).is_truthy());
    assert!(BtConstantValue::Int16(1).is_truthy());
    assert!(BtConstantValue::Int32(1).is_truthy());
    assert!(BtConstantValue::Int64(1).is_truthy());
    assert!(BtConstantValue::UInt8(1).is_truthy());
    assert!(BtConstantValue::UInt16(1).is_truthy());
    assert!(BtConstantValue::UInt32(1).is_truthy());
    assert!(BtConstantValue::UInt64(1).is_truthy());

    assert!(BtConstantValue::Float32(1.0).is_truthy());
    assert!(BtConstantValue::Float64(1.0).is_truthy());

    assert!(BtConstantValue::Int8(12).is_truthy());
    assert!(BtConstantValue::Int16(13).is_truthy());
    assert!(BtConstantValue::Int32(14).is_truthy());
    assert!(BtConstantValue::Int64(15).is_truthy());
    assert!(BtConstantValue::UInt8(16).is_truthy());
    assert!(BtConstantValue::UInt16(17).is_truthy());
    assert!(BtConstantValue::UInt32(18).is_truthy());
    assert!(BtConstantValue::UInt64(19).is_truthy());

    assert!(BtConstantValue::Float32(3.14).is_truthy());
    assert!(BtConstantValue::Float32(-3.14).is_truthy());
    assert!(BtConstantValue::Float64(0.0001).is_truthy());
    assert!(BtConstantValue::Float64(-0.0001).is_truthy());

    assert!(!BtConstantValue::Int8(0).is_truthy());
    assert!(!BtConstantValue::Int16(0).is_truthy());
    assert!(!BtConstantValue::Int32(0).is_truthy());
    assert!(!BtConstantValue::Int64(0).is_truthy());
    assert!(!BtConstantValue::UInt8(0).is_truthy());
    assert!(!BtConstantValue::UInt16(0).is_truthy());
    assert!(!BtConstantValue::UInt32(0).is_truthy());
    assert!(!BtConstantValue::UInt64(0).is_truthy());

    // 0.0 and -0.0 are both false
    assert!(!BtConstantValue::Float32(0.0).is_truthy());
    assert!(!BtConstantValue::Float64(0.0).is_truthy());
    assert!(!BtConstantValue::Float32(-0.0).is_truthy());
    assert!(!BtConstantValue::Float64(-0.0).is_truthy());
}
