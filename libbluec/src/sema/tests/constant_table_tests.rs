// Copyright 2025-2026 Neil Henderson

use super::super::constant_table::*;

#[test]
fn get_float_constants() {
    let mut table = ConstantTable::new();
    _ = table.add_f64(1.0, 8);
    _ = table.add_f32(1.0, 4);
    _ = table.add_f64(-1.0, 8);
    _ = table.add_f32(-1.0, 4);
    _ = table.add_f64(0.0, 16);
    _ = table.add_f32(0.0, 16);
    _ = table.add_f64(-0.0, 8);
    _ = table.add_f32(-0.0, 8);
    _ = table.add_f64(std::f64::NAN, 8);
    _ = table.add_f32(std::f32::NAN, 4);

    let list = table.get_float_constants();
    assert_eq!(list.len(), 10);

    assert_eq!(list[0].index, 0);
    assert_eq!(list[0].value, UnsignedValue::U64(0x3ff0000000000000));
    assert_eq!(list[0].alignment, 8);

    assert_eq!(list[1].index, 1);
    assert_eq!(list[1].value, UnsignedValue::U32(0x3F800000));
    assert_eq!(list[1].alignment, 4);

    assert_eq!(list[2].index, 2);
    assert_eq!(list[2].value, UnsignedValue::U64(0xbff0000000000000));
    assert_eq!(list[2].alignment, 8);

    assert_eq!(list[3].index, 3);
    assert_eq!(list[3].value, UnsignedValue::U32(0xBF800000));
    assert_eq!(list[3].alignment, 4);

    assert_eq!(list[4].index, 4);
    assert_eq!(list[4].value, UnsignedValue::U64(0x0));
    assert_eq!(list[4].alignment, 16);

    assert_eq!(list[6].index, 6);
    assert_eq!(list[6].value, UnsignedValue::U64(0x8000000000000000));
    assert_eq!(list[6].alignment, 8);

    assert_eq!(list[7].index, 7);
    assert_eq!(list[7].value, UnsignedValue::U32(0x80000000));
    assert_eq!(list[7].alignment, 8);

    // Not all NANs have the same value, but this tests the value matches Rust's NAN constant.
    assert_eq!(list[8].index, 8);
    assert_eq!(list[8].value, UnsignedValue::U64(0x7FF8000000000000));
    assert_eq!(list[8].alignment, 8);

    assert_eq!(list[9].index, 9);
    assert_eq!(list[9].value, UnsignedValue::U32(0x7FC00000));
    assert_eq!(list[9].alignment, 4);
}

#[test]
fn update_float_alignment() {
    let mut table = ConstantTable::new();

    let idx1 = table.add_f64(1.0, 8);
    let idx2 = table.add_f64(1.0, 8);
    assert_eq!(idx1, idx2);

    let idx3 = table.add_f32(1.0, 4);
    let idx4 = table.add_f32(1.0, 4);
    assert_eq!(idx3, idx4);
    assert_ne!(idx1, idx3);

    let list = table.get_float_constants();
    assert_eq!(list.len(), 2);

    assert_eq!(list[0].index, 0);
    assert_eq!(list[0].value, UnsignedValue::U64(0x3ff0000000000000));
    assert_eq!(list[0].alignment, 8);

    assert_eq!(list[1].index, 1);
    assert_eq!(list[1].value, UnsignedValue::U32(0x3F800000));
    assert_eq!(list[1].alignment, 4);

    let idx_f64_1 = table.add_f64(1.0, 16);
    assert_eq!(idx1, idx_f64_1);

    let idx_f32_1 = table.add_f32(1.0, 8);
    assert_eq!(idx3, idx_f32_1);

    let list = table.get_float_constants();
    assert_eq!(list.len(), 2);

    assert_eq!(list[0].index, 0);
    assert_eq!(list[0].value, UnsignedValue::U64(0x3ff0000000000000));
    assert_eq!(list[0].alignment, 16);

    assert_eq!(list[1].index, 1);
    assert_eq!(list[1].value, UnsignedValue::U32(0x3F800000));
    assert_eq!(list[1].alignment, 8);
}
