// Copyright 2025-2026 Neil Henderson

use std::f64;
use std::io::BufReader;
use std::io::Cursor;

use crate::compiler_driver;
use crate::lexer;
use crate::parser::tests::utils;
use crate::parser::{AstConstantFp, AstConstantInteger, AstConstantValue, Parser};
use crate::parser::{expr, recursive_descent};

use super::super::constant_eval;
use super::super::type_check;

#[test]
fn integer_literal() {
    verify_expr_evaluates_to_i32("0", Some(0));
    verify_expr_evaluates_to_i32("-0", Some(0));
    verify_expr_evaluates_to_i32("123", Some(123));
    verify_expr_evaluates_to_i32("(123)", Some(123));
    verify_expr_evaluates_to_i32("-321", Some(-321));

    verify_expr_evaluates_to_u32("0xFFFFFFFFu", Some(4294967295));
    verify_expr_evaluates_to_i64("0xFFFFFFFF", Some(4294967295));

    verify_expr_evaluates_to_i32("0b11111111", Some(255));
    verify_expr_evaluates_to_u32("0b11111111u", Some(255));

    verify_expr_evaluates_to_i32("0212", Some(138));
    verify_expr_evaluates_to_u32("0212u", Some(138));

    verify_expr_evaluates_to_u32("0U", Some(0));
    verify_expr_evaluates_to_u64("0UL", Some(0));
    verify_expr_evaluates_to_u64("9223372036854775807ull", Some(9223372036854775807));
    verify_expr_evaluates_to_i64("8589934592L", Some(8589934592));
    verify_expr_evaluates_to_i64("8589934592LL", Some(8589934592));
    verify_expr_evaluates_to_i64("8589934592", Some(8589934592));

    // i32::MIN and i32::MAX literals
    verify_expr_evaluates_to_i64("-2147483648", Some(-2_147_483_648));
    verify_expr_evaluates_to_i32("2147483647", Some(2_147_483_647));

    verify_expr_evaluates_to_i64("-(2147483648)", Some(-2_147_483_648));
    verify_expr_evaluates_to_i32("(2147483647)", Some(2_147_483_647));

    // i64::MIN+1 and i64::MAX literals
    //
    //      Can't parse the i64::MIN literal written as "-9223372036854775808" due
    //      to parsing the '-' negate operator first and then trying to fit the
    //      value inside an i64, which is 1 too big for a positive i64.
    //      Instead, C stdlib provides a macro to calculate the value:
    //
    //          #define INT64_MIN (-9223372036854775807LL -1)
    //
    verify_expr_evaluates_to_i64("-9223372036854775807", Some(-9223372036854775807));
    verify_expr_evaluates_to_i64("9223372036854775807", Some(9223372036854775807));

    // i64::MIN with expression
    verify_expr_evaluates_to_i64("(-9223372036854775807L -1)", Some(-9223372036854775808));
}

#[test]
fn float_literals() {
    verify_expr_evaluates_to_f32("0.0f", Some(0.0));
    verify_expr_evaluates_to_f32("0.0F", Some(0.0));
    verify_expr_evaluates_to_f32("1e0f", Some(1.0));

    verify_expr_evaluates_to_f32("0xFFp0f", Some(255.0));
    verify_expr_evaluates_to_f32("0x1.0000000000000p-1f", Some(0.5));

    verify_expr_evaluates_to_f64("0.0", Some(0.0));
    verify_expr_evaluates_to_f64("0.123", Some(0.123));
    verify_expr_evaluates_to_f64("3.1415", Some(3.1415));
    verify_expr_evaluates_to_f64("999.999", Some(999.999));

    verify_expr_evaluates_to_f64("1e0", Some(1.0));
    verify_expr_evaluates_to_f64("1e1", Some(10.0));
    verify_expr_evaluates_to_f64("1e2", Some(100.0));
    verify_expr_evaluates_to_f64("1e3", Some(1000.0));
    verify_expr_evaluates_to_f64("1e-1", Some(0.1));
    verify_expr_evaluates_to_f64("1e-2", Some(0.01));
    verify_expr_evaluates_to_f64("1e-3", Some(0.001));
    verify_expr_evaluates_to_f64("1.23e-2", Some(0.0123));
    verify_expr_evaluates_to_f64("1.23e+2", Some(123.0));

    verify_expr_evaluates_to_f64("0xFFp0", Some(255.0));
    verify_expr_evaluates_to_f64("0x1.0000000000000p-1", Some(0.5));

    verify_expr_evaluates_to_f64("0.0l", Some(0.0));
    verify_expr_evaluates_to_f64("0.0L", Some(0.0));
    verify_expr_evaluates_to_f64("1.23e+2L", Some(123.0));
}

#[test]
fn string_literal() {
    verify_expr_evaluates_to_string(r#" "test" "#, Some("test"));
    verify_expr_evaluates_to_string(r#" "test" "hello" "#, Some("testhello"));
}

#[test]
fn bitwise_not() {
    verify_expr_evaluates_to_i32("~100", Some(-101));
    verify_expr_evaluates_to_i32("~-101", Some(100));
    verify_expr_evaluates_to_i32("~(-101)", Some(100));
}

#[test]
fn unary_plus_minus() {
    verify_expr_evaluates_to_i32("+100", Some(100));
    verify_expr_evaluates_to_i32("-100", Some(-100));
    verify_expr_evaluates_to_i32("+-100", Some(-100));
    verify_expr_evaluates_to_i32("-+100", Some(-100));
    verify_expr_evaluates_to_i32("+~100", Some(-101));
    verify_expr_evaluates_to_i32("~+100", Some(-101));
    verify_expr_evaluates_to_i32("+(100)", Some(100));
    verify_expr_evaluates_to_i32("+(+(+(100)))", Some(100));
    verify_expr_evaluates_to_i32("-(-(-(-(100))))", Some(100));

    verify_expr_evaluates_to_f32("+1.0f", Some(1.0));
    verify_expr_evaluates_to_f32("-1.0f", Some(-1.0));
    verify_expr_evaluates_to_f32("-(1.0f)", Some(-1.0));
    verify_expr_evaluates_to_f32("-1.0e-1f", Some(-0.10));

    verify_expr_evaluates_to_f64("+1.0", Some(1.0));
    verify_expr_evaluates_to_f64("-1.0", Some(-1.0));
    verify_expr_evaluates_to_f64("-(1.0)", Some(-1.0));
    verify_expr_evaluates_to_f64("-(-(-(-(100.0))))", Some(100.0));
    verify_expr_evaluates_to_f64("-1.0e1", Some(-10.0));
    verify_expr_evaluates_to_f64("-1.0e-1", Some(-0.10));
    verify_expr_evaluates_to_f64("+1.0e-1", Some(0.10));
}

#[test]
fn logical_not() {
    verify_expr_evaluates_to_i32("!1", Some(0));
    verify_expr_evaluates_to_i32("!-1", Some(0));
    verify_expr_evaluates_to_i32("!(-123)", Some(0));
    verify_expr_evaluates_to_i32("!0", Some(1));
    verify_expr_evaluates_to_i32("!(0)", Some(1));

    verify_expr_evaluates_to_i32("!0.0", Some(1));
    verify_expr_evaluates_to_i32("!1.0", Some(0));
    verify_expr_evaluates_to_f64("1.0 + !(0.0)", Some(2.0));
    verify_expr_evaluates_to_i32("!10e0", Some(0));

    verify_expr_evaluates_to_i32("!0.0f", Some(1));
    verify_expr_evaluates_to_i32("!1.0f", Some(0));
    verify_expr_evaluates_to_f32("1.0f + !(0.0f)", Some(2.0));
    verify_expr_evaluates_to_i32("!10e0f", Some(0));
}

#[test]
fn binary_expressions() {
    verify_expr_evaluates_to_i32("100 + 10", Some(110));
    verify_expr_evaluates_to_i32("-100 + 10", Some(-90));
    verify_expr_evaluates_to_i32("(1 + 2) / 3", Some(1));
    verify_expr_evaluates_to_i32("1 * 2 + 3 / 4 % 5", Some(2));
    verify_expr_evaluates_to_i32("(((5 * 4) + 2) / 2) % 10", Some(1));

    verify_expr_evaluates_to_i32("~1 + 10", Some(8));
    verify_expr_evaluates_to_i32("10 ^ 2", Some(8));
    verify_expr_evaluates_to_i32("1 & 3", Some(1));
    verify_expr_evaluates_to_i32("1 | 2", Some(3));
    verify_expr_evaluates_to_i32("1 << 3", Some(8));
    verify_expr_evaluates_to_i32("8 >> 3", Some(1));
    verify_expr_evaluates_to_i32("1024 >> 0", Some(1024));
    verify_expr_evaluates_to_i32("1024 << 0", Some(1024));
    verify_expr_evaluates_to_i32("32 >> 6", Some(0));

    verify_expr_evaluates_to_i32("1 && 2", Some(1));
    verify_expr_evaluates_to_i32("1 && 0", Some(0));
    verify_expr_evaluates_to_i32("1 || 0", Some(1));
    verify_expr_evaluates_to_i32("0 || 0", Some(0));

    verify_expr_evaluates_to_i32("17 == 17", Some(1));
    verify_expr_evaluates_to_i32("17 == 16", Some(0));
    verify_expr_evaluates_to_i32("17 != 17", Some(0));
    verify_expr_evaluates_to_i32("17 != 16", Some(1));
    verify_expr_evaluates_to_i32("2 > 1", Some(1));
    verify_expr_evaluates_to_i32("2 >= 2", Some(1));
    verify_expr_evaluates_to_i32("-1 < 2", Some(1));
    verify_expr_evaluates_to_i32("3 <= 3", Some(1));

    verify_expr_evaluates_to_i64("-2147483648 + 0", Some(-2_147_483_648));
    verify_expr_evaluates_to_i32("-2147483647 + 0", Some(-2_147_483_647));
    verify_expr_evaluates_to_i32("2147483647 + 0", Some(2_147_483_647));
    verify_expr_evaluates_to_i32("2147483647 * 1", Some(2_147_483_647));
    verify_expr_evaluates_to_i32("2147483647 / -1", Some(-2_147_483_647));

    verify_expr_evaluates_to_f64("1.0 + 2.0", Some(3.0));
    verify_expr_evaluates_to_f64("1.0 - 2.0", Some(-1.0));
    verify_expr_evaluates_to_f64("2.5 * 2.5", Some(6.25));
    verify_expr_evaluates_to_f64("9223372036854775808.0 * 9223372036854775808.0", Some(8.507059173023462e37));
    verify_expr_evaluates_to_f64("11 / 2.0", Some(5.5));
    verify_expr_evaluates_to_f64("(1 + 2 + 3) / 2.0", Some(3.0));

    verify_expr_evaluates_to_f64("1.0f + 2.0f + 3.0", Some(6.0));
    verify_expr_evaluates_to_f32("1.0f + 3", Some(4.0));
    verify_expr_evaluates_to_f32("3.0f / 3", Some(1.0));
    verify_expr_evaluates_to_f64("3.0 / 3", Some(1.0));
    verify_expr_evaluates_to_f32("2 * 1.0f", Some(2.0));
    verify_expr_evaluates_to_f64("2 * 1.0", Some(2.0));

    verify_expr_evaluates_to_i32("17.0 == 17.0", Some(1));
    verify_expr_evaluates_to_i32("17.0 == 17", Some(1));
    verify_expr_evaluates_to_i32("17.0 != 17.01", Some(1));
    verify_expr_evaluates_to_i32("-1.0 < 0.0", Some(1));
    verify_expr_evaluates_to_i32("0.1 > 0.01", Some(1));
    verify_expr_evaluates_to_i32("0.1 >= 0.1", Some(1));
    verify_expr_evaluates_to_i32("0.1 <= 0.2", Some(1));
}

#[test]
fn ternary_expressions() {
    verify_expr_evaluates_to_i32("1 + 2 == 3 ? 100 : 1", Some(100));
    verify_expr_evaluates_to_i32("1 + 4 == 3 ? 101 : 2", Some(2));
    verify_expr_evaluates_to_i32("1 * 4 != 4 ? 102 : 3", Some(3));
    verify_expr_evaluates_to_i32("1 * 4 != 5 ? 103 : 4", Some(103));

    verify_expr_evaluates_to_i32("1 + 2 == 3 ? 3 * 6 / 2 : 0", Some(9));

    verify_expr_evaluates_to_i32("51 == 51 ? -33 : 0", Some(-33));
    verify_expr_evaluates_to_i32("51 == 50 ? -33 : -1", Some(-1));
    verify_expr_evaluates_to_i32("2 > 1 ? 10 : 0", Some(10));
    verify_expr_evaluates_to_i32("2 >= 2 ? 11 : 0", Some(11));
    verify_expr_evaluates_to_i32("-1 < 2 ? 12 : 0", Some(12));
    verify_expr_evaluates_to_i32("3 <= 3 ? 13 : 0", Some(13));

    verify_expr_evaluates_to_i64("1 < 2 ? 13ll : 0", Some(13));
    verify_expr_evaluates_to_u64("1 < 0 ? 13l : 0ull", Some(0));

    verify_expr_evaluates_to_f64("1 < 2 ? 1.0 : 2.0", Some(1.0));
    verify_expr_evaluates_to_f64("1 > 2 ? 1.0 : 2.0", Some(2.0));
    verify_expr_evaluates_to_f64("15.01 > 15.0 ? -0.111 : 0.222", Some(-0.111));
    verify_expr_evaluates_to_f64("11 < 2 ? 1.0 : 2.0f", Some(2.0));
    verify_expr_evaluates_to_f64("11 > 2 ? 1.0f : 2.0", Some(1.0));

    verify_expr_evaluates_to_f32("1 < 2 ? 1.0f : 2.0f", Some(1.0));
    verify_expr_evaluates_to_f32("1 > 2 ? 1.0f : 2.0f", Some(2.0));
    verify_expr_evaluates_to_f32("15.01 > 15.0 ? -0.111f : 0.222f", Some(-0.111));
}

#[test]
fn casts() {
    verify_expr_evaluates_to_i8("(char)-1", Some(-1));
    verify_expr_evaluates_to_i8("(char)8589934592", Some(0));
    verify_expr_evaluates_to_i8("(char)-1.0", Some(-1));
    verify_expr_evaluates_to_i8("(char)1.0", Some(1));
    verify_expr_evaluates_to_i8("(char)(int)(long)-1", Some(-1));

    verify_expr_evaluates_to_i16("(short)-1", Some(-1));
    verify_expr_evaluates_to_i16("(short)8589934592", Some(0));
    verify_expr_evaluates_to_i16("(short)-1.0", Some(-1));
    verify_expr_evaluates_to_i16("(short)1.0", Some(1));
    verify_expr_evaluates_to_i16("(short)(int)(long)-1", Some(-1));

    verify_expr_evaluates_to_u16("(unsigned short)8589934592", Some(0));
    verify_expr_evaluates_to_u16("(unsigned short)-3.0", Some(0)); // This would be UB at runtime
    verify_expr_evaluates_to_u16("(unsigned short)3.0", Some(3));

    verify_expr_evaluates_to_i32("(int)8589934592", Some(0));
    verify_expr_evaluates_to_i32("(int)(8589934592 + 8589934592)", Some(0));
    verify_expr_evaluates_to_i32("(int)(long)(int)1", Some(1));
    verify_expr_evaluates_to_i32("(int)(long)(int)-1", Some(-1));
    verify_expr_evaluates_to_i32("(int)-5.0", Some(-5));
    verify_expr_evaluates_to_i32("(int)-0.123", Some(0));
    verify_expr_evaluates_to_i32("(int)0.123", Some(0));
    verify_expr_evaluates_to_i32("(int)5.0", Some(5));
    verify_expr_evaluates_to_i32("(int)(short)(long)-1", Some(-1));

    verify_expr_evaluates_to_u32("(unsigned int)0", Some(0));
    verify_expr_evaluates_to_u32("(unsigned int)-1", Some(4294967295));
    verify_expr_evaluates_to_u32("(unsigned int)100", Some(100));
    verify_expr_evaluates_to_u32("(unsigned int)8589934592", Some(0));
    verify_expr_evaluates_to_u32("(unsigned int)100.123", Some(100));

    verify_expr_evaluates_to_i64("(long)(short)8589934592", Some(0));
    verify_expr_evaluates_to_i64("(long)8589934592", Some(8589934592));
    verify_expr_evaluates_to_i64("(long)8589934592 + 1LL", Some(8589934593));
    verify_expr_evaluates_to_i64("(long)8589934592 + 1L", Some(8589934593));
    verify_expr_evaluates_to_i64("(long)8589934592 + 1", Some(8589934593));
    verify_expr_evaluates_to_i64("(long)-99.99", Some(-99));
    verify_expr_evaluates_to_i64("(long)99.99", Some(99));
    verify_expr_evaluates_to_i64("(long)(short)(char)-1", Some(-1));

    verify_expr_evaluates_to_u64("(unsigned long)0", Some(0));
    verify_expr_evaluates_to_u64("(unsigned long)100", Some(100));
    verify_expr_evaluates_to_u64("(unsigned long long)100", Some(100));
    verify_expr_evaluates_to_u64("(unsigned long)-1", Some(18446744073709551615));
    verify_expr_evaluates_to_u64("(unsigned long)(-1LL)", Some(18446744073709551615));
    verify_expr_evaluates_to_u64("(unsigned long)3.14", Some(3));

    verify_expr_evaluates_to_f64("(double)1", Some(1.0));
    verify_expr_evaluates_to_f64("(double)-1", Some(-1.0));
    verify_expr_evaluates_to_f64("(double)1 + (double)2", Some(3.0));
    verify_expr_evaluates_to_f64("(unsigned int)(1 + 2 + 3) / (double)2", Some(3.0));

    verify_expr_evaluates_to_f64("(double)0.0f", Some(0.0));
    verify_expr_evaluates_to_f64("(double)-0.0f", Some(-0.0));
    verify_expr_evaluates_to_f64("(double)1.0f", Some(1.0));
    verify_expr_evaluates_to_f64("(double)(12e30f + -12e30f)", Some(0.0));
    verify_expr_evaluates_to_f64("(double)((2.0f + 3.0f) - 127.5f * 4.0f)", Some(-505.0));

    verify_expr_evaluates_to_f32("(float)0.0", Some(0.0));
    verify_expr_evaluates_to_f32("(float)-0.0", Some(-0.0));
    verify_expr_evaluates_to_f32("(float)1.0", Some(1.0));
    verify_expr_evaluates_to_f32("(float)(12e30 + -12e30)", Some(0.0));
    verify_expr_evaluates_to_f32("(float)((2.0 + 3.0) - 127.5 * 4.0)", Some(-505.0));
}

#[test]
fn overflow() {
    verify_expr_evaluates_to_i64("9223372036854775807 * 2", Some(-2));
    verify_expr_evaluates_to_i64("-9223372036854775807 * 2", Some(2));
    verify_expr_evaluates_to_i64("9223372036854775807 + 1", Some(-9223372036854775808));
    verify_expr_evaluates_to_i64("-9223372036854775808 - 1", Some(9223372036854775807));
    verify_expr_evaluates_to_i64("9223372036854775807 * 9223372036854775807", Some(1));
    verify_expr_evaluates_to_i64("-9223372036854775808 * 2", Some(0));

    verify_expr_evaluates_to_f64("1.7976931348623157E+308 * 1.1", Some(f64::INFINITY));
    verify_expr_evaluates_to_f64("1.7976931348623157E+308 * -1.1", Some(f64::NEG_INFINITY));
    verify_expr_evaluates_to_f64("1.0e100000 * 2.0e100000", Some(f64::INFINITY));
    verify_expr_evaluates_to_f64("1.0e100000 * -2.0e100000", Some(f64::NEG_INFINITY));

    verify_expr_evaluates_to_f32("3.40282347E+38f + 3.4e38f", Some(f32::INFINITY));
    verify_expr_evaluates_to_f32("3.40282347E+38f * 1.1f", Some(f32::INFINITY));
    verify_expr_evaluates_to_f32("3.40282347E+38f * -1.1f", Some(f32::NEG_INFINITY));
    verify_expr_evaluates_to_f32("1.0e38f * 1.0e38f", Some(f32::INFINITY));
    verify_expr_evaluates_to_f32("1.0e38f * -1.0e38f", Some(f32::NEG_INFINITY));
}

#[test]
fn cannot_shift_by_negative() {
    verify_expr_evaluates_to_i32("1024 << -1", None);
    verify_expr_evaluates_to_i32("1024 >> -1", None);
}

#[test]
fn cannot_div_by_zero() {
    verify_expr_evaluates_to_i32("99 % 0", None);
    verify_expr_evaluates_to_i32("6 / 0", None);
    verify_expr_evaluates_to_i32("7 / (3 - 3)", None);
    verify_expr_evaluates_to_i32("8 / (2 * 0)", None);
    verify_expr_evaluates_to_i32("9 / ((3 / 3) - 1)", None);
    verify_expr_evaluates_to_i32("512 / (32 >> 6)", None);
}

#[test]
fn float_div_by_zero_equals_infinity() {
    verify_expr_evaluates_to_f32("1.0f / 0.0f", Some(f32::INFINITY));
    verify_expr_evaluates_to_f32("1.0f / -0.0f", Some(f32::NEG_INFINITY));

    verify_expr_evaluates_to_f64("1.0 / 0.0", Some(f64::INFINITY));
    verify_expr_evaluates_to_f64("1.0 / -0.0", Some(f64::NEG_INFINITY));
}

#[test]
fn no_invalid_operations_on_float() {
    assert_eq!(evaluate_expr("~1.0"), None);
    assert_eq!(evaluate_expr("1.0 << 1"), None);
    assert_eq!(evaluate_expr("1.0 >> 1"), None);
    assert_eq!(evaluate_expr("1.0 & 2.0"), None);
    assert_eq!(evaluate_expr("1.0 | 2.0"), None);
    assert_eq!(evaluate_expr("1.0 ^ 1"), None);
    assert_eq!(evaluate_expr("1 ^ 1.0"), None);
    assert_eq!(evaluate_expr("1.0 % 5"), None);
    assert_eq!(evaluate_expr("5 % 5.0"), None);

    assert_eq!(evaluate_expr("~1.0f"), None);
    assert_eq!(evaluate_expr("1.0f << 1"), None);
    assert_eq!(evaluate_expr("1.0f >> 1"), None);
    assert_eq!(evaluate_expr("1.0f & 2.0"), None);
    assert_eq!(evaluate_expr("1.0f | 2.0"), None);
    assert_eq!(evaluate_expr("1.0f ^ 1"), None);
    assert_eq!(evaluate_expr("1 ^ 1.0f"), None);
    assert_eq!(evaluate_expr("1.0f % 5"), None);
    assert_eq!(evaluate_expr("5 % 5.0f"), None);
}

#[test]
fn pointer_initializer() {
    let valid_cases = vec![
        "static int* ptr = 0;",
        "static int* ptr = 10 - 10;",
        "static int* ptr = (5 / 5) - 1;",
        "static int value = 0; static int* ptr = &value;",
        "static long arr[4] = {1, 2, 3, 4}; static long* ptr = arr;",
        "static long arr[4] = {1, 2, 3, 4}; static long* ptr = &arr[2];",
        "static long arr[4] = {1, 2, 3, 4}; static long* ptr = &arr[4] - 4;",
        "static double arr[4] = {1., 2., 3., 4.}; static double (*ptr)[4] = &arr;",
        "static long arr[4] = {1, 2, 3, 4}; static int *ptr = (int*)&arr[1];",
    ];

    for case in valid_cases {
        let mut driver = compiler_driver::Driver::for_testing();
        parse_and_type_check(&mut driver, case);
        assert!(!driver.has_error_diagnostics());
    }

    let invalid_cases = vec![
        "static int* ptr = 1;",
        "static int* ptr = 10 - 9;",
        "static int* ptr = (5 / 5);",
        "static float arr[4]; static int *ptr = arr;",
        "static float arr[4]; static float *ptr = &arr;",
        "static int *p1; static int *p2 = p1;",
    ];

    for case in invalid_cases {
        let mut driver = compiler_driver::Driver::for_testing();
        parse_and_type_check(&mut driver, case);
        assert!(driver.has_error_diagnostics());
    }
}

#[test]
fn pointer_arithmetic() {
    let valid_cases = vec![
        "static int a = 1; static int *ptr = 10 + &a - 10;",
        "static float arr[3] = {11, 22, 33}; static float *ptr = &arr[0] + 1;",
        "static float arr[3] = {11, 22, 33}; static float *ptr = &arr[0] + 1;",
        "static float arr[3] = {11, 22, 33}; static float *ptr = 1 + &arr[0] + 1;",
        "static long arr[4] = {1, 2, 3, 4}; static long diff = &arr[4] - &arr[0];",
    ];

    for case in valid_cases {
        let mut driver = compiler_driver::Driver::for_testing();
        parse_and_type_check(&mut driver, case);
        assert!(!driver.has_error_diagnostics());
    }

    let invalid_cases = vec![
        "static int x[1]; static int* ptr = &x[0] + 1.0;",
        "static int x[1]; static int* ptr = 1.0f + &x[0];",
        "static int x[1]; static float y[1]; static int* ptr = &x[0] + &y[0];",
        "static int x[1]; static float y[1]; static long diff = &x[0] - &y[0];",
        "static int value = 0; static int* ptr = &value + &value;",
    ];

    for case in invalid_cases {
        let mut driver = compiler_driver::Driver::for_testing();
        parse_and_type_check(&mut driver, case);
        assert!(driver.has_error_diagnostics());
    }
}

fn verify_expr_evaluates_to_i8(expression_source_code: &str, expected: Option<i8>) {
    let value = evaluate_expr(expression_source_code);

    if expected.is_none() {
        assert_eq!(value, None);
        return;
    }

    let Some(AstConstantValue::Integer(const_integer)) = value else {
        assert!(value.is_some(), "Expression '{expression_source_code}' did not evaluate to Integer");
        panic!();
    };

    match const_integer {
        AstConstantInteger::Char(value) => {
            assert_eq!(expected, Some(value));
        }
        _ => assert!(
            false,
            "Expression '{expression_source_code}' evaluated to {} {const_integer}",
            const_integer.get_ast_type()
        ),
    }
}

fn verify_expr_evaluates_to_i16(expression_source_code: &str, expected: Option<i16>) {
    let value = evaluate_expr(expression_source_code);

    if expected.is_none() {
        assert_eq!(value, None);
        return;
    }

    let Some(AstConstantValue::Integer(const_integer)) = value else {
        assert!(value.is_some(), "Expression '{expression_source_code}' did not evaluate to Integer");
        panic!();
    };

    match const_integer {
        AstConstantInteger::Short(value) => {
            assert_eq!(expected, Some(value));
        }
        _ => assert!(
            false,
            "Expression '{expression_source_code}' evaluated to {} {const_integer}",
            const_integer.get_ast_type()
        ),
    }
}

fn verify_expr_evaluates_to_u16(expression_source_code: &str, expected: Option<u16>) {
    let value = evaluate_expr(expression_source_code);

    if expected.is_none() {
        assert_eq!(value, None);
        return;
    }

    let Some(AstConstantValue::Integer(const_integer)) = value else {
        assert!(value.is_some(), "Expression '{expression_source_code}' did not evaluate to Integer");
        panic!();
    };

    match const_integer {
        AstConstantInteger::UnsignedShort(value) => {
            assert_eq!(expected, Some(value));
        }
        _ => assert!(
            false,
            "Expression '{expression_source_code}' evaluated to {} {const_integer}",
            const_integer.get_ast_type()
        ),
    }
}

fn verify_expr_evaluates_to_i32(expression_source_code: &str, expected: Option<i32>) {
    let value = evaluate_expr(expression_source_code);

    if expected.is_none() {
        assert_eq!(value, None);
        return;
    }

    let Some(AstConstantValue::Integer(const_integer)) = value else {
        assert!(value.is_some(), "Expression '{expression_source_code}' did not evaluate to Integer");
        panic!();
    };

    match const_integer {
        AstConstantInteger::Int(value) => {
            assert_eq!(expected, Some(value));
        }
        _ => assert!(
            false,
            "Expression '{expression_source_code}' evaluated to {} {const_integer}",
            const_integer.get_ast_type()
        ),
    }
}

fn verify_expr_evaluates_to_u32(expression_source_code: &str, expected: Option<u32>) {
    let value = evaluate_expr(expression_source_code);

    if expected.is_none() {
        assert_eq!(value, None);
        return;
    }

    let Some(AstConstantValue::Integer(const_integer)) = value else {
        assert!(value.is_some(), "Expression '{expression_source_code}' did not evaluate to Integer");
        panic!();
    };

    match const_integer {
        AstConstantInteger::UnsignedInt(value) => {
            assert_eq!(expected, Some(value));
        }
        _ => assert!(
            false,
            "Expression '{expression_source_code}' evaluated to {} {const_integer}",
            const_integer.get_ast_type()
        ),
    }
}

fn verify_expr_evaluates_to_i64(expression_source_code: &str, expected: Option<i64>) {
    let value = evaluate_expr(expression_source_code);

    if expected.is_none() {
        assert_eq!(value, None);
        return;
    }

    let Some(AstConstantValue::Integer(const_integer)) = value else {
        assert!(value.is_some(), "Expression '{expression_source_code}' did not evaluate to Integer");
        panic!();
    };

    match const_integer {
        AstConstantInteger::LongLong(value) => {
            assert_eq!(expected, Some(value));
        }
        _ => assert!(
            false,
            "Expression '{expression_source_code}' evaluated to {} {const_integer}",
            const_integer.get_ast_type()
        ),
    }
}

fn verify_expr_evaluates_to_u64(expression_source_code: &str, expected: Option<u64>) {
    let value = evaluate_expr(expression_source_code);

    if expected.is_none() {
        assert_eq!(value, None);
        return;
    }

    let Some(AstConstantValue::Integer(const_integer)) = value else {
        assert!(value.is_some(), "Expression '{expression_source_code}' did not evaluate to Integer");
        panic!();
    };

    match const_integer {
        AstConstantInteger::UnsignedLongLong(value) => {
            assert_eq!(expected, Some(value));
        }
        _ => assert!(
            false,
            "Expression '{expression_source_code}' evaluated to {} {const_integer}",
            const_integer.get_ast_type()
        ),
    }
}

fn verify_expr_evaluates_to_f64(expression_source_code: &str, expected: Option<f64>) {
    let value = evaluate_expr(expression_source_code);

    if expected.is_none() {
        assert_eq!(value, None);
        return;
    }

    let Some(AstConstantValue::Fp(constant_fp)) = value else {
        assert!(value.is_some(), "Expression '{expression_source_code}' did not evaluate to Fp");
        panic!();
    };

    match constant_fp {
        AstConstantFp::Double(value) => {
            assert_eq!(expected, Some(value));
        }
        _ => assert!(
            false,
            "Expression '{expression_source_code}' evaluated to {} {constant_fp}",
            constant_fp.get_ast_type()
        ),
    }
}

fn verify_expr_evaluates_to_f32(expression_source_code: &str, expected: Option<f32>) {
    let value = evaluate_expr(expression_source_code);

    if expected.is_none() {
        assert_eq!(value, None);
        return;
    }

    let Some(AstConstantValue::Fp(constant_fp)) = value else {
        assert!(value.is_some(), "Expression '{expression_source_code}' did not evaluate to Fp");
        panic!();
    };

    match constant_fp {
        AstConstantFp::Float(value) => {
            assert_eq!(expected, Some(value));
        }
        _ => assert!(
            false,
            "Expression '{expression_source_code}' evaluated to {} {constant_fp}",
            constant_fp.get_ast_type()
        ),
    }
}

fn verify_expr_evaluates_to_string(expression_source_code: &str, expected: Option<&str>) {
    let value = evaluate_expr(expression_source_code);

    if expected.is_none() {
        assert_eq!(value, None);
        return;
    }

    let Some(AstConstantValue::String { ascii, .. }) = value else {
        assert!(value.is_some(), "Expression '{expression_source_code}' did not evaluate to String");
        panic!();
    };

    let ascii_joined = ascii.join("");
    assert_eq!(ascii_joined, expected.unwrap())
}

fn evaluate_expr<'a>(expression_source_code: &str) -> Option<AstConstantValue> {
    let mut driver = compiler_driver::Driver::for_testing();

    let cursor = Cursor::new(expression_source_code.as_bytes());
    let mut reader = BufReader::new(cursor);

    let tokens = lexer::lex_buf_reader(&mut driver, &mut reader);
    if driver.has_error_diagnostics() {
        driver.debug_print_diagnostics();
        assert!(false, "Lexer emitted errors");
    }

    let mut parser = Parser::new(tokens);

    let expr = expr::parse_full_expression(&mut parser, &mut driver);
    if driver.has_error_diagnostics() {
        driver.debug_print_diagnostics();
        assert!(false, "Parser emitted errors");
    }

    assert!(expr.is_ok(), "Did not parse {}", expression_source_code);
    let expr = expr.unwrap();

    let mut chk = type_check::TypeChecker::default();
    let mut eval = constant_eval::Eval::new(&mut chk, &mut driver);
    eval.set_diagnostics_enabled(false);
    eval.evaluate_full_expr(&expr)
}

fn parse_and_type_check(driver: &mut compiler_driver::Driver, source: &str) {
    let mut parser = utils::make_parser(driver, source);
    let ast_root = recursive_descent::parse_translation_unit(&mut parser, driver);
    assert!(!driver.has_error_diagnostics());

    let mut ast_root = ast_root.unwrap();
    let Parser { metadata, .. } = parser;
    let mut chk = type_check::TypeChecker::new(metadata);

    _ = type_check::type_check(&mut ast_root, &mut chk, driver);
}
