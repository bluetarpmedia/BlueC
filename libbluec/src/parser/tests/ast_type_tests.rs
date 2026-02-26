// Copyright 2025-2026 Neil Henderson

use crate::parser::AstType;

#[test]
fn ast_type_bits() {
    assert_eq!(AstType::Bool.bits(), 8);

    assert_eq!(AstType::Char.bits(), 8);
    assert_eq!(AstType::SignedChar.bits(), 8);
    assert_eq!(AstType::Short.bits(), 16);
    assert_eq!(AstType::Int.bits(), 32);
    assert_eq!(AstType::Long.bits(), 64);
    assert_eq!(AstType::LongLong.bits(), 64);

    assert_eq!(AstType::UnsignedChar.bits(), 8);
    assert_eq!(AstType::UnsignedShort.bits(), 16);
    assert_eq!(AstType::UnsignedInt.bits(), 32);
    assert_eq!(AstType::UnsignedLong.bits(), 64);
    assert_eq!(AstType::UnsignedLongLong.bits(), 64);

    assert_eq!(AstType::new_pointer_to(AstType::Short).bits(), 64);
    assert_eq!(AstType::new_pointer_to(AstType::Int).bits(), 64);

    assert_eq!(AstType::new_array(AstType::Char, 10).bits() / 8, 10);
    assert_eq!(AstType::new_array(AstType::Short, 10).bits() / 8, 20);
    assert_eq!(AstType::new_array(AstType::Long, 10).bits() / 8, 80);
    assert_eq!(AstType::new_array(AstType::new_pointer_to(AstType::new_fn(AstType::Int, vec![])), 3).bits() / 8, 24);
}

#[test]
fn ast_type_is_scalar() {
    assert!(AstType::Bool.is_scalar());

    assert!(AstType::Char.is_scalar());
    assert!(AstType::SignedChar.is_scalar());
    assert!(AstType::Short.is_scalar());
    assert!(AstType::Int.is_scalar());
    assert!(AstType::Long.is_scalar());
    assert!(AstType::LongLong.is_scalar());

    assert!(AstType::UnsignedChar.is_scalar());
    assert!(AstType::UnsignedShort.is_scalar());
    assert!(AstType::UnsignedInt.is_scalar());
    assert!(AstType::UnsignedLong.is_scalar());
    assert!(AstType::UnsignedLongLong.is_scalar());

    assert!(AstType::Float.is_scalar());
    assert!(AstType::Double.is_scalar());
    assert!(AstType::LongDouble.is_scalar());

    assert!(!AstType::Void.is_scalar());
    assert!(!AstType::new_array(AstType::Short, 10).is_scalar());
    assert!(!AstType::new_fn(AstType::Void, vec![]).is_scalar());
}

#[test]
fn ast_type_is_aggregate() {
    assert!(AstType::new_array(AstType::Char, 0).is_aggregate());
    assert!(AstType::new_array(AstType::Short, 10).is_aggregate());
    assert!(AstType::new_array(AstType::Double, 1).is_aggregate());

    assert!(!AstType::Bool.is_aggregate());
    assert!(!AstType::Char.is_aggregate());
    assert!(!AstType::SignedChar.is_aggregate());
    assert!(!AstType::Short.is_aggregate());
    assert!(!AstType::Int.is_aggregate());
    assert!(!AstType::Long.is_aggregate());
    assert!(!AstType::LongLong.is_aggregate());

    assert!(!AstType::UnsignedChar.is_aggregate());
    assert!(!AstType::UnsignedShort.is_aggregate());
    assert!(!AstType::UnsignedInt.is_aggregate());
    assert!(!AstType::UnsignedLong.is_aggregate());
    assert!(!AstType::UnsignedLongLong.is_aggregate());

    assert!(!AstType::Float.is_aggregate());
    assert!(!AstType::Double.is_aggregate());
    assert!(!AstType::LongDouble.is_aggregate());

    assert!(!AstType::Void.is_aggregate());
    assert!(!AstType::new_fn(AstType::Void, vec![]).is_aggregate());
}

#[test]
fn ast_type_is_pointer() {
    assert!(AstType::new_pointer_to(AstType::Int).is_pointer());
    assert!(!AstType::new_pointer_to(AstType::Int).is_pointer_to_void());

    assert!(AstType::new_pointer_to(AstType::new_array(AstType::Char, 1)).is_pointer());

    assert!(AstType::new_pointer_to(AstType::new_fn(AstType::Char, vec![])).is_pointer());
    assert!(AstType::new_pointer_to(AstType::new_fn(AstType::Char, vec![])).is_function_pointer());

    assert!(AstType::new_pointer_to(AstType::Void).is_pointer());
    assert!(AstType::new_pointer_to(AstType::Void).is_pointer_to_void());

    assert!(!AstType::Void.is_pointer());
    assert!(!AstType::Void.is_pointer_to_void());
}

#[test]
fn ast_type_integer_promotion() {
    assert_eq!(AstType::Bool.promote_if_rank_lower_than_int(), (AstType::Int, true));
    assert_eq!(AstType::Char.promote_if_rank_lower_than_int(), (AstType::Int, true));
    assert_eq!(AstType::SignedChar.promote_if_rank_lower_than_int(), (AstType::Int, true));
    assert_eq!(AstType::UnsignedChar.promote_if_rank_lower_than_int(), (AstType::Int, true));
    assert_eq!(AstType::Short.promote_if_rank_lower_than_int(), (AstType::Int, true));
    assert_eq!(AstType::UnsignedShort.promote_if_rank_lower_than_int(), (AstType::Int, true));

    // No change
    assert_eq!(AstType::Void.promote_if_rank_lower_than_int(), (AstType::Void, false));
    assert_eq!(AstType::Float.promote_if_rank_lower_than_int(), (AstType::Float, false));
    assert_eq!(AstType::Double.promote_if_rank_lower_than_int(), (AstType::Double, false));
    assert_eq!(AstType::Int.promote_if_rank_lower_than_int(), (AstType::Int, false));
    assert_eq!(AstType::Long.promote_if_rank_lower_than_int(), (AstType::Long, false));
    assert_eq!(AstType::LongLong.promote_if_rank_lower_than_int(), (AstType::LongLong, false));
    assert_eq!(AstType::UnsignedInt.promote_if_rank_lower_than_int(), (AstType::UnsignedInt, false));
    assert_eq!(AstType::UnsignedLong.promote_if_rank_lower_than_int(), (AstType::UnsignedLong, false));
    assert_eq!(AstType::UnsignedLongLong.promote_if_rank_lower_than_int(), (AstType::UnsignedLongLong, false));
}

#[test]
fn ast_type_integer_common_type() {
    test_common_type(AstType::Int, AstType::Int, AstType::Int);
    test_common_type(AstType::UnsignedInt, AstType::UnsignedInt, AstType::UnsignedInt);

    // If both types have the same signedness then promote to the larger of the two.
    test_common_type(AstType::Char, AstType::Int, AstType::Int);
    test_common_type(AstType::SignedChar, AstType::Int, AstType::Int);
    test_common_type(AstType::Short, AstType::Int, AstType::Int);
    test_common_type(AstType::Short, AstType::Long, AstType::Long);
    test_common_type(AstType::Short, AstType::LongLong, AstType::LongLong);
    test_common_type(AstType::Int, AstType::Long, AstType::Long);
    test_common_type(AstType::Int, AstType::LongLong, AstType::LongLong);
    test_common_type(AstType::Long, AstType::LongLong, AstType::LongLong);

    test_common_type(AstType::Bool, AstType::UnsignedInt, AstType::UnsignedInt);
    test_common_type(AstType::UnsignedChar, AstType::UnsignedInt, AstType::UnsignedInt);
    test_common_type(AstType::UnsignedShort, AstType::UnsignedInt, AstType::UnsignedInt);
    test_common_type(AstType::UnsignedShort, AstType::UnsignedLong, AstType::UnsignedLong);
    test_common_type(AstType::UnsignedShort, AstType::UnsignedLongLong, AstType::UnsignedLongLong);
    test_common_type(AstType::UnsignedInt, AstType::UnsignedLong, AstType::UnsignedLong);
    test_common_type(AstType::UnsignedInt, AstType::UnsignedLongLong, AstType::UnsignedLongLong);
    test_common_type(AstType::UnsignedLong, AstType::UnsignedLongLong, AstType::UnsignedLongLong);

    // If the types have different signedness and their ranks are the same, or the unsigned one is higher, then
    // the common type is the unsigned one.
    test_common_type(AstType::Char, AstType::UnsignedShort, AstType::UnsignedShort);
    test_common_type(AstType::Short, AstType::UnsignedShort, AstType::UnsignedShort);
    test_common_type(AstType::Short, AstType::UnsignedInt, AstType::UnsignedInt);
    test_common_type(AstType::Short, AstType::UnsignedLong, AstType::UnsignedLong);
    test_common_type(AstType::Short, AstType::UnsignedLongLong, AstType::UnsignedLongLong);
    test_common_type(AstType::Int, AstType::UnsignedInt, AstType::UnsignedInt);
    test_common_type(AstType::Int, AstType::UnsignedLong, AstType::UnsignedLong);
    test_common_type(AstType::Int, AstType::UnsignedLongLong, AstType::UnsignedLongLong);
    test_common_type(AstType::Long, AstType::UnsignedLong, AstType::UnsignedLong);
    test_common_type(AstType::Long, AstType::UnsignedLongLong, AstType::UnsignedLongLong);

    // If the signed type has a higher rank, and if all the values of the unsigned type can fit in the signed type then the
    // common type is the signed one.
    test_common_type(AstType::Bool, AstType::Int, AstType::Int);
    test_common_type(AstType::UnsignedChar, AstType::Int, AstType::Int);
    test_common_type(AstType::UnsignedShort, AstType::Int, AstType::Int);
    test_common_type(AstType::UnsignedShort, AstType::Long, AstType::Long);
    test_common_type(AstType::UnsignedShort, AstType::LongLong, AstType::LongLong);
    test_common_type(AstType::UnsignedInt, AstType::Long, AstType::Long);
    test_common_type(AstType::UnsignedInt, AstType::LongLong, AstType::LongLong);

    // Otherwise, if the signed type has a higher rank, the common type is the unsigned version of the signed type.
    test_common_type(AstType::UnsignedLong, AstType::LongLong, AstType::UnsignedLongLong);
}

#[test]
fn ast_type_same_signedness() {
    assert!(AstType::Char.same_signedness(&AstType::SignedChar));
    assert!(AstType::Short.same_signedness(&AstType::Char));
    assert!(AstType::Short.same_signedness(&AstType::Int));
    assert!(AstType::Short.same_signedness(&AstType::Long));
    assert!(AstType::Short.same_signedness(&AstType::LongLong));

    assert!(AstType::UnsignedInt.same_signedness(&AstType::Bool));
    assert!(AstType::UnsignedInt.same_signedness(&AstType::UnsignedChar));
    assert!(AstType::UnsignedInt.same_signedness(&AstType::UnsignedShort));
    assert!(AstType::UnsignedInt.same_signedness(&AstType::UnsignedInt));
    assert!(AstType::UnsignedInt.same_signedness(&AstType::UnsignedLong));
    assert!(AstType::UnsignedInt.same_signedness(&AstType::UnsignedLongLong));

    assert_eq!(false, AstType::Bool.same_signedness(&AstType::Int));
    assert_eq!(false, AstType::UnsignedInt.same_signedness(&AstType::Short));
    assert_eq!(false, AstType::Int.same_signedness(&AstType::UnsignedShort));
}

#[test]
fn ast_type_can_hold_value() {
    assert_eq!(true, AstType::Bool.can_hold_value(0));
    assert_eq!(true, AstType::Bool.can_hold_value(1));
    assert_eq!(false, AstType::Bool.can_hold_value(2));

    assert_eq!(true, AstType::Char.can_hold_value(0));
    assert_eq!(true, AstType::Char.can_hold_value(127));
    assert_eq!(false, AstType::Char.can_hold_value(128));

    assert_eq!(true, AstType::UnsignedChar.can_hold_value(0));
    assert_eq!(true, AstType::UnsignedChar.can_hold_value(255));
    assert_eq!(false, AstType::UnsignedChar.can_hold_value(256));

    assert_eq!(true, AstType::Short.can_hold_value(0));
    assert_eq!(true, AstType::Short.can_hold_value(32767));
    assert_eq!(false, AstType::Short.can_hold_value(32768));

    assert_eq!(true, AstType::UnsignedShort.can_hold_value(0));
    assert_eq!(true, AstType::UnsignedShort.can_hold_value(65535));
    assert_eq!(false, AstType::UnsignedShort.can_hold_value(65536));

    assert_eq!(true, AstType::Int.can_hold_value(0));
    assert_eq!(true, AstType::Int.can_hold_value(2147483647));
    assert_eq!(false, AstType::Int.can_hold_value(2147483648));

    assert_eq!(true, AstType::UnsignedInt.can_hold_value(0));
    assert_eq!(true, AstType::UnsignedInt.can_hold_value(4294967295));
    assert_eq!(false, AstType::UnsignedInt.can_hold_value(4294967296));

    assert_eq!(true, AstType::Long.can_hold_value(0));
    assert_eq!(true, AstType::Long.can_hold_value(9223372036854775807));
    assert_eq!(false, AstType::Long.can_hold_value(9223372036854775808));

    assert_eq!(true, AstType::UnsignedLong.can_hold_value(0));
    assert_eq!(true, AstType::UnsignedLong.can_hold_value(18446744073709551615));
}

#[test]
fn ast_type_signed_integer_fits_inside() {
    test_fits_inside(
        AstType::Char,
        &[AstType::Char, AstType::SignedChar, AstType::Short, AstType::Int, AstType::Long, AstType::LongLong],
        &[
            AstType::Bool,
            AstType::UnsignedChar,
            AstType::UnsignedShort,
            AstType::UnsignedInt,
            AstType::UnsignedLong,
            AstType::UnsignedLongLong,
        ],
    );

    test_fits_inside(
        AstType::Short,
        &[AstType::Short, AstType::Int, AstType::Long, AstType::LongLong],
        &[
            AstType::Bool,
            AstType::Char,
            AstType::SignedChar,
            AstType::UnsignedChar,
            AstType::UnsignedShort,
            AstType::UnsignedInt,
            AstType::UnsignedLong,
            AstType::UnsignedLongLong,
        ],
    );

    test_fits_inside(
        AstType::Int,
        &[AstType::Int, AstType::Long, AstType::LongLong],
        &[
            AstType::Bool,
            AstType::Char,
            AstType::SignedChar,
            AstType::UnsignedChar,
            AstType::Short,
            AstType::UnsignedShort,
            AstType::UnsignedInt,
            AstType::UnsignedLong,
            AstType::UnsignedLongLong,
        ],
    );

    test_fits_inside(
        AstType::Long,
        &[AstType::Long, AstType::LongLong],
        &[
            AstType::Bool,
            AstType::Char,
            AstType::SignedChar,
            AstType::UnsignedChar,
            AstType::Short,
            AstType::Int,
            AstType::UnsignedShort,
            AstType::UnsignedInt,
            AstType::UnsignedLong,
            AstType::UnsignedLongLong,
        ],
    );

    test_fits_inside(
        AstType::LongLong,
        &[AstType::Long, AstType::LongLong],
        &[
            AstType::Bool,
            AstType::Char,
            AstType::SignedChar,
            AstType::UnsignedChar,
            AstType::Short,
            AstType::Int,
            AstType::UnsignedShort,
            AstType::UnsignedInt,
            AstType::UnsignedLong,
            AstType::UnsignedLongLong,
        ],
    );
}

#[test]
fn ast_type_unsigned_integer_fits_inside() {
    test_fits_inside(
        AstType::Bool,
        &[
            AstType::Bool,
            AstType::Char,
            AstType::SignedChar,
            AstType::UnsignedChar,
            AstType::Short,
            AstType::Int,
            AstType::Long,
            AstType::LongLong,
            AstType::UnsignedShort,
            AstType::UnsignedInt,
            AstType::UnsignedLong,
            AstType::UnsignedLongLong,
        ],
        &[],
    );

    test_fits_inside(
        AstType::UnsignedChar,
        &[
            AstType::UnsignedChar,
            AstType::UnsignedShort,
            AstType::Int,
            AstType::Long,
            AstType::LongLong,
            AstType::UnsignedInt,
            AstType::UnsignedLong,
            AstType::UnsignedLongLong,
        ],
        &[AstType::Bool, AstType::Char, AstType::SignedChar],
    );

    test_fits_inside(
        AstType::UnsignedShort,
        &[
            AstType::UnsignedShort,
            AstType::Int,
            AstType::Long,
            AstType::LongLong,
            AstType::UnsignedInt,
            AstType::UnsignedLong,
            AstType::UnsignedLongLong,
        ],
        &[AstType::Bool, AstType::Char, AstType::SignedChar, AstType::UnsignedChar, AstType::Short],
    );

    test_fits_inside(
        AstType::UnsignedInt,
        &[AstType::Long, AstType::LongLong, AstType::UnsignedInt, AstType::UnsignedLong, AstType::UnsignedLongLong],
        &[
            AstType::Bool,
            AstType::Char,
            AstType::SignedChar,
            AstType::UnsignedChar,
            AstType::Short,
            AstType::Int,
            AstType::UnsignedShort,
        ],
    );

    test_fits_inside(
        AstType::UnsignedLong,
        &[AstType::UnsignedLong, AstType::UnsignedLongLong],
        &[
            AstType::Bool,
            AstType::Char,
            AstType::SignedChar,
            AstType::UnsignedChar,
            AstType::Short,
            AstType::Int,
            AstType::Long,
            AstType::LongLong,
            AstType::UnsignedShort,
            AstType::UnsignedInt,
        ],
    );

    test_fits_inside(
        AstType::UnsignedLong,
        &[AstType::UnsignedLong, AstType::UnsignedLongLong],
        &[
            AstType::Bool,
            AstType::Char,
            AstType::SignedChar,
            AstType::UnsignedChar,
            AstType::Short,
            AstType::Int,
            AstType::Long,
            AstType::LongLong,
            AstType::UnsignedShort,
            AstType::UnsignedInt,
        ],
    );
}

#[test]
fn ast_type_to_string_simple() {
    assert_eq!(AstType::Int.to_string(), "int");
    assert_eq!(AstType::new_pointer_to(AstType::Short).to_string(), "short *");
    assert_eq!(AstType::new_pointer_to(AstType::new_pointer_to(AstType::Double)).to_string(), "double **");
}

#[test]
fn ast_type_to_string_arrays() {
    assert_eq!(AstType::new_array(AstType::Bool, 32).to_string(), "_Bool [32]");
    assert_eq!(AstType::new_array(AstType::Double, 4).to_string(), "double [4]");
    assert_eq!(AstType::new_array(AstType::UnsignedLongLong, 16).to_string(), "unsigned long long [16]");
    assert_eq!(AstType::new_array(AstType::new_array(AstType::Int, 2), 5).to_string(), "int [5][2]");
    assert_eq!(AstType::new_array(AstType::new_array(AstType::Float, 10), 5).to_string(), "float [5][10]");
}

#[test]
fn ast_type_to_string_functions() {
    assert_eq!(AstType::new_fn(AstType::Int, vec![]).to_string(), "int (void)");
    assert_eq!(
        AstType::new_fn(AstType::UnsignedShort, vec![AstType::Float, AstType::Double]).to_string(),
        "unsigned short (float, double)"
    );

    // "int *(void)" (Function returning a pointer)
    assert_eq!(AstType::new_fn(AstType::new_pointer_to(AstType::Int), vec![]).to_string(), "int *(void)");
}

#[test]
fn ast_type_to_string_function_pointers() {
    assert_eq!(AstType::new_pointer_to(AstType::new_fn(AstType::Int, vec![])).to_string(), "int (*)(void)");

    assert_eq!(
        AstType::new_pointer_to(AstType::new_fn(AstType::Int, vec![AstType::Int, AstType::Int])).to_string(),
        "int (*)(int, int)"
    );

    assert_eq!(
        AstType::new_pointer_to(AstType::new_fn(AstType::Float, vec![AstType::Int, AstType::Int])).to_string(),
        "float (*)(int, int)"
    );
    assert_eq!(
        AstType::new_pointer_to(AstType::new_fn(
            AstType::Int,
            vec![AstType::new_pointer_to(AstType::new_fn(AstType::Int, vec![]))]
        ))
        .to_string(),
        "int (*)(int (*)(void))"
    );
}

#[test]
fn ast_type_to_string_mixed() {
    // "int *[3]" (Array of 3 pointers to int)
    assert_eq!(AstType::new_array(AstType::new_pointer_to(AstType::Int), 3).to_string(), "int *[3]");

    // float *[6] (Array of 6 pointers to float)
    assert_eq!(AstType::new_array(AstType::new_pointer_to(AstType::Float), 6).to_string(), "float *[6]");

    // "int (*)[3]" (Pointer to an array of 3 ints)
    assert_eq!(AstType::new_pointer_to(AstType::new_array(AstType::Int, 3)).to_string(), "int (*)[3]");

    // "int (*[5])[3]" (Array of 5 pointers to arrays of 3 ints)
    assert_eq!(
        AstType::new_array(AstType::new_pointer_to(AstType::new_array(AstType::Int, 3)), 5).to_string(),
        "int (*[5])[3]"
    );

    // long *[4][3] (Array of 4 arrays of 3 pointers to long)
    assert_eq!(
        AstType::new_array(AstType::new_array(AstType::new_pointer_to(AstType::Long), 3), 4).to_string(),
        "long *[4][3]"
    );
}

#[test]
fn ast_type_to_string_complex() {
    // "float (*[10])(int)" (Array of 10 function pointers)
    assert_eq!(
        AstType::new_array(AstType::new_pointer_to(AstType::new_fn(AstType::Float, vec![AstType::Int])), 10)
            .to_string(),
        "float (*[10])(int)"
    );

    // "int (*(void))[5]" (Function returning a pointer to an array)
    assert_eq!(
        AstType::new_fn(AstType::new_pointer_to(AstType::new_array(AstType::Int, 5)), vec![]).to_string(),
        "int (*(void))[5]"
    );
}

fn test_common_type(a: AstType, b: AstType, common: AstType) {
    assert_eq!(AstType::get_common_type(&a, &b), common);
    assert_eq!(AstType::get_common_type(&b, &a), common);
}

fn test_fits_inside(data_type: AstType, yes: &[AstType], no: &[AstType]) {
    for t in yes {
        assert!(data_type.fits_inside(&t), "Expected {data_type} to fit inside {t}");
    }
    for t in no {
        assert!(!data_type.fits_inside(&t), "Did not expect {data_type} to fit inside {t}");
    }
}
