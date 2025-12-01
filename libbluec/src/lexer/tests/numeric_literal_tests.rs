// Copyright 2025 Neil Henderson, Blue Tarp Media.

use super::super::numeric_literals::{
    extract_valid_float_literal_suffix_chars, extract_valid_integer_literal_suffix_chars, make_float_literal_suffix,
    make_integer_literal_suffix,
};

use crate::compiler_driver;
use crate::lexer::{FloatLiteralSuffix, IntegerLiteralSuffix};

#[test]
fn make_integer_literal_suffix_test() {
    let mut driver = compiler_driver::Driver::for_testing();

    assert_eq!(make_integer_literal_suffix(&mut driver, 0, 0, ""), Ok(None));
    assert_eq!(make_integer_literal_suffix(&mut driver, 0, 0, "L"), Ok(Some(IntegerLiteralSuffix::L)));
    assert_eq!(make_integer_literal_suffix(&mut driver, 0, 0, "LL"), Ok(Some(IntegerLiteralSuffix::LL)));
    assert_eq!(make_integer_literal_suffix(&mut driver, 0, 0, "U"), Ok(Some(IntegerLiteralSuffix::U)));
    assert_eq!(make_integer_literal_suffix(&mut driver, 0, 0, "UL"), Ok(Some(IntegerLiteralSuffix::UL)));
    assert_eq!(make_integer_literal_suffix(&mut driver, 0, 0, "ULL"), Ok(Some(IntegerLiteralSuffix::ULL)));

    assert_eq!(make_integer_literal_suffix(&mut driver, 0, 0, "l"), Ok(Some(IntegerLiteralSuffix::L)));
    assert_eq!(make_integer_literal_suffix(&mut driver, 0, 0, "ll"), Ok(Some(IntegerLiteralSuffix::LL)));
    assert_eq!(make_integer_literal_suffix(&mut driver, 0, 0, "u"), Ok(Some(IntegerLiteralSuffix::U)));
    assert_eq!(make_integer_literal_suffix(&mut driver, 0, 0, "ul"), Ok(Some(IntegerLiteralSuffix::UL)));
    assert_eq!(make_integer_literal_suffix(&mut driver, 0, 0, "ull"), Ok(Some(IntegerLiteralSuffix::ULL)));

    assert_eq!(make_integer_literal_suffix(&mut driver, 0, 0, "uLL"), Ok(Some(IntegerLiteralSuffix::ULL)));
    assert_eq!(make_integer_literal_suffix(&mut driver, 0, 0, "Ull"), Ok(Some(IntegerLiteralSuffix::ULL)));

    assert!(make_integer_literal_suffix(&mut driver, 0, 0, "uLl").is_err());
    assert!(make_integer_literal_suffix(&mut driver, 0, 0, "uu").is_err());
    assert!(make_integer_literal_suffix(&mut driver, 0, 0, "uU").is_err());
    assert!(make_integer_literal_suffix(&mut driver, 0, 0, "LUL").is_err());
}

#[test]
fn make_float_literal_suffix_test() {
    let mut driver = compiler_driver::Driver::for_testing();

    assert_eq!(make_float_literal_suffix(&mut driver, 0, 0, ""), Ok(None));
    assert_eq!(make_float_literal_suffix(&mut driver, 0, 0, "f"), Ok(Some(FloatLiteralSuffix::F)));
    assert_eq!(make_float_literal_suffix(&mut driver, 0, 0, "F"), Ok(Some(FloatLiteralSuffix::F)));

    assert!(make_float_literal_suffix(&mut driver, 0, 0, "FF").is_err());
    assert!(make_float_literal_suffix(&mut driver, 0, 0, "ff").is_err());
    assert!(make_float_literal_suffix(&mut driver, 0, 0, "fF").is_err());
}

#[test]
fn extract_integer_literal_suffix_test() {
    assert_eq!(extract_valid_integer_literal_suffix_chars(String::new()), (String::new(), String::new()));

    assert_eq!(extract_valid_integer_literal_suffix_chars("L".to_string()), (String::new(), "L".to_string()));
    assert_eq!(extract_valid_integer_literal_suffix_chars("Ll".to_string()), (String::new(), "Ll".to_string()));
    assert_eq!(extract_valid_integer_literal_suffix_chars("UuU".to_string()), (String::new(), "UuU".to_string()));

    assert_eq!(
        extract_valid_integer_literal_suffix_chars("123456789".to_string()),
        ("123456789".to_string(), String::new())
    );
    assert_eq!(
        extract_valid_integer_literal_suffix_chars("0xFF112233FF".to_string()),
        ("0xFF112233FF".to_string(), String::new())
    );

    assert_eq!(extract_valid_integer_literal_suffix_chars("123L".to_string()), ("123".to_string(), "L".to_string()));
    assert_eq!(extract_valid_integer_literal_suffix_chars("123lu".to_string()), ("123".to_string(), "lu".to_string()));
    assert_eq!(extract_valid_integer_literal_suffix_chars("123u".to_string()), ("123".to_string(), "u".to_string()));

    assert_eq!(
        extract_valid_integer_literal_suffix_chars("LLU0xFFLLU".to_string()),
        ("LLU0xFF".to_string(), "LLU".to_string())
    );
    assert_eq!(extract_valid_integer_literal_suffix_chars("u123u".to_string()), ("u123".to_string(), "u".to_string()));

    assert_eq!(
        extract_valid_integer_literal_suffix_chars("testLLU".to_string()),
        ("test".to_string(), "LLU".to_string())
    );
    assert_eq!(
        extract_valid_integer_literal_suffix_chars("testllu".to_string()),
        ("test".to_string(), "llu".to_string())
    );
    assert_eq!(
        extract_valid_integer_literal_suffix_chars("testULL".to_string()),
        ("test".to_string(), "ULL".to_string())
    );
    assert_eq!(
        extract_valid_integer_literal_suffix_chars("testull".to_string()),
        ("test".to_string(), "ull".to_string())
    );

    assert_eq!(
        extract_valid_integer_literal_suffix_chars("LuLulUl".to_string()),
        (String::new(), "LuLulUl".to_string())
    );
    assert_eq!(
        extract_valid_integer_literal_suffix_chars("uuuuuUUUU".to_string()),
        (String::new(), "uuuuuUUUU".to_string())
    );
}

#[test]
fn extract_float_literal_suffix_test() {
    assert_eq!(extract_valid_float_literal_suffix_chars(String::new()), (String::new(), String::new()));

    assert_eq!(extract_valid_float_literal_suffix_chars("L".to_string()), (String::new(), "L".to_string()));
    assert_eq!(extract_valid_float_literal_suffix_chars("F".to_string()), (String::new(), "F".to_string()));
    assert_eq!(extract_valid_float_literal_suffix_chars("ff".to_string()), (String::new(), "ff".to_string()));

    assert_eq!(
        extract_valid_float_literal_suffix_chars("testLL".to_string()),
        ("test".to_string(), "LL".to_string())
    );
    assert_eq!(
        extract_valid_float_literal_suffix_chars("testll".to_string()),
        ("test".to_string(), "ll".to_string())
    );
    assert_eq!(
        extract_valid_float_literal_suffix_chars("testFF".to_string()),
        ("test".to_string(), "FF".to_string())
    );
    assert_eq!(
        extract_valid_float_literal_suffix_chars("testff".to_string()),
        ("test".to_string(), "ff".to_string())
    );
}
