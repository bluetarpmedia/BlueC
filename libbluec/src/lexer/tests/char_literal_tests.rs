// Copyright 2025 Neil Henderson, Blue Tarp Media.

use crate::compiler_driver::Driver;
use crate::core::FilePosition;

use super::super::TokenType;
use super::super::line_lexer::LineLexer;

#[test]
fn lex_valid_char_literals() {
    let valid = [
        ("'a'", Some(97)),
        ("'A'", Some(65)),
        ("'0'", Some(48)),
        //
        // Octal
        //
        ("'\\1'", Some(1)),
        ("'\\11'", Some(9)),
        ("'\\111'", Some(73)),
        ("'\\377'", Some(255)),
        //
        // Hex
        //
        ("'\\x0'", Some(0)),
        ("'\\x1'", Some(1)),
        ("'\\xF'", Some(15)),
        ("'\\x0F'", Some(15)),
        ("'\\xA0'", Some(160)),
        ("'\\xFF'", Some(255)),
        ("'\\x0000FF'", Some(255)),
        //
        // Multichar with hex/octal takes the last value
        //
        ("'\\111A'", Some(65)),
        ("'\\11\\22\\33A'", Some(65)),
        ("'\\11\\22\\33'", Some(27)),
        ("'\\1111'", Some(49)),
        ("'\\xFF\\x11'", Some(17)),
        ("'\\xFF\\xFF\\x11'", Some(17)),
        //
        // Escape sequences
        //
        ("'\\a'", Some(7)),
        ("'\\b'", Some(8)),
        ("'\\f'", Some(12)),
        ("'\\n'", Some(10)),
        ("'\\r'", Some(13)),
        ("'\\t'", Some(9)),
        ("'\\v'", Some(11)),
        ("'\\\''", Some(39)), // \'
        ("'\\\"'", Some(34)), // \"
        ("'\\?'", Some(63)),  // \?
        ("'\\\\'", Some(92)), // \\
        ("'\\y'", Some(121)), // Emits a warning about unknown escape code \y but evaluates to 'y'
        //
        // Multichar
        //
        ("'\\a\\a\\a\\a'", Some(117901063)),
        ("'\\t\\a\\a\\a\\a'", Some(117901063)),
        ("'\\aaaa'", Some(123822433)),
        ("'a\\aaa'", Some(1627873633)),
        ("'aa\\aa'", Some(1633748833)),
        ("'aaa\\a'", Some(1633771783)),
        ("'AAAA'", Some(1094795585)),
        ("'BBBBAAAA'", Some(1094795585)),
        ("'CCCCBBBBAAAA'", Some(1094795585)),
        ("'DCBAAAA'", Some(1094795585)),
        ("'xBBBB'", Some(1111638594)),
        ("'xxBBBB'", Some(1111638594)),
        ("'xxxBBBB'", Some(1111638594)),
        ("'xxxxBBBB'", Some(1111638594)),
    ];

    for (lit, expected) in valid {
        expect_char_literal_valid(lit, true, expected);
    }
}

#[test]
fn lex_invalid_char_literals() {
    let invalid = [
        "'\'",      // Empty ''
        "'",        // Single opening '
        "'\\",      // Unclosed '\
        "'a",       // Unclosed 'a
        "'\\'",     // Escape without a following char '\'
        "'\\x'",    // No hex digits after '\x'
        "'\\xFF0'", // Hex value out of range
        "'\\xFFF'", // Hex value out of range
        "'\\477'",  // Octal value out of range
        "'\\777'",  // Octal value out of range
    ];

    for lit in invalid {
        expect_char_literal_valid(lit, false, None);
    }
}

fn expect_char_literal_valid(literal: &str, expected_valid: bool, expected_value: Option<i32>) {
    let mut driver = Driver::for_testing();
    let mut line_lexer = LineLexer::new(&mut driver, FilePosition::default(), literal);
    match line_lexer.get_next_token() {
        Ok(Some(token)) => {
            if let TokenType::CharLiteral { value, .. } = token.token_type {
                assert!(expected_valid, "Expected {literal} to be {expected_valid}");
                if expected_valid {
                    assert_eq!(value, expected_value.unwrap(), "Literal: {literal}");
                }
            } else {
                assert!(!expected_valid, "Expected {literal} to be {expected_valid}");
            }
        }
        _ => assert!(!expected_valid, "Expected {literal} to be {expected_valid}"),
    }
}
