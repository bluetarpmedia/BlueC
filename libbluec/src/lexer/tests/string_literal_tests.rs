// Copyright 2025 Neil Henderson, Blue Tarp Media.

use crate::compiler_driver;
use crate::lexer::TokenType;
use crate::lexer::line_lexer::LineLexer;

#[test]
fn lex_valid_string_literals() {
    let valid = [
        (r#""""#, Some(r#""#)),
        (r#""test!""#, Some(r#"test!"#)),
        (r#""1 2 3 4 5""#, Some(r#"1 2 3 4 5"#)),
        (r#""\xFF""#, Some(r#"\377"#)),
        (r#""\xFF""#, Some(r#"\377"#)),
        (r#""\y""#, Some(r#"y"#)),
        (r#""Hello, \"world\"\nThis is a test\t\r\n.""#, Some(r#"Hello, \"world\"\nThis is a test\t\r\n."#)),
        (r#""\xFF""#, Some(r#"\377"#)),
        (r#""\x41\x42""#, Some(r#"AB"#)),
        (r#""\012""#, Some(r#"\n"#)),
        (r#""\0abc""#, Some(r#"\000abc"#)),
        (r#""\1234""#, Some(r#"S4"#)),
        (r#""\a\b\v""#, Some(r#"\007\b\013"#)),
        (r#""\\ \" \'""#, Some(r#"\\ \" '"#)),
    ];

    for (lit, expected) in valid {
        expect_string_literal_valid(lit, true, expected);
    }
}

#[test]
fn lex_invalid_string_literals() {
    let invalid = [
        r#"""#,        // Opening " without closing literal
        r#""\x""#,     // "\x" without a subsequent hex digit
        r#""\xFFFF""#, // Hex value out of range
        r#""\777""#,   // Octal value out of range
    ];

    for lit in invalid {
        expect_string_literal_valid(lit, false, None);
    }
}

fn expect_string_literal_valid(literal: &str, expected_valid: bool, expected_eval: Option<&str>) {
    let mut driver = compiler_driver::Driver::for_testing();
    let mut line_lexer = LineLexer::new(&mut driver, 1, literal);
    match line_lexer.get_next_token() {
        Ok(Some(token)) => {
            if let TokenType::StringLiteral { ascii, .. } = token.token_type {
                assert!(expected_valid, "Expected {literal} to be {expected_valid}");
                if expected_valid {
                    let ascii_joined = ascii.join("");
                    assert_eq!(ascii_joined, expected_eval.unwrap(), "Literal: {literal}");
                }
            } else {
                assert!(!expected_valid, "Expected {literal} to be {expected_valid}");
            }
        }
        _ => assert!(!expected_valid, "Expected {literal} to be {expected_valid}"),
    }
}
