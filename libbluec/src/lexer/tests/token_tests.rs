// Copyright 2025 Neil Henderson, Blue Tarp Media.

use crate::compiler_driver;
use crate::lexer::line_lexer::LineLexer;
use crate::lexer::tests::utils;
use crate::lexer::{Token, TokenType};

#[test]
fn lex_return_integer() {
    let expected = vec![
        Token::without_location(TokenType::make_identifier("return")),
        Token::without_location(TokenType::make_int_literal("1234")),
        Token::without_location(TokenType::Semicolon),
    ];

    test_lexer("return 1234;", &expected);
}

#[test]
fn lex_valid_integer_literals() {
    let valid_dec = [
        "0",
        "0u",
        "0U",
        "0l",
        "0lu",
        "0ULL",
        "0llu",
        "123456789",
        "123456789u",
        "123456789ll",
        "123456789LLu",
        "123456789Ull",
    ];
    for lit in valid_dec {
        expect_numeric_literal_valid(lit, true);
    }

    let valid_hex = [
        "0x0",
        "0x0u",
        "0x0U",
        "0x0l",
        "0x0lu",
        "0x0ULL",
        "0x0llu",
        "0xFF",
        "0x123456789abcdef",
        "0xFull",
        "0xFULL",
        "0xFLLu",
    ];
    for lit in valid_hex {
        expect_numeric_literal_valid(lit, true);
    }

    let valid_bin = [
        "0b0",
        "0b0u",
        "0b0U",
        "0b0l",
        "0b0lu",
        "0b0ULL",
        "0b0llu",
        "0b0101010101",
        "0b111U",
        "0b111l",
        "0b111LL",
        "0b111LLU",
    ];
    for lit in valid_bin {
        expect_numeric_literal_valid(lit, true);
    }

    let valid_oct = [
        "00",
        "00u",
        "00U",
        "00l",
        "00lu",
        "00ULL",
        "00llu",
        "01234567",
        "01234567u",
        "01234567ll",
        "01234567LLu",
        "01234567Ull",
    ];
    for lit in valid_oct {
        expect_numeric_literal_valid(lit, true);
    }
}

#[test]
fn lex_invalid_integer_literals() {
    let invalid_dec =
        ["123abc456", "123abc", "abc", "abc123", "123LLLL", "0uu", "0uuu", "1uLl", "1lL", "1LLLLLLLUUUUU"];
    for lit in invalid_dec {
        expect_numeric_literal_valid(lit, false);
    }

    let invalid_hex = [
        "0x",
        "0xU",
        "0xLL",
        "0x0x0x",
        "0xG",
        "0xFG",
        "0xgf",
        "0x12yyyFF",
        "0xFuu",
        "0xFF0x111",
        "0x1LLL",
        "0x0uLl",
        "0x0UlL",
        "0x1.2", // A hex float literal must have exponent 'p'
    ];
    for lit in invalid_hex {
        expect_numeric_literal_valid(lit, false);
    }

    let invalid_bin = [
        "0b",
        "0bU",
        "0bLL",
        "0b2",
        "0b0b0b",
        "0b1010abc10101",
        "0b11110002",
        "0babc",
        "0b1110b111",
        "0b1110xFF",
        "0b111uu",
        "0b111LLL",
        "0b1.0",
    ];
    for lit in invalid_bin {
        expect_numeric_literal_valid(lit, false);
    }

    let invalid_oct = ["012345678", "01234567bad0101", "08", "080xFF", "010b11", "01uu", "01LLL"];
    for lit in invalid_oct {
        expect_numeric_literal_valid(lit, false);
    }
}

#[test]
fn lex_valid_float_literals() {
    let valid_dec = [
        "1.0",
        "0.1",
        "1.",
        ".1",
        "100E10",
        "100e10",
        ".05e-2",
        ".05E-10",
        "5.E+3",
        "5.0e+30",
        "1.2",
        ".1",
        "10e-2",
        "0.0",
        "123.",
        "0.",
        ".0",
        "3.14159",
        "2e10",
        "2E10",
        "2e+10",
        "2e-10",
        "6.022e23",
        "1.0e-6",
        "0e0",
        "00.0",
        "007.00",
        "0001e0",
        "123.456e78",
        ".25e2",
        "5.e3",
        "5.E-3",
        "1e0",
        "1E-0",
        "9.99e+99",
        "0.0001",
        ".0001",
        "10.0f",
        "10.0F",
        ".5f",
        // Future: long double constants "10.0L"
    ];
    for lit in valid_dec {
        expect_numeric_literal_valid(lit, true);
    }

    let valid_hex = [
        "0x1.8p-1",
        "0xFFp+2",
        "0x1p0",
        "0x1p+0",
        "0x1p-0",
        "0x0.1p4",
        "0x0.0001p+16",
        "0x1.0p10",
        "0x1.921fb54442d18p+0",
        "0x1.921FB54442D18P+0",
        "0x1.fffffffffffffp+1023",
        "0x1.0p-1022",
        "0x1.0p-1074",
        "0x1.2p3",
        "0xA.BCp-4",
        "0xAbCd.1234p+5",
        "0x0p0",
        "0x0p+0",
        "0x0p-0",
        "0x10p+1",
        "0x10.0p+1",
        "0xDEAD.BEEFp-8",
        "0xdead.beefP8",
        "0xF.Fp+7",
        "0x1.0000000000001p-1",
        "0x1.0000000000000p-1",
        "0x3.fp+2",
        "0x3.P+2",
        "0x.8p+1",
        "0x.8P+1",
        "0xABCp-123",
        "0xabc.p123",
        "0x1p+2147483647",
        "0x1p-2147483648",
        "0x1.1p1",
        "0x1.2p+03",    // exponent with leading zero
        "0x1.2p-003",   // negative exponent with leading zeros
        "0x1.2P3",      // uppercase exponent marker
        "0x1.2p+3",     // normal form
        "0x1p+1",       // integer mantissa only
        "0x1.p+1",      // trailing dot allowed
        "0x1.0p-1",
        "0xF.FFFFFFp-127",
        "0x1.fffffffep+127"
    ];
    for lit in valid_hex {
        expect_numeric_literal_valid(lit, true);
    }
}

#[test]
fn lex_invalid_float_literals() {
    let invalid_dec = [
        "1.d0",
        "0.d1",
        "0.d1f",
        "0.d1F",
        "1.d",
        ".d1",
        "100E10E20",
        "100e10e20",
        ".05e-+2",
        ".05E-+10",
        "5.Ef+3",
        "5.0fe+30",
        "10e",
        "10e-",
        "10e+",
        "e",
        "E",
        "1.2ff",
        "1.2FF",
        "10e-1ff",
        "10e+1FF",
        "10.fe-2",
        "1.e-10x",
        "2._",
        "2e1_",
        "10ee1",
        "10EE-2",
    ];
    for lit in invalid_dec {
        expect_numeric_literal_valid(lit, false);
    }
}

#[test]
fn verify_integer_literals() {
    test_lexer("98765", &vec![Token::without_location(TokenType::make_int_literal("98765"))]);
    test_lexer("98765l", &vec![Token::without_location(TokenType::make_long_int_literal("98765"))]);
    test_lexer("98765L", &vec![Token::without_location(TokenType::make_long_int_literal("98765"))]);
    test_lexer("98765ll", &vec![Token::without_location(TokenType::make_long_long_int_literal("98765"))]);
    test_lexer("98765LL", &vec![Token::without_location(TokenType::make_long_long_int_literal("98765"))]);
    test_lexer("123456789L", &vec![Token::without_location(TokenType::make_long_int_literal("123456789"))]);
    test_lexer("123456789LL", &vec![Token::without_location(TokenType::make_long_long_int_literal("123456789"))]);

    test_lexer(
        "-567000",
        &vec![
            Token::without_location(TokenType::Minus),
            Token::without_location(TokenType::make_int_literal("567000")),
        ],
    );

    test_lexer(
        "-567000L",
        &vec![
            Token::without_location(TokenType::Minus),
            Token::without_location(TokenType::make_long_int_literal("567000")),
        ],
    );
}

#[test]
fn lex_return_expression() {
    let expected = vec![
        Token::without_location(TokenType::make_identifier("return")),
        Token::without_location(TokenType::make_int_literal("1")),
        Token::without_location(TokenType::Plus),
        Token::without_location(TokenType::make_int_literal("2")),
        Token::without_location(TokenType::Multiply),
        Token::without_location(TokenType::make_int_literal("3")),
        Token::without_location(TokenType::Divide),
        Token::without_location(TokenType::make_int_literal("4")),
        Token::without_location(TokenType::Semicolon),
    ];

    test_lexer("return 1 + 2 * 3 / 4;", &expected);
}

#[test]
fn has_type() {
    assert!(Token::without_location(TokenType::Plus).has_type(TokenType::Plus));
    assert!(Token::without_location(TokenType::Semicolon).has_type(TokenType::Semicolon));
    assert!(Token::without_location(TokenType::EqualTo).has_type(TokenType::EqualTo));
    assert!(
        Token::without_location(TokenType::make_identifier("return")).has_type(TokenType::make_identifier("return"))
    );
    assert!(Token::without_location(TokenType::make_int_literal("2")).has_type(TokenType::make_int_literal("2")));
}

fn expect_numeric_literal_valid(literal: &str, expected_valid: bool) {
    let mut driver = compiler_driver::Driver::for_testing();
    let mut line_lexer = LineLexer::new(&mut driver, 1, literal);
    match line_lexer.get_next_token() {
        Ok(Some(token)) => {
            if matches!(token.token_type, TokenType::IntegerLiteral { .. } | TokenType::FloatLiteral { .. }) {
                assert!(expected_valid, "Expected {literal} to be {expected_valid}");
            } else {
                assert!(!expected_valid, "Expected {literal} to be {expected_valid}");
            }
        }
        _ => assert!(!expected_valid, "Expected {literal} to be {expected_valid}"),
    }
}

fn test_lexer(source_code: &str, expected: &Vec<Token>) {
    let mut driver = compiler_driver::Driver::for_testing();
    let mut line_lexer = LineLexer::new(&mut driver, 1, source_code);
    let mut actual = Vec::new();
    loop {
        match line_lexer.get_next_token() {
            Ok(Some(token)) => {
                actual.push(token);
            }
            Ok(None) => break, // No more tokens in this line
            Err(_) => {
                eprintln!("LineLexer failed.");
                driver.debug_print_diagnostics();
                std::process::exit(1);
            }
        }
    }
    utils::compare_token_types(&actual, &expected);
}
