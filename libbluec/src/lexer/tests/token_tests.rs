// Copyright 2025 Neil Henderson, Blue Tarp Media.

use super::utils::test_lexer;
use crate::lexer::{Token, TokenType};

#[test]
fn lex_return_integer() {
    let expected = vec![
        Token::without_location(TokenType::new_identifier("return")),
        Token::without_location(TokenType::new_int_literal("1234")),
        Token::without_location(TokenType::Semicolon),
    ];

    test_lexer("return 1234;", &expected);
}

#[test]
fn lex_return_expression() {
    let expected = vec![
        Token::without_location(TokenType::new_identifier("return")),
        Token::without_location(TokenType::new_int_literal("1")),
        Token::without_location(TokenType::Plus),
        Token::without_location(TokenType::new_int_literal("2")),
        Token::without_location(TokenType::Multiply),
        Token::without_location(TokenType::new_int_literal("3")),
        Token::without_location(TokenType::Divide),
        Token::without_location(TokenType::new_int_literal("4")),
        Token::without_location(TokenType::Semicolon),
    ];

    test_lexer("return 1 + 2 * 3 / 4;", &expected);
}

#[test]
fn has_type() {
    assert!(Token::without_location(TokenType::Plus).has_type(TokenType::Plus));
    assert!(Token::without_location(TokenType::Semicolon).has_type(TokenType::Semicolon));
    assert!(Token::without_location(TokenType::EqualTo).has_type(TokenType::EqualTo));
    assert!(Token::without_location(TokenType::new_identifier("return")).has_type(TokenType::new_identifier("return")));
    assert!(Token::without_location(TokenType::new_int_literal("2")).has_type(TokenType::new_int_literal("2")));
}

#[test]
fn is_identifier() {
    assert_eq!(Token::without_location(TokenType::Plus).is_identifier(), false);
    assert_eq!(Token::without_location(TokenType::new_identifier("test")).is_identifier(), true);

    assert_eq!(Token::without_location(TokenType::Plus).is_identifier_with_name("test"), false);

    assert_eq!(Token::without_location(TokenType::new_identifier("test")).is_identifier_with_name("test"), true);
    assert_eq!(Token::without_location(TokenType::new_identifier("test")).is_identifier_with_name("testing"), false);
    assert!(Token::without_location(TokenType::new_identifier("test")).get_identifier().is_some_and(|id| id == "test"));
}
