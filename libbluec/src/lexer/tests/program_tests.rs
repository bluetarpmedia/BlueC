// Copyright 2025-2026 Neil Henderson

use std::io::BufReader;
use std::io::Cursor;

use crate::compiler_driver;
use crate::lexer::tests::utils;
use crate::lexer::{Token, TokenType, lex_buf_reader};

#[test]
fn lex_one_line_program() {
    let expected = vec![
        Token::without_location(TokenType::new_identifier("int")),
        Token::without_location(TokenType::new_identifier("main")),
        Token::without_location(TokenType::OpenParen),
        Token::without_location(TokenType::new_identifier("void")),
        Token::without_location(TokenType::CloseParen),
        Token::without_location(TokenType::OpenBrace),
        Token::without_location(TokenType::new_identifier("return")),
        Token::without_location(TokenType::new_int_literal("1")),
        Token::without_location(TokenType::Semicolon),
        Token::without_location(TokenType::CloseBrace),
    ];

    test_lexer("int main(void) { return 1; }", &expected);
}

#[test]
fn lex_program() {
    let expected = vec![
        Token::without_location(TokenType::new_identifier("int")),
        Token::without_location(TokenType::new_identifier("main")),
        Token::without_location(TokenType::OpenParen),
        Token::without_location(TokenType::new_identifier("void")),
        Token::without_location(TokenType::CloseParen),
        Token::without_location(TokenType::OpenBrace),
        Token::without_location(TokenType::new_identifier("return")),
        Token::without_location(TokenType::Minus),
        Token::without_location(TokenType::OpenParen),
        Token::without_location(TokenType::new_int_literal("3")),
        Token::without_location(TokenType::Multiply),
        Token::without_location(TokenType::new_int_literal("4")),
        Token::without_location(TokenType::CloseParen),
        Token::without_location(TokenType::Semicolon),
        Token::without_location(TokenType::CloseBrace),
    ];

    test_lexer("int main(void)\n{\nreturn -(3 * 4);\n}\n", &expected);
}

fn test_lexer(source_code: &str, expected: &Vec<Token>) {
    let mut driver = compiler_driver::Driver::for_testing();
    let cursor = Cursor::new(source_code.as_bytes());
    let mut reader = BufReader::new(cursor);
    let actual = lex_buf_reader(&mut driver, &mut reader);
    utils::compare_token_types(&actual, &expected);
}
