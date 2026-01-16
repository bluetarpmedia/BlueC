// Copyright 2025 Neil Henderson, Blue Tarp Media.

use crate::lexer;
use crate::lexer::{Token, TokenType};
use crate::parser::token_stream::TokenStream;

#[test]
fn peek_and_take() {
    let tokens = vec![
        Token::without_location(TokenType::Minus),
        Token::without_location(TokenType::OpenParen),
        Token::without_location(TokenType::BitwiseNot),
        Token::without_location(TokenType::new_int_literal("321")),
        Token::without_location(TokenType::CloseParen),
    ];

    let mut stream = TokenStream::new(tokens);
    assert!(!stream.is_eof());

    assert!(stream.prev_token().is_none());

    verify_token(stream.last_token(), TokenType::CloseParen);

    assert!(stream.next_token_has_type(TokenType::Minus));
    assert!(!stream.next_token_has_type(TokenType::OpenParen));

    let (first, second) = stream.peek_next_2_tokens();
    verify_token(first, TokenType::Minus);
    verify_token(second, TokenType::OpenParen);

    verify_token(stream.peek_next_token(), TokenType::Minus);
    verify_token(stream.take_token(), TokenType::Minus);

    let prev = stream.prev_token();
    verify_token(prev, TokenType::Minus);

    assert!(stream.next_token_has_type(TokenType::OpenParen));

    let (first, second) = stream.peek_next_2_tokens();
    verify_token(first, TokenType::OpenParen);
    verify_token(second, TokenType::BitwiseNot);

    verify_token(stream.peek_next_token(), TokenType::OpenParen);

    let prev = stream.prev_token();
    verify_token(prev, TokenType::Minus);

    verify_token(stream.take_token(), TokenType::OpenParen);

    let (first, second) = stream.peek_next_2_tokens();
    verify_token(first, TokenType::BitwiseNot);
    verify_token(second, TokenType::new_int_literal("321"));

    verify_token(stream.peek_next_token(), TokenType::BitwiseNot);
    verify_token(stream.take_token(), TokenType::BitwiseNot);

    let (first, second) = stream.peek_next_2_tokens();
    verify_token(first, TokenType::new_int_literal("321"));
    verify_token(second, TokenType::CloseParen);

    verify_token(stream.peek_next_token(), TokenType::new_int_literal("321"));
    verify_token(stream.take_token(), TokenType::new_int_literal("321"));

    let (first, second) = stream.peek_next_2_tokens();
    verify_token(first, TokenType::CloseParen);
    assert!(second.is_none());

    verify_token(stream.peek_next_token(), TokenType::CloseParen);
    verify_token(stream.take_token(), TokenType::CloseParen);

    assert!(stream.peek_next_token().is_none());
    assert!(stream.take_token().is_none());
    assert!(stream.is_eof());

    let (first, second) = stream.peek_next_2_tokens();
    assert!(first.is_none());
    assert!(second.is_none());

    assert!(!stream.next_token_has_type(TokenType::CloseParen));

    verify_token(stream.last_token(), TokenType::CloseParen);

    verify_token(stream.prev_token(), TokenType::CloseParen);
}


#[test]
fn peek_next_2_tokens() {
    {
        let no_tokens = Vec::new();

        let stream = TokenStream::new(no_tokens);
        let (first, second) = stream.peek_next_2_tokens();
        assert!(first.is_none());
        assert!(second.is_none());
    }

    {
        let one_token = vec![
            Token::without_location(TokenType::new_int_literal("321")),
        ];

        let stream = TokenStream::new(one_token);
        let (first, second) = stream.peek_next_2_tokens();
        verify_token(first, TokenType::new_int_literal("321"));
        assert!(second.is_none());
    }

    {
        let two_tokens = vec![
            Token::without_location(TokenType::Minus),
            Token::without_location(TokenType::Plus),
        ];

        let stream = TokenStream::new(two_tokens);
        let (first, second) = stream.peek_next_2_tokens();
        verify_token(first, TokenType::Minus);
        verify_token(second, TokenType::Plus);
    }

    {
        let eof_tokens = vec![
            Token::without_location(TokenType::Minus),
            Token::without_location(TokenType::Plus),
            Token::without_location(TokenType::Minus),
            Token::without_location(TokenType::OpenParen),
            Token::without_location(TokenType::BitwiseNot),
        ];

        let mut stream = TokenStream::new(eof_tokens);
        _ = stream.take_token();
        _ = stream.take_token();
        _ = stream.take_token();

        let (first, second) = stream.peek_next_2_tokens();
        verify_token(first, TokenType::OpenParen);
        verify_token(second, TokenType::BitwiseNot);

        _ = stream.take_token();

        let (first, second) = stream.peek_next_2_tokens();
        verify_token(first, TokenType::BitwiseNot);
        assert!(second.is_none());

        _ = stream.take_token();
        assert!(stream.is_eof());

        let (first, second) = stream.peek_next_2_tokens();
        assert!(first.is_none());
        assert!(second.is_none());
    }
}

#[test]
fn peek_and_prev_source_locations() {
    let tokens = vec![
        Token::with_location(TokenType::Minus, 1, 1),
        Token::with_location(TokenType::OpenParen, 1, 2),
        Token::with_location(TokenType::BitwiseNot, 1, 3),
        Token::with_location(TokenType::new_int_literal("321"), 1, 4),
        Token::with_location(TokenType::CloseParen, 1, 7),
    ];

    let mut stream = TokenStream::new(tokens);

    let loc = stream.peek_next_source_location().expect("Should have location");
    assert!(loc.line == 1 && loc.column == 1);

    stream.take_token();
    let loc = stream.peek_next_source_location().expect("Should have location");
    assert!(loc.line == 1 && loc.column == 2);

    let prev_loc = stream.prev_token_source_location().expect("Should have location");
    assert!(prev_loc.line == 1 && prev_loc.column == 1);

    stream.take_token();
    let loc = stream.peek_next_source_location().expect("Should have location");
    assert!(loc.line == 1 && loc.column == 3);

    let prev_loc = stream.prev_token_source_location().expect("Should have location");
    assert!(prev_loc.line == 1 && prev_loc.column == 2);

    stream.take_token();
    let loc = stream.peek_next_source_location().expect("Should have location");
    assert!(loc.line == 1 && loc.column == 4);

    let prev_loc = stream.prev_token_source_location().expect("Should have location");
    assert!(prev_loc.line == 1 && prev_loc.column == 3);

    stream.take_token();
    let loc = stream.peek_next_source_location().expect("Should have location");
    assert!(loc.line == 1 && loc.column == 7);

    let prev_loc = stream.prev_token_source_location().expect("Should have location");
    assert!(prev_loc.line == 1 && prev_loc.column == 4);
}

#[test]
fn take_if_expected() {
    let tokens = vec![
        Token::without_location(TokenType::Minus),
        Token::without_location(TokenType::OpenParen),
        Token::without_location(TokenType::BitwiseNot),
        Token::without_location(TokenType::new_int_literal("321")),
        Token::without_location(TokenType::CloseParen),
    ];

    let mut stream = TokenStream::new(tokens);

    assert!(stream.take_token_if_expected(TokenType::AdditionAssignment).is_none());
    assert!(stream.take_token_if_expected(TokenType::CloseParen).is_none());

    let token = stream.take_token_if_expected(TokenType::Minus);
    verify_token(token, TokenType::Minus);

    let token = stream.take_token_if_expected(TokenType::OpenParen);
    verify_token(token, TokenType::OpenParen);

    let token = stream.take_token_if_expected(TokenType::BitwiseNot);
    verify_token(token, TokenType::BitwiseNot);

    let token = stream.take_token_if_expected(TokenType::new_int_literal("321"));
    verify_token(token, TokenType::new_int_literal("321"));

    let token = stream.take_token_if_expected(TokenType::CloseParen);
    verify_token(token, TokenType::CloseParen);

    let token = stream.take_token_if_expected(TokenType::CloseParen);
    assert!(token.is_none());
}

#[test]
fn move_to_next_statement() {
    let tokens = vec![
        Token::without_location(TokenType::new_identifier("int")),
        Token::without_location(TokenType::new_identifier("a")),
        Token::without_location(TokenType::Assignment),
        Token::without_location(TokenType::new_int_literal("1")),
        Token::without_location(TokenType::Semicolon),
        //
        Token::without_location(TokenType::new_identifier("float")),
        Token::without_location(TokenType::new_identifier("b")),
        Token::without_location(TokenType::Assignment),
        Token::without_location(TokenType::new_int_literal("2")),
        Token::without_location(TokenType::Semicolon),
        //
        Token::without_location(TokenType::new_identifier("int")),
        Token::without_location(TokenType::new_identifier("c")),
        Token::without_location(TokenType::Assignment),
        Token::without_location(TokenType::new_int_literal("3")),
        Token::without_location(TokenType::Semicolon),
        //
        Token::without_location(TokenType::OpenBrace),
        Token::without_location(TokenType::new_identifier("int")),
        Token::without_location(TokenType::new_identifier("d")),
        Token::without_location(TokenType::CloseBrace),
    ];

    let mut stream = TokenStream::new(tokens);

    verify_token(stream.take_token(), TokenType::new_identifier("int"));
    verify_token(stream.take_token(), TokenType::new_identifier("a"));

    stream.move_to_next_statement();
    verify_token(stream.take_token(), TokenType::new_identifier("float"));
    verify_token(stream.take_token(), TokenType::new_identifier("b"));

    stream.move_to_next_statement();
    verify_token(stream.take_token(), TokenType::new_identifier("int"));
    verify_token(stream.take_token(), TokenType::new_identifier("c"));

    stream.move_to_next_statement();
    verify_token(stream.take_token(), TokenType::OpenBrace);
    verify_token(stream.take_token(), TokenType::new_identifier("int"));

    stream.move_to_next_statement();
    verify_token(stream.take_token(), TokenType::CloseBrace);
}

#[test]
fn snapshot_and_restore() {
    let tokens = vec![
        Token::without_location(TokenType::new_identifier("int")),
        Token::without_location(TokenType::new_identifier("a")),
        Token::without_location(TokenType::Assignment),
        Token::without_location(TokenType::new_int_literal("1")),
        Token::without_location(TokenType::Semicolon),
        //
        Token::without_location(TokenType::new_identifier("float")),
        Token::without_location(TokenType::new_identifier("b")),
        Token::without_location(TokenType::Assignment),
        Token::without_location(TokenType::new_int_literal("2")),
        Token::without_location(TokenType::Semicolon),
        //
        Token::without_location(TokenType::new_identifier("long")),
        Token::without_location(TokenType::new_identifier("c")),
        Token::without_location(TokenType::Assignment),
        Token::without_location(TokenType::new_int_literal("3")),
        Token::without_location(TokenType::Semicolon),
    ];

    let mut stream = TokenStream::new(tokens);
    let snapshot1 = stream.snapshot();

    stream.move_to_next_statement();
    let snapshot2 = stream.snapshot();

    stream.move_to_next_statement();
    let snapshot3 = stream.snapshot();

    stream.move_to_next_statement();
    let snapshot4 = stream.snapshot();
    assert!(stream.is_eof());

    stream.restore(snapshot1);
    verify_token(stream.take_token(), TokenType::new_identifier("int"));

    stream.restore(snapshot2);
    verify_token(stream.take_token(), TokenType::new_identifier("float"));

    stream.restore(snapshot3);
    verify_token(stream.take_token(), TokenType::new_identifier("long"));

    stream.restore(snapshot4);
    assert!(stream.is_eof());

    stream.restore(snapshot1);
    verify_token(stream.take_token(), TokenType::new_identifier("int"));
}

fn verify_token(token: Option<&lexer::Token>, expected_type: TokenType) {
    match token {
        Some(t) => assert!(t.token_type == expected_type, "{} != {}", t.token_type, expected_type),
        _ => assert!(false),
    }
}
