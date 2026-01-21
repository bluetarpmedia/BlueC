// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `utils` module provides helper utility functions to the parent `recursive_descent` module.

use crate::compiler_driver::{Diagnostic, Driver};
use crate::core::{SourceLocation, SymbolKind};
use crate::lexer;

use super::super::recursive_descent::add_error_at_eof;
use super::super::{ParseError, ParseResult, Parser};

/// Takes the next token from the stream, validates its type against the given expected type, and returns its
/// source location.
pub fn expect_token(
    expected_type: lexer::TokenType,
    parser: &mut Parser,
    driver: &mut Driver,
) -> ParseResult<SourceLocation> {
    match parser.token_stream.take_token() {
        Some(token) if token.token_type == expected_type => Ok(token.location),
        Some(token) => {
            let err = format!("Expected `{}` instead of `{}`", expected_type, token.token_type);
            let loc = token.location;
            driver.add_diagnostic(Diagnostic::error_at_location(err, loc));
            Err(ParseError)
        }
        None => {
            add_error_at_eof(parser, driver, format!("Expected `{}`", expected_type));
            Err(ParseError)
        }
    }
}

/// Takes the next token from the stream if it's a semicolon to indicate an end of statement.
pub fn expect_end_of_statement(parser: &mut Parser, driver: &mut Driver, error: &str) -> ParseResult<()> {
    if parser.token_stream.next_token_has_type(lexer::TokenType::Semicolon) {
        _ = parser.token_stream.take_token();
        Ok(())
    } else {
        let loc = get_next_source_location_after_previous_token(parser);
        driver.add_diagnostic(Diagnostic::error_at_location(error.into(), loc));
        Err(ParseError)
    }
}

/// Returns the next token if it's an identifier.
pub fn expect_identifier(parser: &mut Parser, driver: &mut Driver) -> ParseResult<lexer::Token> {
    match parser.token_stream.take_token() {
        Some(token) => {
            if token.is_identifier() {
                Ok(token.clone())
            } else {
                let err = format!("Expected identifier instead of `{}`", token.token_type);
                let loc = token.location;
                driver.add_diagnostic(Diagnostic::error_at_location(err, loc));
                Err(ParseError)
            }
        }
        None => {
            add_error_at_eof(parser, driver, "Expected identifier.");
            Err(ParseError)
        }
    }
}

/// The kind of literal.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum LiteralKind {
    Char,
    String,
    Integer,
    Float,
}

/// Returns the next token if it's a literal of the given type.
pub fn expect_literal_token(
    parser: &mut Parser,
    driver: &mut Driver,
    expected: LiteralKind,
) -> ParseResult<lexer::Token> {
    match parser.token_stream.take_token() {
        Some(token) => {
            if expected == LiteralKind::Char
                && let lexer::TokenType::CharLiteral { .. } = token.token_type
            {
                Ok((*token).clone())
            } else if expected == LiteralKind::String
                && let lexer::TokenType::StringLiteral { .. } = token.token_type
            {
                Ok((*token).clone())
            } else if expected == LiteralKind::Integer
                && let lexer::TokenType::IntegerLiteral { .. } = token.token_type
            {
                Ok((*token).clone())
            } else if expected == LiteralKind::Float
                && let lexer::TokenType::FloatLiteral { .. } = token.token_type
            {
                Ok((*token).clone())
            } else {
                let err = format!("Expected literal instead of `{}`", token.token_type);
                let loc = token.location;
                driver.add_diagnostic(Diagnostic::error_at_location(err, loc));
                Err(ParseError)
            }
        }
        None => {
            add_error_at_eof(parser, driver, "Expected literal.");
            Err(ParseError)
        }
    }
}

/// Advances past the current file scope variable or function declaration (statement or block).
///
/// The stream will be positioned at the beginning of the next file scope declaration.
pub fn move_to_next_file_scope_declaration(parser: &mut Parser) {
    let mut has_block = false;
    let mut brace_count = 0;

    while !parser.token_stream.is_eof() {
        match parser.token_stream.peek_next_token() {
            // If we detect an open brace then we have to consume all tokens until we finish the block.
            Some(token) if token.has_type(lexer::TokenType::OpenBrace) => {
                _ = parser.token_stream.take_token();
                has_block = true;
                brace_count += 1;
            }
            Some(token) if has_block && token.has_type(lexer::TokenType::CloseBrace) => {
                _ = parser.token_stream.take_token();
                brace_count -= 1;

                if brace_count == 0 {
                    break;
                }
            }
            Some(token) if !has_block && token.has_type(lexer::TokenType::Semicolon) => {
                _ = parser.token_stream.take_token();
                break;
            }
            Some(_) => _ = parser.token_stream.take_token(),
            None => break,
        }
    }
}

/// Is the given identifier a type alias visible from the current scope?
pub fn is_type_alias_visible_from_current_scope(parser: &Parser, id: &str) -> bool {
    parser.get_identifier_if_visible_from_current_scope(id).is_some_and(|decl| decl.kind == SymbolKind::TypeAlias)
}

/// Is the token an identifier but not a reserved keyword?
pub fn token_is_identifier_and_not_reserved(token: &lexer::Token) -> bool {
    token.get_identifier().is_some_and(|id| !is_reserved_keyword(id))
}

/// Gets the next source location, of length 1, after the most-recent token that was taken from the stream.
pub fn get_next_source_location_after_previous_token(parser: &Parser) -> SourceLocation {
    let previous_token_loc = match parser.token_stream.prev_token() {
        Some(token) => token.location,
        None => parser.token_stream.peek_next_source_location().expect("Token should exist"),
    };

    previous_token_loc.get_next_location()
}

/// Is the given `id` a reserved keyword in C?
pub fn is_reserved_keyword(id: &str) -> bool {
    match id {
        x if is_builtin_type_specifier(x) => true,
        x if is_type_qualifier(x) => true,
        x if is_storage_class_specifier(x) => true,
        x if is_control_statement_identifier(x) => true,
        _ => false,
    }
}

/// Is the given identifier a built-in type specifier?
pub fn is_builtin_type_specifier(id: &str) -> bool {
    matches!(id, "_Bool" | "char" | "int" | "float" | "double" | "long" | "short" | "signed" | "unsigned" | "void")
}

/// Is the given `id` a type qualifier?
pub fn is_type_qualifier(id: &str) -> bool {
    matches!(id, "const" | "volatile")
}

/// Is the given `id` a storage class specifier?
pub fn is_storage_class_specifier(id: &str) -> bool {
    matches!(id, "static" | "extern" | "typedef") // Future: auto, register, constexpr, thread_local
}

/// Is the given `id` a control statement identifier?
pub fn is_control_statement_identifier(id: &str) -> bool {
    matches!(
        id,
        "if" | "else"
            | "switch"
            | "case"
            | "default"
            | "for"
            | "while"
            | "do"
            | "break"
            | "continue"
            | "goto"
            | "return"
    )
}

/// Evaluates a hex string as a `u64`.
pub fn hex_to_u64(s: &str) -> Result<u64, std::num::ParseIntError> {
    let s = s.strip_prefix("0x").or_else(|| s.strip_prefix("0X")).unwrap_or(s);
    u64::from_str_radix(s, 16)
}

/// Evaluates an octal string as a `u64`.
pub fn oct_to_u64(s: &str) -> Result<u64, std::num::ParseIntError> {
    u64::from_str_radix(s, 8)
}

/// Evaluates a binary string as a `u64`.
pub fn bin_to_u64(s: &str) -> Result<u64, std::num::ParseIntError> {
    let s = s.strip_prefix("0b").or_else(|| s.strip_prefix("0B")).unwrap_or(s);
    u64::from_str_radix(s, 2)
}
