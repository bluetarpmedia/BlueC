// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `literal` module defines the various parsing functions for integer and floating-point literals.

use super::hex_float;
use super::utils;
use super::{AstExpression, AstFloatLiteralKind, AstIntegerLiteralKind, AstNodeId};
use super::{ParseError, ParseResult, Parser, add_error};

use crate::ICE;
use crate::compiler_driver::{Driver, warnings::Warning};
use crate::lexer;
use crate::lexer::SourceLocation;
use crate::parser::meta;

/// Parses a character literal.
pub fn parse_char_literal(parser: &mut Parser, driver: &mut Driver) -> ParseResult<AstExpression> {
    let token = utils::expect_literal_token(parser, driver, utils::LiteralKind::Char)?;

    let lexer::TokenType::CharLiteral { literal, value } = token.token_type else {
        ICE!("Expected character literal");
    };

    let node_id = AstNodeId::new();
    parser.metadata.add_source_span(node_id, meta::AstNodeSourceSpan::from_source_location(&token.location));

    Ok(AstExpression::CharLiteral { node_id, literal, value })
}

/// Parses a string literal.
///
/// Adjacent string literal tokens are concatenated into one expression.
///
/// ```c
/// char *str = "this" "is" "joined";   --->   "thisisjoined"
/// ```
pub fn parse_string_literal(parser: &mut Parser, driver: &mut Driver) -> ParseResult<AstExpression> {
    let is_string_literal =
        |tok: &lexer::Token| -> bool { matches!(tok.token_type, lexer::TokenType::StringLiteral { .. }) };

    let mut literals = Vec::new();
    let mut ascii = Vec::new();
    let mut start_loc = None;

    // Take all of the adjacent string literal tokens and concatenate them
    //
    while parser.token_stream.peek_next_token().is_some_and(is_string_literal) {
        let token = utils::expect_literal_token(parser, driver, utils::LiteralKind::String)?;

        let lexer::TokenType::StringLiteral { literal: this_lit, ascii: mut this_ascii } = token.token_type else {
            ICE!("Expected string literal");
        };

        literals.push(this_lit);

        // Concatenate the evaluated strings
        if ascii.is_empty() {
            ascii = this_ascii;
        } else {
            ascii.append(&mut this_ascii);
        }

        if start_loc.is_none() {
            start_loc = Some(token.location);
        }
    }

    let start_loc = start_loc.ok_or(ParseError)?;
    let end_loc = parser.token_stream.prev_token_source_location().ok_or(ParseError)?;

    let node_id = AstNodeId::new();
    parser.metadata.add_source_span(node_id, meta::AstNodeSourceSpan::from_source_location_pair(&start_loc, &end_loc));

    Ok(AstExpression::StringLiteral { node_id, literals, ascii })
}

/// Parses an integer literal.
pub fn parse_integer_literal(parser: &mut Parser, driver: &mut Driver) -> ParseResult<AstExpression> {
    let token = utils::expect_literal_token(parser, driver, utils::LiteralKind::Integer)?;

    let lexer::TokenType::IntegerLiteral { literal, base, suffix } = token.token_type else {
        ICE!("Expected integer literal");
    };

    // Convert the literal string into a u64.
    let value = match base {
        lexer::NumericLiteralBase::Decimal => {
            // We need to be able to handle the edge case of parsing -2147483648 which is interpreted as a
            // unary negate followed by the literal 2147483648. But 2147483648 is too big for an i32.
            literal.parse::<u64>().map_err(|_parse_int_err| {
                add_error(driver, "Integer literal is too large to be represented in any integer type", token.location);
            })
        }
        lexer::NumericLiteralBase::Hex => utils::hex_to_u64(&literal).map_err(|_parse_int_err| {
            add_error(driver, "Integer literal is too large to be represented in any integer type", token.location);
        }),
        lexer::NumericLiteralBase::Octal => utils::oct_to_u64(&literal).map_err(|_parse_int_err| {
            add_error(driver, "Integer literal is too large to be represented in any integer type", token.location);
        }),
        lexer::NumericLiteralBase::Binary => utils::bin_to_u64(&literal).map_err(|_parse_int_err| {
            add_error(driver, "Integer literal is too large to be represented in any integer type", token.location);
        }),
    }
    .map_err(|_| ParseError)?;

    let kind = match suffix {
        Some(suffix) => match suffix {
            lexer::IntegerLiteralSuffix::L => AstIntegerLiteralKind::Long,
            lexer::IntegerLiteralSuffix::LL => AstIntegerLiteralKind::LongLong,
            lexer::IntegerLiteralSuffix::U => AstIntegerLiteralKind::UnsignedInt,
            lexer::IntegerLiteralSuffix::UL => AstIntegerLiteralKind::UnsignedLong,
            lexer::IntegerLiteralSuffix::ULL => AstIntegerLiteralKind::UnsignedLongLong,
        },
        None => AstIntegerLiteralKind::Int,
    };

    let mut warn_too_large_for_signed = || {
        Warning::integer_literal_implicitly_unsigned(token.location, driver);
    };

    // Promotions
    //      'int' is promoted to 'long' or 'unsigned long' if it's too large.
    //      'unsigned int' is promoted to 'unsigned long' if it's too large.
    //      'long' is promoted to 'unsigned long' if it's too large.
    //      If a signed integer literal is promoted to unsigned then a warning is emitted.
    //
    let kind = match kind {
        AstIntegerLiteralKind::Int if value > i32::MAX as u64 => {
            if value > i64::MAX as u64 {
                warn_too_large_for_signed();
                AstIntegerLiteralKind::UnsignedLong
            } else {
                AstIntegerLiteralKind::Long
            }
        }

        AstIntegerLiteralKind::UnsignedInt if value > u32::MAX as u64 => AstIntegerLiteralKind::UnsignedLong,

        AstIntegerLiteralKind::Long if value > i64::MAX as u64 => {
            warn_too_large_for_signed();
            AstIntegerLiteralKind::UnsignedLong
        }

        AstIntegerLiteralKind::LongLong if value > i64::MAX as u64 => {
            warn_too_large_for_signed();
            AstIntegerLiteralKind::UnsignedLongLong
        }

        _ => kind,
    };

    let node_id = AstNodeId::new();
    parser.metadata.add_source_span(node_id, meta::AstNodeSourceSpan::from_source_location(&token.location));

    Ok(AstExpression::IntegerLiteral { node_id, literal, literal_base: base.as_int(), value, kind })
}

/// Parses a floating-point literal.
pub fn parse_float_literal(parser: &mut Parser, driver: &mut Driver) -> ParseResult<AstExpression> {
    let token = utils::expect_literal_token(parser, driver, utils::LiteralKind::Float)?;

    let lexer::TokenType::FloatLiteral { literal, base, suffix } = token.token_type else {
        ICE!("Expected floating-point literal");
    };

    // Convert the literal string into an f64.
    let value = match base {
        lexer::NumericLiteralBase::Decimal => literal.parse::<f64>().map_err(|_parse_float_err| {
            add_error(driver, "Cannot parse floating-point literal", token.location);
            ParseError
        }),
        lexer::NumericLiteralBase::Hex => match suffix {
            Some(suffix) => match suffix {
                lexer::FloatLiteralSuffix::F => {
                    parse_hex_float_literal_as_f32(&literal, token.location, driver).map(|value| value as f64)
                }
                lexer::FloatLiteralSuffix::L => parse_hex_float_literal_as_f64(&literal, token.location, driver),
            },
            None => parse_hex_float_literal_as_f64(&literal, token.location, driver),
        },
        _ => ICE!("Invalid base for float literal"),
    }?;

    let kind = match suffix {
        Some(suffix) => match suffix {
            lexer::FloatLiteralSuffix::F => AstFloatLiteralKind::Float,
            lexer::FloatLiteralSuffix::L => AstFloatLiteralKind::LongDouble,
        },
        None => AstFloatLiteralKind::Double,
    };

    let node_id = AstNodeId::new();
    parser.metadata.add_source_span(node_id, meta::AstNodeSourceSpan::from_source_location(&token.location));

    Ok(AstExpression::FloatLiteral { node_id, literal, literal_base: base.as_int(), value, kind })
}

#[cfg(feature = "hex-float-literal")]
fn parse_hex_float_literal_as_f64(literal: &str, literal_loc: SourceLocation, driver: &mut Driver) -> ParseResult<f64> {
    hex_float::parse_as_f64(literal)
        .map_err(|parse_hex_float_err| match parse_hex_float_err {
            hex_float::ParseHexFloatErr::OutOfRange => {
                add_error(driver, "Floating-point constant is too large for type 'double'", literal_loc)
            }
            _ => add_error(driver, "Cannot parse floating-point literal", literal_loc),
        })
        .map_err(|_| ParseError)
}

#[cfg(feature = "hex-float-literal")]
fn parse_hex_float_literal_as_f32(literal: &str, literal_loc: SourceLocation, driver: &mut Driver) -> ParseResult<f32> {
    hex_float::parse_as_f32(literal)
        .map_err(|parse_hex_float_err| match parse_hex_float_err {
            hex_float::ParseHexFloatErr::OutOfRange => {
                add_error(driver, "Floating-point constant is too large for type 'float'", literal_loc)
            }
            _ => add_error(driver, "Cannot parse floating-point literal", literal_loc),
        })
        .map_err(|_| ParseError)
}

#[cfg(not(feature = "hex-float-literal"))]
fn parse_hex_float_literal_as_f64(
    _literal: &str,
    literal_loc: SourceLocation,
    driver: &mut Driver,
) -> ParseResult<f64> {
    add_error(
        driver,
        "Hexadecimal floating-point literals are not supported in this build. \
         To enable support, rebuild BlueC with the 'hex-float-literal' feature enabled.",
        literal_loc,
    );
    Err(ParseError)
}

#[cfg(not(feature = "hex-float-literal"))]
fn parse_hex_float_literal_as_f32(
    _literal: &str,
    literal_loc: SourceLocation,
    driver: &mut Driver,
) -> ParseResult<f32> {
    add_error(
        driver,
        "Hexadecimal floating-point literals are not supported in this build. \
         To enable support, rebuild BlueC with the 'hex-float-literal' feature enabled.",
        literal_loc,
    );
    Err(ParseError)
}
