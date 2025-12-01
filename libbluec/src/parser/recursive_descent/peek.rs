// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `peek` module provides "peek ahead" parsing utility functions to the parent `recursive_descent` module.
//! These functions look ahead at the token stream and try to parse the tokens in a certain way without consuming the
//! tokens or affecting the stream.

use super::super::expr;
use super::super::symbol::SymbolKind;
use super::super::{ParseError, Parser};
use super::decl;
use super::declarator;
use super::utils;

use crate::compiler_driver::Driver;
use crate::lexer;

/// Peeks ahead and returns whether the next tokens are a variable, function, or type alias declaration.
///
/// The token stream is restored to its original position after this function returns, and no diagnostics
/// are emitted while attempting to detect the declaration.
pub fn is_declaration(parser: &mut Parser, driver: &mut Driver) -> bool {
    parser.disable_diagnostics_during(driver, |parser, _driver| {
        parser
            .restore_token_stream_after(|parser| {
                if let Some(peek_next_token) = parser.token_stream.peek_next_token() {
                    if let lexer::TokenType::Identifier(ref id) = peek_next_token.token_type {
                        //
                        // If the token is a builtin type specifier, storage class specifier, or the "typedef" keyword
                        // then this must be a declaration. (This also handles the case of "typedef" appearing after
                        // the type.)
                        //
                        if utils::is_builtin_type_specifier(id)
                            || utils::is_storage_class_specifier(id)
                            || id == "typedef"
                        {
                            Ok(())
                        }
                        //
                        // If the token is an identifier for an existing variable or function declaration then it
                        // cannot be a new declaration.
                        //
                        else if parser.get_identifier_if_visible_from_current_scope(id).is_some_and(|decl| {
                            decl.kind == SymbolKind::Variable || decl.kind == SymbolKind::Function
                        }) {
                            Err(())
                        }
                        //
                        // If the token is a type alias then (having ruled out an identifier above) it must be
                        // a declarataion.
                        //
                        else if utils::is_type_alias_visible_from_current_scope(parser, id) {
                            Ok(())
                        }
                        //
                        // Otherwise, it cannot be a declaration
                        //
                        else {
                            Err(())
                        }
                    } else {
                        Err(())
                    }
                } else {
                    Err(())
                }
            })
            .is_ok()
    })
}

/// Peeks ahead and returns whether the next tokens are type and/or storage class specifiers.
///
/// E.g. returns true if the next tokens have a form like the following:
/// ```c
/// signed long int
/// extern float
/// static long double
/// MyInt  // Assuming typedef prior and is visible in current scope
/// ```
///
/// The token stream is restored to its original position after this function returns, and no diagnostics
/// are emitted while attempting to detect the declaration.
pub fn is_type_and_storage_specifiers(parser: &mut Parser, driver: &mut Driver) -> bool {
    parser
        .disable_diagnostics_during(driver, |parser, driver| {
            parser.restore_token_stream_after(|parser| -> Result<(), ParseError> {
                // Parse the type specifiers
                //
                const EXPECTS_IDENTIFIER: bool = false;
                _ = decl::parse_type_and_storage_specifiers(parser, driver, EXPECTS_IDENTIFIER)?;

                Ok(())
            })
        })
        .is_ok()
}

/// Peeks ahead and returns whether the next tokens are a type cast operator.
///
/// E.g. returns true if the next tokens have a form like the following:
/// ```c
/// (signed long int)
/// ```
///
/// The token stream is restored to its original position after this function returns, and no diagnostics
/// are emitted while attempting to detect the declaration.
pub fn is_type_cast_operator(parser: &mut Parser, driver: &mut Driver) -> bool {
    parser
        .disable_diagnostics_during(driver, |parser, driver| {
            parser.restore_token_stream_after(|parser| -> Result<(), ParseError> {
                // Expect open parenthesis
                _ = utils::expect_token(lexer::TokenType::OpenParen, parser, driver)?;

                // Parse the type specifiers
                //      We'll still treat it as a cast if there's a storage class specifier too, because
                //      the parse function will emit a diagnostic in that case.
                //
                const EXPECTS_IDENTIFIER: bool = false;
                _ = decl::parse_type_and_storage_specifiers(parser, driver, EXPECTS_IDENTIFIER)?;

                // Parse the optional abstract declarator.
                //      Ignore errors if we can't parse it correctly, because the type specifiers already
                //      indicates this is a cast. We just want to confirm by consuming the tokens and confirming
                //      the cast ends in a close parenthesis.
                //
                _ = declarator::parse_declarator(parser, driver, declarator::ParseOption::RequireAbstract);

                // Expect close parenthesis
                _ = utils::expect_token(lexer::TokenType::CloseParen, parser, driver)?;

                Ok(())
            })
        })
        .is_ok()
}

/// Peeks ahead and returns whether the parser can successfully parse an expression.
///
/// The token stream is restored to its original position after the function returns. In other words, you
/// must still parse the expression if the function returns true. This function is helpful for detecting
/// error cases, like if an expression is expected.
///
/// This function does not generate any diagnostics while attempting to parse the expression. It only
/// returns a boolean value indicating whether a valid expression is the next parsed sequence of tokens.
pub fn is_expression(parser: &mut Parser, driver: &mut Driver) -> bool {
    parser.disable_diagnostics_during(driver, |parser, driver| {
        parser.restore_token_stream_after(|parser| expr::parse_expression(parser, driver)).is_ok()
    })
}
