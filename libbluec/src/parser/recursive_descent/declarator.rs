// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `declarator` module provides functionality for parsing declarators in declarations.

use crate::ICE;
use crate::compiler_driver::{Driver, Error};
use crate::core::SourceIdentifier;
use crate::lexer::TokenType;

use super::decl::parse_type_and_storage_specifiers;
use super::literal;
use super::peek;
use super::utils;
use super::{AstDeclarator, AstDeclaratorKind, AstDeclaredType, AstExpression, AstIdentifier};
use super::{ParseError, ParseResult, Parser, add_error, add_error_at_eof};

/// A declarator can be parsed with or without an identifier.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum ParseOption {
    /// Parse a declarator (with an identifier).
    RequireIdentifier,

    // Parse an abstract declarator (without an identifier).
    RequireAbstract,

    // Parse either a declarator or an abstract declarator.
    AllowAbstract,
}

/// Parses an [AstDeclarator].
///
/// A declarator is the part of a declaration that specifies the identifier and modifiers of a variable or function.
/// An abstract declarator omits the identifier.
///
/// ```markdown
/// <declarator>        ::= "*" [ <declarator> | <direct-declarator> ]
/// <direct-declarator> ::= <simple-declarator> [ <declarator-suffix> ]
/// <declarator-suffix> ::= <param-list> | { "[" <int> "]" }+
/// <simple-declarator> ::= <identifier> | "(" <declarator> ")"
/// <param-list>        ::= "(" "void" ")" | "(" <param> { "," <param> }+ ")"
/// <param>             ::= { <type-specifier> }+ <declarator>
/// ```
pub fn parse_declarator(parser: &mut Parser, driver: &mut Driver, opt: ParseOption) -> ParseResult<AstDeclarator> {
    parse_declarator_recursively(parser, driver, opt)
}

fn parse_declarator_recursively(
    parser: &mut Parser,
    driver: &mut Driver,
    opt: ParseOption,
) -> ParseResult<AstDeclarator> {
    let start_decl_loc = parser.token_stream.peek_next_source_location().ok_or(ParseError)?;

    let kind = match parser.token_stream.peek_next_token() {
        // Identifier
        //
        Some(tok) if tok.is_identifier() => {
            let identifier_token = parser.token_stream.take_token().unwrap();
            let identifier = identifier_token.get_identifier().unwrap();

            // Identifier cannot be a keyword
            if utils::is_reserved_keyword(identifier) {
                Error::identifier_cannot_be_keyword(SourceIdentifier(identifier, identifier_token.location), driver);
                return Err(ParseError);
            }

            // An abstract declarator cannot have an identifier
            if opt == ParseOption::RequireAbstract {
                add_error(driver, "A type name cannot have an identifier.", identifier_token.location);
                return Err(ParseError);
            }

            AstDeclaratorKind::Ident(AstIdentifier::new(identifier, identifier_token.location))
        }

        // Pointer
        //
        Some(tok) if tok.has_type(TokenType::Multiply) => {
            _ = parser.token_stream.take_token();

            // Should we try and parse a declarator after this '*' token?
            //
            let skip_declarator = if opt == ParseOption::AllowAbstract || opt == ParseOption::RequireAbstract {
                match parser.token_stream.peek_next_token() {
                    Some(token) => token.has_type(TokenType::Comma) || token.has_type(TokenType::CloseParen),
                    None => true,
                }
            } else {
                false
            };

            if skip_declarator {
                AstDeclaratorKind::AbstractPointer
            } else {
                let decl = parse_declarator_recursively(parser, driver, opt)?;
                AstDeclaratorKind::Pointer(Box::new(decl))
            }
        }

        // Array
        //
        Some(tok) if tok.has_type(TokenType::OpenSqBracket) => {
            let size = parse_array_size(parser, driver)?;
            AstDeclaratorKind::AbstractArray { size }
        }

        // Function
        //
        Some(tok) if tok.has_type(TokenType::OpenParen) => {
            _ = parser.token_stream.take_token();

            // We either have a parenthesised declarator or a parameter list with no parent declarator.
            // E.g. `int (float, double)`
            //           ~~~~~~~~~~~~~~~
            // But a parenthesised declarator is more likely: `int (*fnptr)(float, double)`
            //                                                     ~~~~~~~~
            //
            if peek::is_type_and_storage_specifiers(parser, driver) {
                let params = parse_function_parameters(parser, driver)?;

                _ = utils::expect_token(TokenType::CloseParen, parser, driver)?;

                AstDeclaratorKind::AbstractFunction { params }
            } else {
                let decl = parse_declarator_recursively(parser, driver, opt)?;

                _ = utils::expect_token(TokenType::CloseParen, parser, driver)?;

                decl.kind
            }
        }

        _ => {
            let loc = utils::get_next_source_location_after_previous_token(parser);
            Error::expect_identifier(loc, driver);
            return Err(ParseError);
        }
    };

    let end_decl_loc = parser.token_stream.prev_token_source_location().unwrap();
    let loc = start_decl_loc.merge_with(end_decl_loc);

    let mut declarator = AstDeclarator::new(kind, loc);

    // Are there any array suffixes?
    //
    while parser.token_stream.next_token_has_type(TokenType::OpenSqBracket) {
        let size = parse_array_size(parser, driver)?;

        let end_decl_loc = parser.token_stream.prev_token_source_location().unwrap();
        let loc = start_decl_loc.merge_with(end_decl_loc);

        let kind = AstDeclaratorKind::Array { decl: Box::new(declarator), size };

        declarator = AstDeclarator::new(kind, loc);
    }

    // Is there a parameter list after the declarator?
    //      This is different to the parameter list case above in an abstract declarator. In this case we have
    //      a declarator.
    //
    if parser.token_stream.next_token_has_type(TokenType::OpenParen) {
        _ = parser.token_stream.take_token();

        let params = parse_function_parameters(parser, driver)?;

        _ = utils::expect_token(TokenType::CloseParen, parser, driver)?;

        let end_decl_loc = parser.token_stream.prev_token_source_location().unwrap();
        let loc = start_decl_loc.merge_with(end_decl_loc);

        let kind = AstDeclaratorKind::Function { decl: Box::new(declarator), params };

        Ok(AstDeclarator::new(kind, loc))
    } else {
        Ok(declarator)
    }
}

/// Parses a function's parameters.
fn parse_function_parameters(parser: &mut Parser, driver: &mut Driver) -> ParseResult<Vec<AstDeclaredType>> {
    if parser.token_stream.next_token_has_type(TokenType::new_identifier("void")) {
        _ = parser.token_stream.take_token();
        return Ok(Vec::new());
    }

    let mut params = Vec::new();
    let mut first_param = true;

    while !parser.token_stream.next_token_has_type(TokenType::CloseParen) {
        if !first_param {
            _ = utils::expect_token(TokenType::Comma, parser, driver)?;
        }

        // Parameter type
        const EXPECTS_IDENTIFIER: bool = false;
        let (basic_type, storage) = parse_type_and_storage_specifiers(parser, driver, EXPECTS_IDENTIFIER)?;

        if let Some(storage) = storage {
            add_error(driver, "A function parameter cannot have a storage class specifier", storage.loc);
        }

        // Is there a parameter declarator?
        // We require a declarator with an identifier in function definitions, which we validate later.
        // Params in function declarations may have no declarator (e.g. type only) or may be an abstract declarator
        // with no identifier.
        //
        let skip_declarator = parser
            .token_stream
            .peek_next_token()
            .is_some_and(|token| token.has_type(TokenType::Comma) || token.has_type(TokenType::CloseParen));

        let declarator = if skip_declarator {
            None
        } else {
            Some(parse_declarator_recursively(parser, driver, ParseOption::AllowAbstract)?)
        };

        params.push(AstDeclaredType::unresolved(basic_type, None, declarator));

        first_param = false;
    }

    Ok(params)
}

fn parse_array_size(parser: &mut Parser, driver: &mut Driver) -> ParseResult<usize> {
    _ = utils::expect_token(TokenType::OpenSqBracket, parser, driver)?;

    // We allow zero-length arrays (with the zero constant omitted) so check if the next token is the
    // closing square bracket before trying to parse an integer literal, i.e. we're parsing '[]'.
    //
    let size = if parser.token_stream.next_token_has_type(TokenType::CloseSqBracket) {
        0
    } else {
        // TODO: Allow constant expression for array declaration, e.g. int arr[10 + 1];

        // The array size can be an integer literal or a char literal (which evaluates to 'int').
        if let Some(peek_next_token) = parser.token_stream.peek_next_token() {
            match peek_next_token.token_type {
                TokenType::IntegerLiteral { .. } => {
                    let literal = literal::parse_integer_literal(parser, driver)?;

                    let AstExpression::IntegerLiteral { value, .. } = literal else {
                        ICE!("Should have parsed an IntegerLiteral");
                    };

                    value as usize
                }

                TokenType::CharLiteral { .. } => {
                    let literal = literal::parse_char_literal(parser, driver)?;

                    let AstExpression::CharLiteral { value, .. } = literal else {
                        ICE!("Should have parsed a CharLiteral");
                    };

                    value as usize
                }

                _ => {
                    add_error(driver, "Expected an integer constant", peek_next_token.location);
                    return Err(ParseError);
                }
            }
        } else {
            add_error_at_eof(parser, driver, "Expected an integer constant");
            return Err(ParseError);
        }
    };

    _ = utils::expect_token(TokenType::CloseSqBracket, parser, driver)?;

    Ok(size)
}
