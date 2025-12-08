// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `expr` module defines the various parsing functions for expressions.

pub mod ops;

use super::identifier_resolution::SearchScope;
use super::meta;
use super::recursive_descent::decl::parse_type_and_storage_specifiers;
use super::recursive_descent::declarator;
use super::recursive_descent::literal;
use super::recursive_descent::peek;
use super::recursive_descent::utils;
use super::symbol::SymbolKind;
use super::{AstDeclaredType, AstExpression, AstFullExpression, AstNodeId, AstUniqueName};
use super::{ParseError, ParseResult, Parser};
use super::{add_error, add_error_at_eof};

use crate::ICE;
use crate::compiler_driver::Driver;
use crate::compiler_driver::diagnostics::Diagnostic;
use crate::compiler_driver::diagnostics::SourceIdentifier;
use crate::compiler_driver::errors::Error;
use crate::lexer;

/// Parses a full expression.
///
/// A full expression is an expression that is not a subexpression of another expression.
pub fn parse_full_expression(parser: &mut Parser, driver: &mut Driver) -> ParseResult<AstFullExpression> {
    // Capture the source location of the first token in the full expression.
    let start_loc = parser.token_stream.peek_next_source_location().ok_or(ParseError)?;

    let expr = parse_expression_with_precedence(parser, driver, 0)?;

    // Capture the source location of the last token in the full expression.
    let end_loc = parser.token_stream.prev_token_source_location().ok_or(ParseError)?;

    let node_id = AstNodeId::new();
    parser
        .metadata
        .add_source_span(node_id, meta::AstNodeSourceSpanMetadata::from_source_location_pair(&start_loc, &end_loc));

    Ok(AstFullExpression { node_id, expr })
}

/// Attempts to parse a full expression unless the very next token is the given terminator token, e.g. ';' or ')'.
pub fn parse_optional_full_expression(
    parser: &mut Parser,
    driver: &mut Driver,
    terminator: lexer::TokenType,
) -> ParseResult<AstFullExpression> {
    if parser.token_stream.next_token_has_type(terminator) {
        Err(ParseError)
    } else {
        parse_full_expression(parser, driver)
    }
}

/// Parses a tree of (sub)expression(s).
///
/// An expression is either a factor, a binary operation, or a ternary/conditional expression, and
/// may be a subexpression inside a larger tree of expressions.
///
/// ```markdown
/// <exp> ::= <factor> | <exp> <binop> <exp> | <exp> "?" <exp> ":" <exp>
/// ```
pub fn parse_expression(parser: &mut Parser, driver: &mut Driver) -> ParseResult<AstExpression> {
    parse_expression_with_precedence(parser, driver, 0)
}

/// Expressions are parsed with precedence climbing (either left- or right-associative depending on the operator).
///
/// Assignment requires right-associative precedence climbing:  a = b = c    --->   a = (b = c)
/// Remember compound assignment too:                           a += b /= c  --->   a += (b /= c)
/// But everything else is left-associative:                    a + b + c    --->   (a + b) + c
fn parse_expression_with_precedence(
    parser: &mut Parser,
    driver: &mut Driver,
    min_precedence: i32,
) -> ParseResult<AstExpression> {
    // Capture the source location of the first token in the left-hand-side factor/expression.
    // We may need this metadata for an error diagnostic below.
    let mut left_loc = parser.token_stream.peek_next_source_location().ok_or(ParseError)?;

    // This may be a single factor, but we'll call it `left` in case we have an assignment/ternary/binary operation.
    let mut left = parse_factor(parser, driver)?;

    while let Some(peek_next_token) = parser.token_stream.peek_next_token()
        && (ops::is_binary_operator(&peek_next_token.token_type)
            || ops::is_assignment_operator(&peek_next_token.token_type))
        && ops::operator_precedence(&peek_next_token.token_type) >= min_precedence
    {
        let Some(next_token) = parser.token_stream.take_token() else {
            ICE!("Expected token");
        };

        // Extend the `left` expression's source location to include everything we've parsed for it.
        left_loc.set_span_up_to_location(&next_token.location);

        let operator_precedence = ops::operator_precedence(&next_token.token_type);

        // Left-associative means we exclude the next operator's precedence level
        let left_associative_precedence = operator_precedence + 1;

        // Right-associative means we include the next operator's precedence level
        let right_associative_precedence = operator_precedence;

        let next_token_type = next_token.token_type.clone();

        match next_token_type {
            // Assignment (including compound assignment)
            //
            token_type if ops::is_assignment_operator(&token_type) => {
                let node_id = AstNodeId::new();
                let computation_node_id = AstNodeId::new();

                // For an assignment we set the source span to be the `lhs` expression, since that's the object
                // being assigned to.
                parser
                    .metadata
                    .add_source_span(node_id, meta::AstNodeSourceSpanMetadata::from_source_location(&left_loc));

                let right = parse_expression_with_precedence(parser, driver, right_associative_precedence)?;

                let op = ops::parse_assignment_operator(&token_type);

                left = AstExpression::Assignment {
                    node_id,
                    computation_node_id,
                    op,
                    lhs: Box::new(left),
                    rhs: Box::new(right),
                };
            }

            // Ternary/conditional expression
            //      We parse conditional expressions with a trick that lets us interpret them as binary operations.
            //      With the grammar:
            //          <condition> "?" <consequent> ":" <alternative>
            //      We pretend that the 3 tokens "?" <consequent> ":" are the binary operator. We lookup the precedence
            //      for the "?" token.
            lexer::TokenType::Ternary => {
                let node_id = AstNodeId::new();

                // Get the location of the '?' token.
                let loc = parser.token_stream.prev_token_source_location().unwrap();

                parser.metadata.add_source_span(node_id, meta::AstNodeSourceSpanMetadata::from_source_location(&loc));

                let consequent = parse_expression(parser, driver)?; // Note: precedence == 0

                let colon_token = parser.token_stream.take_token_if_expected(lexer::TokenType::Colon);
                if colon_token.is_none() {
                    let loc = utils::get_next_source_location_after_previous_token(parser);
                    add_error(driver, "Missing `:` and alternative in ternary/conditional expression", loc);
                    return Err(ParseError);
                }

                let alternative = parse_expression_with_precedence(parser, driver, right_associative_precedence);
                if alternative.is_err() {
                    let loc = utils::get_next_source_location_after_previous_token(parser);
                    add_error(driver, "Missing alternative in ternary/conditional expression", loc);
                    return Err(ParseError);
                }
                let alternative = alternative.unwrap();

                left = AstExpression::Conditional {
                    node_id,
                    expr: Box::new(left),
                    consequent: Box::new(consequent),
                    alternative: Box::new(alternative),
                };
            }

            // Binary operators
            _ => {
                let op = ops::parse_binary_operator(&next_token_type);

                let node_id = AstNodeId::new();
                parser.metadata.add_source_span(
                    node_id,
                    meta::AstNodeSourceSpanMetadata::from_source_location(&next_token.location),
                );

                let right = parse_expression_with_precedence(parser, driver, left_associative_precedence)?;

                left = AstExpression::BinaryOperation { node_id, op, left: Box::new(left), right: Box::new(right) };
            }
        }
    }

    Ok(left)
}

/// Parses a factor.
///
/// A factor is either an integer literal, a variable identifier, a function call,
/// a prefix/postfix unary operation, or a parenthesised-expression.
///
/// ```markdown
/// <factor> ::= <int> | <identifier> | <identifier> "(" [<argument-list>] ")" |
///              <unop> <factor> | <factor> <unop> | "(" <exp> ")" | "(" {type-specifier}+ ")" <factor>
/// ```
pub fn parse_factor(parser: &mut Parser, driver: &mut Driver) -> ParseResult<AstExpression> {
    // Peek at the next two tokens because we need to distinguish a variable from a function call.
    match parser.token_stream.peek_next_2_tokens() {
        // Function call
        (Some(t1), Some(t2))
            if utils::token_is_identifier_and_not_reserved(t1) && t2.has_type(lexer::TokenType::OpenParen) =>
        {
            parse_function_call_expression(parser, driver)
        }

        (Some(t1), _) => {
            match &t1.token_type {
                lexer::TokenType::IntegerLiteral { .. } => literal::parse_integer_literal(parser, driver),
                lexer::TokenType::FloatLiteral { .. } => literal::parse_float_literal(parser, driver),

                // Variable
                lexer::TokenType::Identifier(_) => parse_variable_expression(parser, driver),

                // Prefix unary operation
                op if ops::is_prefix_unary_operator(op) => {
                    ops::parse_prefix_unary_operation(parser, driver).map_err(|_| ParseError)
                }

                lexer::TokenType::OpenParen => {
                    //
                    // Cast expression
                    //
                    if peek::is_type_cast_operator(parser, driver) {
                        let node_id = AstNodeId::new();

                        let open_paren_token = parser.token_stream.take_token().unwrap(); // Open Paren

                        parser.metadata.add_source_span(
                            node_id,
                            meta::AstNodeSourceSpanMetadata::from_source_location(&open_paren_token.location),
                        );

                        // Type specifiers
                        let (basic_type, storage) = parse_type_and_storage_specifiers(parser, driver, false)?;
                        if let Some(storage) = storage {
                            add_error(driver, "A cast expression cannot have a storage class specifier", storage.loc);
                            return Err(ParseError);
                        }

                        // Optional abstract declarator
                        let has_declarator = !parser.token_stream.next_token_has_type(lexer::TokenType::CloseParen);

                        let declarator = if has_declarator {
                            let require_abstract = declarator::ParseOption::RequireAbstract;
                            let declarator = declarator::parse_declarator(parser, driver, require_abstract)?;
                            Some(declarator)
                        } else {
                            None
                        };

                        _ = utils::expect_token(lexer::TokenType::CloseParen, parser, driver)?;

                        let cast_expr = parse_factor(parser, driver)?;

                        let target_type = AstDeclaredType::unresolved(basic_type, None, declarator);
                        Ok(AstExpression::Cast { node_id, target_type, expr: Box::new(cast_expr) })
                    }
                    //
                    // Parenthesised expression (with possible function call)
                    //
                    else {
                        _ = parser.token_stream.take_token(); // Open Paren

                        let inner_expr = parse_expression(parser, driver)?; // Note: precedence == 0, because it's inside parentheses

                        _ = utils::expect_token(lexer::TokenType::CloseParen, parser, driver)?;

                        // Track that the expression was wrapped in parentheses so that later we can warn about
                        // mixing different binary operators without parentheses.
                        parser.metadata.set_expr_has_parens(inner_expr.node_id());

                        // Is this a function call?
                        if parser.token_stream.next_token_has_type(lexer::TokenType::OpenParen) {
                            let (args, args_node_id) = parse_function_call_arguments(parser, driver)?;

                            let node_id = AstNodeId::new();
                            let span = parser.metadata.get_source_span(&inner_expr.node_id()).unwrap();
                            parser.metadata.add_source_span(node_id, *span);

                            return Ok(AstExpression::FunctionCall {
                                node_id,
                                designator: Box::new(inner_expr),
                                args,
                                args_node_id,
                            });
                        }

                        // Peek ahead for a postfix unary operator
                        if ops::is_next_token_a_postfix_unary_operator(parser) {
                            ops::parse_postfix_unary_operation(inner_expr, parser, driver)
                        } else {
                            Ok(inner_expr)
                        }
                    }
                }

                _ => {
                    add_error(driver, "Missing expression", t1.location);
                    Err(ParseError)
                }
            }
        }

        _ => {
            add_error_at_eof(parser, driver, "Expected expression".into());
            Err(ParseError)
        }
    }
}

fn parse_function_call_expression(parser: &mut Parser, driver: &mut Driver) -> ParseResult<AstExpression> {
    let identifier_token = parser.token_stream.take_token().ok_or(ParseError)?;

    let name = identifier_token
        .get_identifier()
        .or_else(|| {
            ICE!("Expected identifier token");
        })
        .unwrap()
        .clone();

    let identifier_token_loc = identifier_token.location;

    // Identifier resolution
    let unique_name = verify_function_identifier(&name, identifier_token_loc, parser, driver)?;

    let node_id = AstNodeId::new();
    parser
        .metadata
        .add_source_span(node_id, meta::AstNodeSourceSpanMetadata::from_source_location(&identifier_token_loc));

    let designator = Box::new(AstExpression::Identifier { node_id, name, unique_name });

    make_function_call_expression(designator, parser, driver)
}

fn make_function_call_expression(
    designator: Box<AstExpression>,
    parser: &mut Parser,
    driver: &mut Driver,
) -> ParseResult<AstExpression> {
    let (args, args_node_id) = parse_function_call_arguments(parser, driver)?;

    let node_id = AstNodeId::new();
    let designator_span = parser.metadata.get_source_span(&designator.node_id()).unwrap();
    parser.metadata.add_source_span(node_id, *designator_span);

    let fn_call = AstExpression::FunctionCall { node_id, designator, args, args_node_id };

    if parser.token_stream.next_token_has_type(lexer::TokenType::OpenParen) {
        make_function_call_expression(Box::new(fn_call), parser, driver)
    } else {
        Ok(fn_call)
    }
}

fn parse_function_call_arguments(
    parser: &mut Parser,
    driver: &mut Driver,
) -> ParseResult<(Vec<AstExpression>, AstNodeId)> {
    _ = utils::expect_token(lexer::TokenType::OpenParen, parser, driver)?;

    let args_start_loc = parser.token_stream.peek_next_source_location().ok_or(ParseError)?;

    let mut args = Vec::new();
    let mut first_arg = true;

    while !parser.token_stream.next_token_has_type(lexer::TokenType::CloseParen) {
        if !first_arg {
            _ = utils::expect_token(lexer::TokenType::Comma, parser, driver)?;
        }

        let arg = parse_expression(parser, driver)?;
        args.push(arg);

        first_arg = false;
    }

    let args_end_loc = parser.token_stream.prev_token_source_location().ok_or(ParseError)?;

    _ = utils::expect_token(lexer::TokenType::CloseParen, parser, driver)?;

    let args_node_id = AstNodeId::new();
    parser.metadata.add_source_span(
        args_node_id,
        meta::AstNodeSourceSpanMetadata::from_source_location_pair(&args_start_loc, &args_end_loc),
    );

    Ok((args, args_node_id))
}

fn parse_variable_expression(parser: &mut Parser, driver: &mut Driver) -> ParseResult<AstExpression> {
    let identifier_token = parser.token_stream.take_token().cloned().ok_or(ParseError)?;

    let name = identifier_token.get_identifier().unwrap_or_else(|| {
        ICE!("Expected identifier token");
    });

    if utils::is_reserved_keyword(name) {
        let variable = SourceIdentifier(name, identifier_token.location);
        Error::identifier_cannot_be_keyword(variable, driver);
        return Err(ParseError);
    }

    let node_id = AstNodeId::new();

    parser
        .metadata
        .add_source_span(node_id, meta::AstNodeSourceSpanMetadata::from_source_location(&identifier_token.location));

    // Identifier resolution
    //
    let unique_name = verify_identifier(name, identifier_token.location, parser, driver)?;

    let identifier = AstExpression::Identifier { node_id, name: name.clone(), unique_name };

    // Peek ahead for a postfix unary operator
    if ops::is_next_token_a_postfix_unary_operator(parser) {
        ops::parse_postfix_unary_operation(identifier, parser, driver).map_err(|_| ParseError)
    } else {
        Ok(identifier)
    }
}

fn verify_function_identifier(
    function_name: &str,
    function_name_loc: lexer::SourceLocation,
    parser: &mut Parser,
    driver: &mut Driver,
) -> ParseResult<AstUniqueName> {
    // Is there a function (or variable, if a function pointer) visible from the current scope for the given identifier?
    //      Sema will typecheck that the variable is a function pointer later.
    //
    if let Some(decl) = parser.identifiers.resolve_identifier(function_name, SearchScope::All) {
        if decl.kind == SymbolKind::Function || decl.kind == SymbolKind::Variable {
            Ok(decl.unique.clone())
        } else {
            let err = format!(
                "Cannot call '{}' because '{}' is not a function in this scope",
                &function_name, &function_name
            );
            let mut diag = Diagnostic::error_at_location(err, function_name_loc);
            diag.add_note(format!("{} '{}' was previously declared here:", decl.kind, &function_name), Some(decl.loc));
            driver.add_diagnostic(diag);
            Err(ParseError)
        }
    } else {
        Error::call_undeclared_function(SourceIdentifier(function_name, function_name_loc), driver);
        Err(ParseError)
    }
}

fn verify_identifier(
    variable_name: &str,
    variable_name_loc: lexer::SourceLocation,
    parser: &mut Parser,
    driver: &mut Driver,
) -> ParseResult<AstUniqueName> {
    // Is there a function or variable visible from the current scope for the given identifier?
    //      We may be initializing a function pointer. Sema will perform typechecking later.
    //
    if let Some(decl) = parser.identifiers.resolve_identifier(variable_name, SearchScope::All) {
        if decl.kind == SymbolKind::Variable || decl.kind == SymbolKind::Function {
            Ok(decl.unique.clone())
        } else {
            let err = format!("'{}' is not a variable in this scope", variable_name);
            let mut diag = Diagnostic::error_at_location(err, variable_name_loc);
            diag.add_note(format!("{} '{}' was previously declared here:", decl.kind, variable_name), Some(decl.loc));
            driver.add_diagnostic(diag);

            Err(ParseError)
        }
    } else {
        let err = if utils::is_builtin_type_specifier(variable_name) {
            format!("Invalid use of type `{}`", variable_name)
        } else {
            format!("Use of undeclared identifier `{}`", variable_name)
        };

        driver.add_diagnostic(Diagnostic::error_at_location(err, variable_name_loc));

        Err(ParseError)
    }
}
