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
use super::{
    AstAssignmentOp, AstBinaryOp, AstDeclaredType, AstExpression, AstFullExpression, AstNodeId, AstUniqueName,
};
use super::{ParseError, ParseResult, Parser};
use super::{add_error, add_error_at_eof};

use crate::ICE;
use crate::compiler_driver::Driver;
use crate::compiler_driver::diagnostics::Diagnostic;
use crate::compiler_driver::diagnostics::SourceIdentifier;
use crate::compiler_driver::errors::Error;
use crate::lexer;
use crate::lexer::SourceLocation;

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
/// An expression is either a unary expression, a binary expression, an assignment expression, or a ternary/conditional
/// expression, and may be a subexpression inside a larger tree of expressions. Assignment expressions include
/// compound assignment operations like `+=`.
///
/// ```markdown
/// <expr> ::= <unary-expr> | <expr> <binary-op> <expr> | <lhs> <assign-op> <rhs> | <expr> "?" <expr> ":" <expr>
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
    // At the moment this is just a single unary expression, but we name it `left` in case we're about to parse an
    // assignment/ternary/binary operation, in which case it becomes the left-hand-side expression for what
    // follows.
    let mut left = parse_unary_expression(parser, driver)?;

    // Do we have a binary, assignment, or ternary expression?
    //      If so, keep parsing with precedence climbing to build the expression tree. If not, we'll return the
    //      unary expression which we named `left`.
    //      We use a parsing trick to pretend that ternary expressions are binary expressions; see below.
    //
    while let Some(peek_next_token) = parser.token_stream.peek_next_token()
        && (ops::is_binary_operator(&peek_next_token.token_type)
            || ops::is_assignment_operator(&peek_next_token.token_type))
        && ops::operator_precedence(&peek_next_token.token_type) >= min_precedence
    {
        let Some(token) = parser.token_stream.take_token() else {
            ICE!("Expected token");
        };

        let operator_precedence = ops::operator_precedence(&token.token_type);

        // Left-associative means we exclude the next operator's precedence level
        let left_associative_precedence = operator_precedence + 1;

        // Right-associative means we include the next operator's precedence level
        let right_associative_precedence = operator_precedence;

        let token_type = token.token_type.clone();
        let token_loc = token.location;

        match token_type {
            // Assignment (including compound assignment)
            tt if ops::is_assignment_operator(&tt) => {
                let op = ops::translate_assignment_operator(&tt);
                left = parse_assignment_expression(op, &token_loc, left, right_associative_precedence, parser, driver)?;
            }

            // Ternary/conditional expression
            //      We parse conditional expressions with a trick that lets us interpret them as binary operations.
            //      With the grammar:
            //          <condition> "?" <consequent> ":" <alternative>
            //      We pretend that the 3 tokens "?" <consequent> ":" are the binary operator. We lookup the precedence
            //      for the "?" token.
            lexer::TokenType::Ternary => {
                left = parse_ternary_expression(left, right_associative_precedence, parser, driver)?;
            }

            // Binary expression
            _ => {
                let op = ops::translate_binary_operator(&token_type);
                let op_loc = token.location;
                left = parse_binary_expression(op, &op_loc, left, left_associative_precedence, parser, driver)?;
            }
        }
    }

    Ok(left)
}

/// Parses an assignment expression for the given assignment operator `op` and `lhs` expression.
fn parse_assignment_expression(
    op: AstAssignmentOp,
    op_loc: &SourceLocation,
    lhs: AstExpression,
    precedence: i32,
    parser: &mut Parser,
    driver: &mut Driver,
) -> ParseResult<AstExpression> {
    let rhs = parse_expression_with_precedence(parser, driver, precedence)?;

    let node_id = AstNodeId::new();
    parser.metadata.add_source_span(node_id, meta::AstNodeSourceSpanMetadata::from_source_location(op_loc));

    let computation_node_id = AstNodeId::new(); // No source location needed; this is for the typechecker
    Ok(AstExpression::Assignment { node_id, computation_node_id, op, lhs: Box::new(lhs), rhs: Box::new(rhs) })
}

/// Parses a ternary/conditional expression for the given `condition` expression.
fn parse_ternary_expression(
    condition: AstExpression,
    alternative_expr_precedence: i32,
    parser: &mut Parser,
    driver: &mut Driver,
) -> ParseResult<AstExpression> {
    // Get the location of the '?' token.
    let loc = parser.token_stream.prev_token_source_location().unwrap();

    let node_id = AstNodeId::new();
    parser.metadata.add_source_span(node_id, meta::AstNodeSourceSpanMetadata::from_source_location(&loc));

    let consequent = parse_expression(parser, driver)?; // Note: precedence == 0

    let colon_token = parser.token_stream.take_token_if_expected(lexer::TokenType::Colon);
    if colon_token.is_none() {
        let loc = utils::get_next_source_location_after_previous_token(parser);
        add_error(driver, "Missing `:` and alternative in ternary/conditional expression", loc);
        return Err(ParseError);
    }

    let alternative = parse_expression_with_precedence(parser, driver, alternative_expr_precedence);
    if alternative.is_err() {
        let loc = utils::get_next_source_location_after_previous_token(parser);
        add_error(driver, "Missing alternative in ternary/conditional expression", loc);
        return Err(ParseError);
    }
    let alternative = alternative.unwrap();

    Ok(AstExpression::Conditional {
        node_id,
        expr: Box::new(condition),
        consequent: Box::new(consequent),
        alternative: Box::new(alternative),
    })
}

/// Parses a binary expression for the given binary operator `op` and the `lhs` expression.
fn parse_binary_expression(
    op: AstBinaryOp,
    op_loc: &SourceLocation,
    lhs: AstExpression,
    rhs_precedence: i32,
    parser: &mut Parser,
    driver: &mut Driver,
) -> ParseResult<AstExpression> {
    let node_id = AstNodeId::new();
    parser.metadata.add_source_span(node_id, meta::AstNodeSourceSpanMetadata::from_source_location(op_loc));

    let rhs = parse_expression_with_precedence(parser, driver, rhs_precedence)?;

    Ok(AstExpression::BinaryOperation { node_id, op, left: Box::new(lhs), right: Box::new(rhs) })
}

/// Parses a unary expression.
///
/// A unary expression is either a prefix unary operation, a cast expression, or a postfix expression.
///
/// ```markdown
/// <unary-expr> ::= <unary-op> <unary-expr> | <cast-expr> | <postfix-expr>
/// ```
pub fn parse_unary_expression(parser: &mut Parser, driver: &mut Driver) -> ParseResult<AstExpression> {
    if let Some(peek_next_token) = parser.token_stream.peek_next_token() {
        let peek_next_token_type = peek_next_token.token_type.clone();

        match peek_next_token_type {
            // Prefix unary operation
            tt if ops::is_prefix_unary_operator(&tt) => {
                ops::parse_prefix_unary_operation(parser, driver).map_err(|_| ParseError)
            }

            // Cast expression
            lexer::TokenType::OpenParen if peek::is_type_cast_operator(parser, driver) => {
                parse_cast_expression(parser, driver)
            }

            // Postfix expression
            _ => parse_postfix_expression(parser, driver),
        }
    } else {
        add_error_at_eof(parser, driver, "Expected expression".into());
        Err(ParseError)
    }
}

/// Parses a cast expression.
///
/// ```markdown
/// <cast-expr> ::= "(" { <type-specifier> }+ [ <abstract-declarator> ] ")" <unary-expr>
/// ```
fn parse_cast_expression(parser: &mut Parser, driver: &mut Driver) -> ParseResult<AstExpression> {
    let open_paren_loc = utils::expect_token(lexer::TokenType::OpenParen, parser, driver)?;

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

    // The expression being cast
    let expr_to_cast = parse_unary_expression(parser, driver)?;

    // The resolved type to which the expression is being cast
    let target_type = AstDeclaredType::unresolved(basic_type, None, declarator);

    let node_id = AstNodeId::new();
    parser.metadata.add_source_span(node_id, meta::AstNodeSourceSpanMetadata::from_source_location(&open_paren_loc));

    Ok(AstExpression::Cast { node_id, target_type, expr: Box::new(expr_to_cast) })
}

/// Parses a postfix expression.
///
/// A postfix expression is either a function call expression, an array subscript expression, or a postfix
/// increment or decrement operation.
///
/// ```markdown
/// <postfix-expr> ::= <postfix-expr> "(" [ <argument-list> ] ")"
///                    | <postfix-expr> "[" <expr> "]"
///                    | <postfix-expr> "++"
///                    | <postfix-expr> "--"
///                    | <primary-expr>
/// ```
fn parse_postfix_expression(parser: &mut Parser, driver: &mut Driver) -> ParseResult<AstExpression> {
    // Parse the primary expression
    let mut postfix_expr = parse_primary_expression(parser, driver)?;

    // Is there a postfix operator?
    while let Some(peek_next_token) = parser.token_stream.peek_next_token() {
        let peek_next_token_type = peek_next_token.token_type.clone();

        match peek_next_token_type {
            // Function call
            lexer::TokenType::OpenParen => {
                postfix_expr = parse_function_call_expression(postfix_expr, parser, driver)?;
            }

            // Array subscript
            lexer::TokenType::OpenSqBracket => {
                postfix_expr = parse_array_subscript_expression(postfix_expr, parser, driver)?;
            }

            // Postfix increment/decrement
            lexer::TokenType::Increment | lexer::TokenType::Decrement => {
                postfix_expr = ops::parse_postfix_incr_or_decr(postfix_expr, parser, driver)?;
            }
            _ => break,
        }
    }

    Ok(postfix_expr)
}

/// Parses a primary expression.
///
/// A primary expression is either a literal, an identifier, or a parenthesised expression.
///
/// ```markdown
/// <primary-expr> ::= <literal> | <identifier> | "(" <expr> ")"
/// ```
fn parse_primary_expression(parser: &mut Parser, driver: &mut Driver) -> ParseResult<AstExpression> {
    if let Some(peek_next_token) = parser.token_stream.peek_next_token() {
        let peek_next_token_type = peek_next_token.token_type.clone();

        match peek_next_token_type {
            // Literals
            lexer::TokenType::IntegerLiteral { .. } => literal::parse_integer_literal(parser, driver),
            lexer::TokenType::FloatLiteral { .. } => literal::parse_float_literal(parser, driver),

            // Identifier
            lexer::TokenType::Identifier(_) => parse_identifier_expression(parser, driver),

            // Parenthesised expression
            lexer::TokenType::OpenParen => parse_parenthesised_expression(parser, driver),

            _ => {
                add_error(driver, "Expected expression", peek_next_token.location);
                Err(ParseError)
            }
        }
    } else {
        add_error_at_eof(parser, driver, "Expected expression".into());
        Err(ParseError)
    }
}

/// Parses a function call expression where the given `expr` is the function designator.
fn parse_function_call_expression(
    expr: AstExpression,
    parser: &mut Parser,
    driver: &mut Driver,
) -> ParseResult<AstExpression> {
    let (args, args_node_id) = parse_function_call_arguments(parser, driver)?;

    let designator = Box::new(expr);
    let designator_span = parser.metadata.get_source_span(&designator.node_id()).unwrap();

    let node_id = AstNodeId::new();
    parser.metadata.add_source_span(node_id, *designator_span);

    Ok(AstExpression::FunctionCall { node_id, designator, args, args_node_id })
}

/// Parses the function call arguments.
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

/// Parses an array subscript expression where the given `expr` is the expression where the postfix '[]' is applied to.
fn parse_array_subscript_expression(
    expr: AstExpression,
    parser: &mut Parser,
    driver: &mut Driver,
) -> ParseResult<AstExpression> {
    let open_sq_bracket_loc = utils::expect_token(lexer::TokenType::OpenSqBracket, parser, driver)?;

    let subscript_expr = parse_expression(parser, driver)?; // Note: precedence == 0

    _ = utils::expect_token(lexer::TokenType::CloseSqBracket, parser, driver)?;

    let node_id = AstNodeId::new();

    // Set the source location as the '[' token. (Diagnostics can add the other expressions as locations.)
    parser.metadata.add_source_span(
        node_id,
        meta::AstNodeSourceSpanMetadata::from_source_location(&open_sq_bracket_loc),
    );

    Ok(AstExpression::Subscript { node_id, expr1: Box::new(expr), expr2: Box::new(subscript_expr) })
}

/// Parses an identifier expression.
fn parse_identifier_expression(parser: &mut Parser, driver: &mut Driver) -> ParseResult<AstExpression> {
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
    let unique_name = resolve_identifier(name, identifier_token.location, parser, driver)?;

    Ok(AstExpression::Identifier { node_id, name: name.clone(), unique_name })
}

/// Parses a parenthesised expression.
fn parse_parenthesised_expression(parser: &mut Parser, driver: &mut Driver) -> ParseResult<AstExpression> {
    _ = utils::expect_token(lexer::TokenType::OpenParen, parser, driver)?;

    let expr = parse_expression(parser, driver)?; // Note: precedence == 0

    _ = utils::expect_token(lexer::TokenType::CloseParen, parser, driver)?;

    // Track that the expression was wrapped in parentheses so that later we can warn about mixing different binary
    // operators without parentheses.
    parser.metadata.set_expr_has_parens(expr.node_id());

    Ok(expr)
}

/// Resolves the given identifier to its unique name.
fn resolve_identifier(
    identifier: &str,
    identifier_loc: SourceLocation,
    parser: &mut Parser,
    driver: &mut Driver,
) -> ParseResult<AstUniqueName> {
    // Is there a function or variable visible from the current scope for the given identifier?
    if let Some(decl) = parser.identifiers.resolve_identifier(identifier, SearchScope::All) {
        if decl.kind == SymbolKind::Variable || decl.kind == SymbolKind::Function {
            Ok(decl.unique.clone())
        } else {
            let err = format!("'{}' is a type alias in this scope", identifier);
            let mut diag = Diagnostic::error_at_location(err, identifier_loc);
            diag.add_note(format!("{} '{}' was previously declared here:", decl.kind, identifier), Some(decl.loc));
            driver.add_diagnostic(diag);

            Err(ParseError)
        }
    } else {
        let err = if utils::is_builtin_type_specifier(identifier) {
            format!("Invalid use of type `{}`", identifier)
        } else {
            format!("Use of undeclared identifier `{}`", identifier)
        };

        driver.add_diagnostic(Diagnostic::error_at_location(err, identifier_loc));

        Err(ParseError)
    }
}
