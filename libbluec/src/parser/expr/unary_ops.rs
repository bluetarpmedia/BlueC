// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The unary_ops module defines some helper functions for parsing unary operations.

use super::super::meta;
use super::super::{AstExpression, AstNodeId, AstUnaryOp, ParseError, ParseResult, Parser};
use super::parse_factor;

use crate::compiler_driver::Driver;
use crate::compiler_driver::errors::Error;
use crate::internal_error;
use crate::lexer;

/// Parses a prefix unary operation and returns an AST expression.
pub fn parse_prefix_unary_operation(parser: &mut Parser, driver: &mut Driver) -> ParseResult<AstExpression> {
    let Some(unary_op_token) = parser.token_stream.take_token() else {
        internal_error::ICE("Parser: Token should exist");
    };

    let unary_op_token = unary_op_token.clone();
    let op = parse_prefix_unary_operator(&unary_op_token.token_type);
    let factor = parse_factor(parser, driver)?;

    // Prefix increment and decrement can only be applied to l-values.
    //
    if is_incr_or_decr_op(&op) && !factor.is_lvalue() {
        Error::expression_is_not_assignable(unary_op_token.location, driver);
        return Err(ParseError);
    }

    let end_loc = parser.token_stream.prev_token_source_location().expect("Should have a location");

    let node_id = AstNodeId::new();
    parser.metadata.add_source_span(
        node_id,
        meta::AstNodeSourceSpanMetadata::from_source_location_pair(&unary_op_token.location, &end_loc),
    );

    // We parse dereference and address-of as unary operations but output dedicated AstExpressions for them,
    // rather than storing them as `AstExpression::UnaryOperation`s. This makes it easier to handle them later
    // for type checking etc.
    let expr = match op {
        AstUnaryOp::Deref => AstExpression::Deref { node_id, expr: Box::new(factor) },
        AstUnaryOp::AddressOf => AstExpression::AddressOf { node_id, expr: Box::new(factor) },
        _ => AstExpression::UnaryOperation { node_id, op, expr: Box::new(factor) },
    };

    Ok(expr)
}

/// Parses a postfix unary operation for the given factor and returns an AST expression.
pub fn parse_postfix_unary_operation(
    factor: AstExpression,
    parser: &mut Parser,
    driver: &mut Driver,
) -> ParseResult<AstExpression> {
    let Some(unary_op_token) = parser.token_stream.take_token() else {
        internal_error::ICE("Parser: Token should exist");
    };

    let unary_op_token = unary_op_token.clone();

    let op = match unary_op_token.token_type {
        lexer::TokenType::Increment => AstUnaryOp::PostfixIncrement,
        lexer::TokenType::Decrement => AstUnaryOp::PostfixDecrement,
        _ => internal_error::ICE("Invalid postfix operator"),
    };

    // Postfix increment and decrement can only be applied to l-values.
    if is_incr_or_decr_op(&op) && !factor.is_lvalue() {
        Error::expression_is_not_assignable(unary_op_token.location, driver);
        return Err(ParseError);
    }

    let end_loc = parser.token_stream.prev_token_source_location().expect("Should have a location");

    let node_id = AstNodeId::new();
    parser.metadata.add_source_span(
        node_id,
        meta::AstNodeSourceSpanMetadata::from_source_location_pair(&unary_op_token.location, &end_loc),
    );

    Ok(AstExpression::UnaryOperation { node_id, op, expr: Box::new(factor) })
}

/// Parses a token and returns an AST unary operator.
pub fn parse_prefix_unary_operator(token_type: &lexer::TokenType) -> AstUnaryOp {
    match token_type {
        lexer::TokenType::Minus => AstUnaryOp::Negate,
        lexer::TokenType::Plus => AstUnaryOp::Plus,
        lexer::TokenType::BitwiseNot => AstUnaryOp::BitwiseNot,
        lexer::TokenType::LogicalNot => AstUnaryOp::LogicalNot,
        lexer::TokenType::Increment => AstUnaryOp::PrefixIncrement,
        lexer::TokenType::Decrement => AstUnaryOp::PrefixDecrement,
        lexer::TokenType::Multiply => AstUnaryOp::Deref,
        lexer::TokenType::BitwiseAnd => AstUnaryOp::AddressOf,
        _ => internal_error::ICE("Parser: Token is not a unary operator"),
    }
}

/// Returns whether the given token is a prefix unary operator.
pub fn is_prefix_unary_operator(token_type: &lexer::TokenType) -> bool {
    matches!(
        token_type,
        lexer::TokenType::BitwiseNot
            | lexer::TokenType::LogicalNot
            | lexer::TokenType::Minus
            | lexer::TokenType::Plus
            | lexer::TokenType::Increment
            | lexer::TokenType::Decrement
            | lexer::TokenType::Multiply // Dereference
            | lexer::TokenType::BitwiseAnd // Address-of
    )
}

/// Peeks ahead and returns whether the next token is a postfix unary operator.
pub fn is_next_token_a_postfix_unary_operator(parser: &mut Parser) -> bool {
    let Some(peek_next_token) = parser.token_stream.peek_next_token() else {
        return false;
    };

    matches!(peek_next_token.token_type, lexer::TokenType::Increment | lexer::TokenType::Decrement)
}

/// Is the unary operator a pre- or post-fix increment/decrement operator?
pub fn is_incr_or_decr_op(op: &AstUnaryOp) -> bool {
    matches!(
        op,
        AstUnaryOp::PrefixIncrement
            | AstUnaryOp::PrefixDecrement
            | AstUnaryOp::PostfixIncrement
            | AstUnaryOp::PostfixDecrement
    )
}
