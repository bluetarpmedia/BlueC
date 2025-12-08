// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `ops` module defines helper functions for parsing unary and binary operations.

use super::super::meta;
use super::super::{
    AstAssignmentOp, AstBinaryOp, AstExpression, AstNodeId, AstUnaryOp, ParseError, ParseResult, Parser,
};
use super::parse_factor;

use crate::ICE;
use crate::compiler_driver::Driver;
use crate::compiler_driver::errors::Error;
use crate::lexer;

/// Parses a prefix unary operation and returns an AST expression.
pub fn parse_prefix_unary_operation(parser: &mut Parser, driver: &mut Driver) -> ParseResult<AstExpression> {
    let Some(unary_op_token) = parser.token_stream.take_token() else {
        ICE!("Token should exist");
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
        ICE!("Token should exist");
    };

    let unary_op_token = unary_op_token.clone();

    let op = match unary_op_token.token_type {
        lexer::TokenType::Increment => AstUnaryOp::PostfixIncrement,
        lexer::TokenType::Decrement => AstUnaryOp::PostfixDecrement,
        _ => ICE!("Invalid postfix operator '{}'", unary_op_token.token_type),
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
        _ => ICE!("Token '{token_type}' is not a unary operator"),
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

/// Is the token a binary operator?
#[rustfmt::skip]
pub fn is_binary_operator(token_type: &lexer::TokenType) -> bool {
    matches!(token_type,
        lexer::TokenType::Plus                    |
        lexer::TokenType::Minus                   |
        lexer::TokenType::Multiply                |
        lexer::TokenType::Divide                  | 
        lexer::TokenType::Remainder               |
        lexer::TokenType::BitwiseAnd              |
        lexer::TokenType::BitwiseXor              |
        lexer::TokenType::BitwiseOr               |
        lexer::TokenType::LeftShift               |
        lexer::TokenType::RightShift              |
        lexer::TokenType::LogicalAnd              |
        lexer::TokenType::LogicalOr               |
        lexer::TokenType::EqualTo                 |
        lexer::TokenType::NotEqualTo              |
        lexer::TokenType::LessThan                |
        lexer::TokenType::GreaterThan             |
        lexer::TokenType::LessThanOrEqualTo       |
        lexer::TokenType::GreaterThanOrEqualTo    |

        // We parse conditional/ternary expressions with a trick that lets
        // them appear to be binary operations.
        lexer::TokenType::Ternary
    )
}

/// Is the token an assignment operator?
#[rustfmt::skip]
pub fn is_assignment_operator(token_type: &lexer::TokenType) -> bool {
    matches!(token_type,
        lexer::TokenType::Assignment              |
        lexer::TokenType::AdditionAssignment      |
        lexer::TokenType::SubtractionAssignment   |
        lexer::TokenType::MultiplyAssignment      |
        lexer::TokenType::DivideAssignment        |
        lexer::TokenType::RemainderAssignment     |
        lexer::TokenType::BitwiseAndAssignment    |
        lexer::TokenType::BitwiseOrAssignment     |
        lexer::TokenType::BitwiseXorAssignment    |
        lexer::TokenType::LeftShiftAssignment     |
        lexer::TokenType::RightShiftAssignment
    )
}

/// Parses a token and produces an AST binary operator.
#[rustfmt::skip]
pub fn parse_binary_operator(token_type: &lexer::TokenType) -> AstBinaryOp {
    match token_type {
        lexer::TokenType::Plus                   => AstBinaryOp::Add,
        lexer::TokenType::Minus                  => AstBinaryOp::Subtract,
        lexer::TokenType::Multiply               => AstBinaryOp::Multiply,
        lexer::TokenType::Divide                 => AstBinaryOp::Divide,
        lexer::TokenType::Remainder              => AstBinaryOp::Remainder,
        lexer::TokenType::BitwiseAnd             => AstBinaryOp::BitwiseAnd,
        lexer::TokenType::BitwiseXor             => AstBinaryOp::BitwiseXor,
        lexer::TokenType::BitwiseOr              => AstBinaryOp::BitwiseOr,
        lexer::TokenType::LeftShift              => AstBinaryOp::LeftShift,
        lexer::TokenType::RightShift             => AstBinaryOp::RightShift,
        lexer::TokenType::LogicalAnd             => AstBinaryOp::LogicalAnd,
        lexer::TokenType::LogicalOr              => AstBinaryOp::LogicalOr,
        lexer::TokenType::EqualTo                => AstBinaryOp::EqualTo,
        lexer::TokenType::NotEqualTo             => AstBinaryOp::NotEqualTo,
        lexer::TokenType::LessThan               => AstBinaryOp::LessThan,
        lexer::TokenType::GreaterThan            => AstBinaryOp::GreaterThan,
        lexer::TokenType::LessThanOrEqualTo      => AstBinaryOp::LessThanOrEqualTo,
        lexer::TokenType::GreaterThanOrEqualTo   => AstBinaryOp::GreaterThanOrEqualTo,
        _ => { ICE!("Token '{token_type}' is not a binary operator") },
    }
}

/// Parses a token and produces an AST assignment operator.
#[rustfmt::skip]
pub fn parse_assignment_operator(token_type: &lexer::TokenType) -> AstAssignmentOp {
    match token_type {
        lexer::TokenType::Assignment             => AstAssignmentOp::Assignment,
        lexer::TokenType::AdditionAssignment     => AstAssignmentOp::Addition,
        lexer::TokenType::SubtractionAssignment  => AstAssignmentOp::Subtraction,
        lexer::TokenType::MultiplyAssignment     => AstAssignmentOp::Multiply,
        lexer::TokenType::DivideAssignment       => AstAssignmentOp::Divide,
        lexer::TokenType::RemainderAssignment    => AstAssignmentOp::Remainder,
        lexer::TokenType::BitwiseAndAssignment   => AstAssignmentOp::BitwiseAnd,
        lexer::TokenType::BitwiseOrAssignment    => AstAssignmentOp::BitwiseOr,
        lexer::TokenType::BitwiseXorAssignment   => AstAssignmentOp::BitwiseXor,
        lexer::TokenType::LeftShiftAssignment    => AstAssignmentOp::LeftShift,
        lexer::TokenType::RightShiftAssignment   => AstAssignmentOp::RightShift,
        _ => { ICE!("Token '{token_type}' is not an assignment operator") },
    }
}

/// Returns the precedence integer value for the given token type.
#[rustfmt::skip]
pub fn operator_precedence(token_type: &lexer::TokenType) -> i32 {
    match token_type {
        lexer::TokenType::Multiply              => 100,
        lexer::TokenType::Divide                => 100,
        lexer::TokenType::Remainder             => 100,

        lexer::TokenType::Plus                  => 90,
        lexer::TokenType::Minus                 => 90,

        lexer::TokenType::LeftShift             => 80,
        lexer::TokenType::RightShift            => 80,

        lexer::TokenType::LessThan              => 70,
        lexer::TokenType::LessThanOrEqualTo     => 70,
        lexer::TokenType::GreaterThan           => 70,
        lexer::TokenType::GreaterThanOrEqualTo  => 70,

        lexer::TokenType::EqualTo               => 60,
        lexer::TokenType::NotEqualTo            => 60,

        lexer::TokenType::BitwiseAnd            => 49,
        lexer::TokenType::BitwiseXor            => 48,
        lexer::TokenType::BitwiseOr             => 47,
        lexer::TokenType::LogicalAnd            => 46,
        lexer::TokenType::LogicalOr             => 45,

        // We parse conditional/ternary expressions with a trick that lets
        // them appear to be binary operations.
        lexer::TokenType::Ternary               => 10,

        lexer::TokenType::Assignment            => 1,
        lexer::TokenType::AdditionAssignment    => 1,
        lexer::TokenType::SubtractionAssignment => 1,
        lexer::TokenType::MultiplyAssignment    => 1,
        lexer::TokenType::DivideAssignment      => 1,
        lexer::TokenType::RemainderAssignment   => 1,
        lexer::TokenType::BitwiseAndAssignment  => 1,
        lexer::TokenType::BitwiseOrAssignment   => 1,
        lexer::TokenType::BitwiseXorAssignment  => 1,
        lexer::TokenType::LeftShiftAssignment   => 1,
        lexer::TokenType::RightShiftAssignment  => 1,

        _ => { ICE!("No precedence for token {token_type}") }
    }
}
