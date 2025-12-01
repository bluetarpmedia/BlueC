// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The binary_ops module defines some helper functions for parsing binary operations.

use crate::internal_error;
use crate::lexer;
use crate::parser::AstBinaryOp;

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
        lexer::TokenType::Assignment              |
        lexer::TokenType::Ternary                 |
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
        lexer::TokenType::AdditionAssignment     => AstBinaryOp::AdditionAssignment,
        lexer::TokenType::SubtractionAssignment  => AstBinaryOp::SubtractionAssignment,
        lexer::TokenType::MultiplyAssignment     => AstBinaryOp::MultiplyAssignment,
        lexer::TokenType::DivideAssignment       => AstBinaryOp::DivideAssignment,
        lexer::TokenType::RemainderAssignment    => AstBinaryOp::RemainderAssignment,
        lexer::TokenType::BitwiseAndAssignment   => AstBinaryOp::BitwiseAndAssignment,
        lexer::TokenType::BitwiseOrAssignment    => AstBinaryOp::BitwiseOrAssignment,
        lexer::TokenType::BitwiseXorAssignment   => AstBinaryOp::BitwiseXorAssignment,
        lexer::TokenType::LeftShiftAssignment    => AstBinaryOp::LeftShiftAssignment,
        lexer::TokenType::RightShiftAssignment   => AstBinaryOp::RightShiftAssignment,
        _ => { internal_error::ICE("Parser: Token is not a binary operator") },
    }
}

/// Returns the precedence integer value for the given binary operator.
#[rustfmt::skip]
pub fn binary_operator_precedence(token_type: &lexer::TokenType) -> i32 {
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

        _ => { internal_error::ICE("Parser: No precedence for unexpected token") }
    }
}
