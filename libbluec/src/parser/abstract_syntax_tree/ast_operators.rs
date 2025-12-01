// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `ast_operators` module defines the `AstUnaryOp` and `AstBinaryOp` types.

use std::fmt;

use crate::lexer;

/// Unary operators.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum AstUnaryOp {
    Negate,
    Plus,
    BitwiseNot, // Complement
    LogicalNot,
    PrefixIncrement,
    PrefixDecrement,
    PostfixIncrement,
    PostfixDecrement,
    Deref,
    AddressOf,
}

/// Binary operators.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum AstBinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    BitwiseAnd,
    BitwiseXor,
    BitwiseOr,
    LeftShift,
    RightShift,
    LogicalAnd,
    LogicalOr,
    EqualTo,
    NotEqualTo,
    LessThan,
    GreaterThan,
    LessThanOrEqualTo,
    GreaterThanOrEqualTo,
    AdditionAssignment,
    SubtractionAssignment,
    MultiplyAssignment,
    DivideAssignment,
    RemainderAssignment,
    BitwiseAndAssignment,
    BitwiseOrAssignment,
    BitwiseXorAssignment,
    LeftShiftAssignment,
    RightShiftAssignment,
}

/// Families or groups of operators.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum AstBinaryOpFamily {
    Arithmetic,
    Bitwise,
    Logical,
    Relational,
    CompoundArithmeticAssignment,
    CompoundBitwiseAssignment,
}

impl fmt::Display for AstUnaryOp {
    #[rustfmt::skip]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AstUnaryOp::Negate           => write!(f, "Negate"),
            AstUnaryOp::Plus             => write!(f, "Plus"),
            AstUnaryOp::BitwiseNot       => write!(f, "BitwiseNot"),
            AstUnaryOp::LogicalNot       => write!(f, "LogicalNot"),
            AstUnaryOp::PrefixIncrement  => write!(f, "PrefixIncrement"),
            AstUnaryOp::PrefixDecrement  => write!(f, "PrefixDecrement"),
            AstUnaryOp::PostfixIncrement => write!(f, "PostfixIncrement"),
            AstUnaryOp::PostfixDecrement => write!(f, "PostfixDecrement"),
            AstUnaryOp::Deref            => write!(f, "Deref"),
            AstUnaryOp::AddressOf        => write!(f, "AddressOf"),
        }
    }
}

impl fmt::Display for AstBinaryOp {
    #[rustfmt::skip]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AstBinaryOp::Add                    => write!(f, "Add"),
            AstBinaryOp::Subtract               => write!(f, "Subtract"),
            AstBinaryOp::Multiply               => write!(f, "Multiply"),
            AstBinaryOp::Divide                 => write!(f, "Divide"),
            AstBinaryOp::Remainder              => write!(f, "Remainder"),
            AstBinaryOp::BitwiseAnd             => write!(f, "BitwiseAnd"),
            AstBinaryOp::BitwiseXor             => write!(f, "BitwiseXor"),
            AstBinaryOp::BitwiseOr              => write!(f, "BitwiseOr"),
            AstBinaryOp::LeftShift              => write!(f, "LeftShift"),
            AstBinaryOp::RightShift             => write!(f, "RightShift"),
            AstBinaryOp::LogicalAnd             => write!(f, "LogicalAnd"),
            AstBinaryOp::LogicalOr              => write!(f, "LogicalOr"),
            AstBinaryOp::EqualTo                => write!(f, "EqualTo"),
            AstBinaryOp::NotEqualTo             => write!(f, "NotEqualTo"),
            AstBinaryOp::LessThan               => write!(f, "LessThan"),
            AstBinaryOp::GreaterThan            => write!(f, "GreaterThan"),
            AstBinaryOp::LessThanOrEqualTo      => write!(f, "LessThanOrEqualTo"),
            AstBinaryOp::GreaterThanOrEqualTo   => write!(f, "GreaterThanOrEqualTo"),
            AstBinaryOp::AdditionAssignment     => write!(f, "AdditionAssignment"),
            AstBinaryOp::SubtractionAssignment  => write!(f, "SubtractionAssignment"),
            AstBinaryOp::MultiplyAssignment     => write!(f, "MultiplyAssignment"),
            AstBinaryOp::DivideAssignment       => write!(f, "DivideAssignment"),
            AstBinaryOp::RemainderAssignment    => write!(f, "RemainderAssignment"),
            AstBinaryOp::BitwiseAndAssignment   => write!(f, "BitwiseAndAssignment"),
            AstBinaryOp::BitwiseOrAssignment    => write!(f, "BitwiseOrAssignment"),
            AstBinaryOp::BitwiseXorAssignment   => write!(f, "BitwiseXorAssignment"),
            AstBinaryOp::LeftShiftAssignment    => write!(f, "LeftShiftAssignment"),
            AstBinaryOp::RightShiftAssignment   => write!(f, "RightShiftAssignment"),

            #[allow(unreachable_patterns)]
            _ => write!(f, "(Unknown)"),
        }
    }
}

impl From<AstBinaryOp> for lexer::TokenType {
    fn from(op: AstBinaryOp) -> Self {
        match op {
            AstBinaryOp::Add => lexer::TokenType::Plus,
            AstBinaryOp::Subtract => lexer::TokenType::Minus,
            AstBinaryOp::Multiply => lexer::TokenType::Multiply,
            AstBinaryOp::Divide => lexer::TokenType::Divide,
            AstBinaryOp::Remainder => lexer::TokenType::Remainder,
            AstBinaryOp::BitwiseAnd => lexer::TokenType::BitwiseAnd,
            AstBinaryOp::BitwiseXor => lexer::TokenType::BitwiseXor,
            AstBinaryOp::BitwiseOr => lexer::TokenType::BitwiseOr,
            AstBinaryOp::LeftShift => lexer::TokenType::LeftShift,
            AstBinaryOp::RightShift => lexer::TokenType::RightShift,
            AstBinaryOp::LogicalAnd => lexer::TokenType::LogicalAnd,
            AstBinaryOp::LogicalOr => lexer::TokenType::LogicalOr,
            AstBinaryOp::EqualTo => lexer::TokenType::EqualTo,
            AstBinaryOp::NotEqualTo => lexer::TokenType::NotEqualTo,
            AstBinaryOp::LessThan => lexer::TokenType::LessThan,
            AstBinaryOp::GreaterThan => lexer::TokenType::GreaterThan,
            AstBinaryOp::LessThanOrEqualTo => lexer::TokenType::LessThanOrEqualTo,
            AstBinaryOp::GreaterThanOrEqualTo => lexer::TokenType::GreaterThanOrEqualTo,
            AstBinaryOp::AdditionAssignment => lexer::TokenType::AdditionAssignment,
            AstBinaryOp::SubtractionAssignment => lexer::TokenType::SubtractionAssignment,
            AstBinaryOp::MultiplyAssignment => lexer::TokenType::MultiplyAssignment,
            AstBinaryOp::DivideAssignment => lexer::TokenType::DivideAssignment,
            AstBinaryOp::RemainderAssignment => lexer::TokenType::RemainderAssignment,
            AstBinaryOp::BitwiseAndAssignment => lexer::TokenType::BitwiseAndAssignment,
            AstBinaryOp::BitwiseOrAssignment => lexer::TokenType::BitwiseOrAssignment,
            AstBinaryOp::BitwiseXorAssignment => lexer::TokenType::BitwiseXorAssignment,
            AstBinaryOp::LeftShiftAssignment => lexer::TokenType::LeftShiftAssignment,
            AstBinaryOp::RightShiftAssignment => lexer::TokenType::RightShiftAssignment,
        }
    }
}

impl AstBinaryOp {
    /// Is the binary operator a compound assignment operator?
    pub fn is_compound_assignment(&self) -> bool {
        matches!(
            self,
            AstBinaryOp::AdditionAssignment
                | AstBinaryOp::SubtractionAssignment
                | AstBinaryOp::MultiplyAssignment
                | AstBinaryOp::DivideAssignment
                | AstBinaryOp::RemainderAssignment
                | AstBinaryOp::BitwiseAndAssignment
                | AstBinaryOp::BitwiseOrAssignment
                | AstBinaryOp::BitwiseXorAssignment
                | AstBinaryOp::LeftShiftAssignment
                | AstBinaryOp::RightShiftAssignment
        )
    }

    /// Is the binary operator a relational operator?
    pub fn is_relational(&self) -> bool {
        matches!(
            self,
            AstBinaryOp::EqualTo
                | AstBinaryOp::NotEqualTo
                | AstBinaryOp::LessThan
                | AstBinaryOp::GreaterThan
                | AstBinaryOp::LessThanOrEqualTo
                | AstBinaryOp::GreaterThanOrEqualTo
        )
    }

    /// The family or group that the operator belongs to.
    pub fn family(&self) -> AstBinaryOpFamily {
        match self {
            AstBinaryOp::Add
            | AstBinaryOp::Subtract
            | AstBinaryOp::Multiply
            | AstBinaryOp::Divide
            | AstBinaryOp::Remainder => AstBinaryOpFamily::Arithmetic,
            AstBinaryOp::BitwiseAnd
            | AstBinaryOp::BitwiseXor
            | AstBinaryOp::BitwiseOr
            | AstBinaryOp::LeftShift
            | AstBinaryOp::RightShift => AstBinaryOpFamily::Bitwise,
            AstBinaryOp::LogicalAnd | AstBinaryOp::LogicalOr => AstBinaryOpFamily::Logical,
            AstBinaryOp::EqualTo
            | AstBinaryOp::NotEqualTo
            | AstBinaryOp::LessThan
            | AstBinaryOp::GreaterThan
            | AstBinaryOp::LessThanOrEqualTo
            | AstBinaryOp::GreaterThanOrEqualTo => AstBinaryOpFamily::Relational,
            AstBinaryOp::AdditionAssignment
            | AstBinaryOp::SubtractionAssignment
            | AstBinaryOp::MultiplyAssignment
            | AstBinaryOp::DivideAssignment
            | AstBinaryOp::RemainderAssignment => AstBinaryOpFamily::CompoundArithmeticAssignment,
            AstBinaryOp::BitwiseAndAssignment
            | AstBinaryOp::BitwiseOrAssignment
            | AstBinaryOp::BitwiseXorAssignment
            | AstBinaryOp::LeftShiftAssignment
            | AstBinaryOp::RightShiftAssignment => AstBinaryOpFamily::CompoundBitwiseAssignment,
        }
    }
}
