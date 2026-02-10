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
}

/// Assignment operators.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum AstAssignmentOp {
    Assignment,
    Addition,
    Subtraction,
    Multiply,
    Divide,
    Remainder,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    LeftShift,
    RightShift,
}

/// Families or groups of binary operators.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum AstBinaryOpFamily {
    Arithmetic,
    Bitwise,
    Logical,
    Relational,
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
        }
    }
}

impl fmt::Display for AstAssignmentOp {
    #[rustfmt::skip]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AstAssignmentOp::Assignment         => write!(f, "Assignment"),
            AstAssignmentOp::Addition           => write!(f, "AdditionAssignment"),
            AstAssignmentOp::Subtraction        => write!(f, "SubtractionAssignment"),
            AstAssignmentOp::Multiply           => write!(f, "MultiplyAssignment"),
            AstAssignmentOp::Divide             => write!(f, "DivideAssignment"),
            AstAssignmentOp::Remainder          => write!(f, "RemainderAssignment"),
            AstAssignmentOp::BitwiseAnd         => write!(f, "BitwiseAndAssignment"),
            AstAssignmentOp::BitwiseOr          => write!(f, "BitwiseOrAssignment"),
            AstAssignmentOp::BitwiseXor         => write!(f, "BitwiseXorAssignment"),
            AstAssignmentOp::LeftShift          => write!(f, "LeftShiftAssignment"),
            AstAssignmentOp::RightShift         => write!(f, "RightShiftAssignment"),
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
        }
    }
}

impl From<AstAssignmentOp> for lexer::TokenType {
    fn from(op: AstAssignmentOp) -> Self {
        match op {
            AstAssignmentOp::Assignment => lexer::TokenType::Assignment,
            AstAssignmentOp::Addition => lexer::TokenType::AdditionAssignment,
            AstAssignmentOp::Subtraction => lexer::TokenType::SubtractionAssignment,
            AstAssignmentOp::Multiply => lexer::TokenType::MultiplyAssignment,
            AstAssignmentOp::Divide => lexer::TokenType::DivideAssignment,
            AstAssignmentOp::Remainder => lexer::TokenType::RemainderAssignment,
            AstAssignmentOp::BitwiseAnd => lexer::TokenType::BitwiseAndAssignment,
            AstAssignmentOp::BitwiseOr => lexer::TokenType::BitwiseOrAssignment,
            AstAssignmentOp::BitwiseXor => lexer::TokenType::BitwiseXorAssignment,
            AstAssignmentOp::LeftShift => lexer::TokenType::LeftShiftAssignment,
            AstAssignmentOp::RightShift => lexer::TokenType::RightShiftAssignment,
        }
    }
}

impl AstUnaryOp {
    /// Does the operator have side effects?
    pub fn has_side_effects(&self) -> bool {
        matches!(
            self,
            AstUnaryOp::PrefixDecrement
                | AstUnaryOp::PrefixIncrement
                | AstUnaryOp::PostfixDecrement
                | AstUnaryOp::PostfixIncrement
        )
    }
}

impl AstBinaryOp {
    /// Is the operator a relational operator?
    pub fn is_relational(&self) -> bool {
        self.family() == AstBinaryOpFamily::Relational
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
        }
    }

    /// Is the operator both commutative and associative (Add or Multiply)?
    pub fn is_commutative_and_associative(&self) -> bool {
        matches!(self, AstBinaryOp::Add | AstBinaryOp::Multiply)
    }

    /// Is the operator either Division or Remainder?
    pub fn is_div_or_rem(&self) -> bool {
        matches!(self, AstBinaryOp::Divide | AstBinaryOp::Remainder)
    }

    /// Is the operator a Left/Right shift operator?
    pub fn is_shift(&self) -> bool {
        matches!(self, AstBinaryOp::LeftShift | AstBinaryOp::RightShift)
    }
}

#[rustfmt::skip]
impl TryFrom<AstAssignmentOp> for AstBinaryOp {
    type Error = ();
    fn try_from(value: AstAssignmentOp) -> Result<Self, Self::Error> {
        match value {
            AstAssignmentOp::Addition    => Ok(AstBinaryOp::Add),
            AstAssignmentOp::Subtraction => Ok(AstBinaryOp::Subtract),
            AstAssignmentOp::Multiply    => Ok(AstBinaryOp::Multiply),
            AstAssignmentOp::Divide      => Ok(AstBinaryOp::Divide),
            AstAssignmentOp::Remainder   => Ok(AstBinaryOp::Remainder),
            AstAssignmentOp::BitwiseAnd  => Ok(AstBinaryOp::BitwiseAnd),
            AstAssignmentOp::BitwiseOr   => Ok(AstBinaryOp::BitwiseOr),
            AstAssignmentOp::BitwiseXor  => Ok(AstBinaryOp::BitwiseXor),
            AstAssignmentOp::LeftShift   => Ok(AstBinaryOp::LeftShift),
            AstAssignmentOp::RightShift  => Ok(AstBinaryOp::RightShift),
            _ => Err(()),
        }
    }
}

impl AstAssignmentOp {
    /// Is the operator a compound assignment operator?
    pub fn is_compound_assignment(&self) -> bool {
        !matches!(self, AstAssignmentOp::Assignment)
    }

    /// Is the operator either the compound division or compound remainder operator?
    pub fn is_div_or_rem(&self) -> bool {
        matches!(self, AstAssignmentOp::Divide | AstAssignmentOp::Remainder)
    }

    /// Is the operator either the compound left/right shift operator?
    pub fn is_shift(&self) -> bool {
        matches!(self, AstAssignmentOp::LeftShift | AstAssignmentOp::RightShift)
    }
}
