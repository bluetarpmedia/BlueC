// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `ast_literals` module defines types related to C literals (e.g. integer literals).

use std::fmt;

use crate::ICE;
use super::AstType;

/// The kind of integer literal.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum AstIntegerLiteralKind {
    Int,
    Long,
    LongLong,
    UnsignedInt,
    UnsignedLong,
    UnsignedLongLong,
}

impl fmt::Display for AstIntegerLiteralKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AstIntegerLiteralKind::Int => write!(f, "Int"),
            AstIntegerLiteralKind::Long => write!(f, "Long"),
            AstIntegerLiteralKind::LongLong => write!(f, "LongLong"),
            AstIntegerLiteralKind::UnsignedInt => write!(f, "UnsignedInt"),
            AstIntegerLiteralKind::UnsignedLong => write!(f, "UnsignedLong"),
            AstIntegerLiteralKind::UnsignedLongLong => write!(f, "UnsignedLongLong"),
        }
    }
}

impl From<&AstType> for AstIntegerLiteralKind {
    fn from(data_type: &AstType) -> Self {
        match data_type {
            AstType::Int => AstIntegerLiteralKind::Int,
            AstType::Long => AstIntegerLiteralKind::Long,
            AstType::LongLong => AstIntegerLiteralKind::LongLong,
            AstType::UnsignedInt => AstIntegerLiteralKind::UnsignedInt,
            AstType::UnsignedLong => AstIntegerLiteralKind::UnsignedLong,
            AstType::UnsignedLongLong => AstIntegerLiteralKind::UnsignedLongLong,
            _ => ICE!("Cannot convert '{data_type}' to AstIntegerLiteralKind"),
        }
    }
}

impl AstIntegerLiteralKind {
    /// The `AstType` for the kind of integer literal.
    pub fn data_type(&self) -> AstType {
        match self {
            AstIntegerLiteralKind::Int => AstType::Int,
            AstIntegerLiteralKind::Long => AstType::Long,
            AstIntegerLiteralKind::LongLong => AstType::LongLong,
            AstIntegerLiteralKind::UnsignedInt => AstType::UnsignedInt,
            AstIntegerLiteralKind::UnsignedLong => AstType::UnsignedLong,
            AstIntegerLiteralKind::UnsignedLongLong => AstType::UnsignedLongLong,
        }
    }
}

/// The kind of floating-point literal.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum AstFloatLiteralKind {
    Float,
    Double,
    LongDouble,
}

impl fmt::Display for AstFloatLiteralKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AstFloatLiteralKind::Float => write!(f, "Float"),
            AstFloatLiteralKind::Double => write!(f, "Double"),
            AstFloatLiteralKind::LongDouble => write!(f, "LongDouble"),
        }
    }
}

impl From<&AstType> for AstFloatLiteralKind {
    fn from(data_type: &AstType) -> Self {
        match data_type {
            AstType::Float => AstFloatLiteralKind::Float,
            AstType::Double => AstFloatLiteralKind::Double,
            AstType::LongDouble => AstFloatLiteralKind::LongDouble,
            _ => ICE!("Cannot convert '{data_type}' to AstFloatLiteralKind"),
        }
    }
}

impl AstFloatLiteralKind {
    /// The `AstType` for the kind of floating-point literal.
    pub fn data_type(&self) -> AstType {
        match self {
            AstFloatLiteralKind::Float => AstType::Float,
            AstFloatLiteralKind::Double => AstType::Double,
            AstFloatLiteralKind::LongDouble => AstType::LongDouble,
        }
    }
}
