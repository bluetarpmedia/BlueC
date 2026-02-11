// Copyright 2025-2026 Neil Henderson
//
//! The `bt_constant_value` module defines the [BtConstantValue] type.

use std::fmt;

use crate::ICE;
use crate::parser::{AstConstantFp, AstConstantInteger};

use super::BtType;

/// A constant arithmetic value.
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum BtConstantValue {
    Int8(i8),
    Int16(i16),
    Int32(i32),
    Int64(i64),
    UInt8(u8),
    UInt16(u16),
    UInt32(u32),
    UInt64(u64),
    Float32(f32),
    Float64(f64),
}

impl From<AstConstantInteger> for BtConstantValue {
    fn from(value: AstConstantInteger) -> Self {
        match value {
            AstConstantInteger::Char(value) => BtConstantValue::Int8(value),
            AstConstantInteger::Short(value) => BtConstantValue::Int16(value),
            AstConstantInteger::Int(value) => BtConstantValue::Int32(value),
            AstConstantInteger::LongLong(value) => BtConstantValue::Int64(value),
            AstConstantInteger::UnsignedChar(value) => BtConstantValue::UInt8(value),
            AstConstantInteger::UnsignedShort(value) => BtConstantValue::UInt16(value),
            AstConstantInteger::UnsignedInt(value) => BtConstantValue::UInt32(value),
            AstConstantInteger::UnsignedLongLong(value) => BtConstantValue::UInt64(value),
        }
    }
}

impl From<AstConstantFp> for BtConstantValue {
    fn from(value: AstConstantFp) -> Self {
        match value {
            AstConstantFp::Float(value) => BtConstantValue::Float32(value),
            AstConstantFp::Double(value) => BtConstantValue::Float64(value),
        }
    }
}

impl fmt::Display for BtConstantValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BtConstantValue::Int8(value) => write!(f, "{value}"),
            BtConstantValue::Int16(value) => write!(f, "{value}"),
            BtConstantValue::Int32(value) => write!(f, "{value}"),
            BtConstantValue::Int64(value) => write!(f, "{value}"),
            BtConstantValue::UInt8(value) => write!(f, "{value}"),
            BtConstantValue::UInt16(value) => write!(f, "{value}"),
            BtConstantValue::UInt32(value) => write!(f, "{value}"),
            BtConstantValue::UInt64(value) => write!(f, "{value}"),
            BtConstantValue::Float32(value) => write!(f, "{value}"),
            BtConstantValue::Float64(value) => write!(f, "{value}"),
        }
    }
}

impl BtConstantValue {
    /// Makes a `BtConstantValue` for the given type with a constant value of `1`.
    pub fn one(bt_type: BtType) -> Self {
        match bt_type {
            BtType::Void => ICE!("Cannot create a BtConstantValue(1) for BtType::Void"),
            BtType::Int8 => BtConstantValue::Int8(1),
            BtType::Int16 => BtConstantValue::Int16(1),
            BtType::Int32 => BtConstantValue::Int32(1),
            BtType::Int64 => BtConstantValue::Int64(1),
            BtType::UInt8 => BtConstantValue::UInt8(1),
            BtType::UInt16 => BtConstantValue::UInt16(1),
            BtType::UInt32 => BtConstantValue::UInt32(1),
            BtType::UInt64 => BtConstantValue::UInt64(1),
            BtType::Float32 => BtConstantValue::Float32(1.0),
            BtType::Float64 => BtConstantValue::Float64(1.0),
            BtType::Pointer => ICE!("Cannot create a value of 1 for a pointer type"),
            BtType::Array { .. } => ICE!("Cannot create a value of 1 for an array type"),
            BtType::Function => ICE!("Cannot create a value of 1 for a function type"),
        }
    }

    /// Is the constant value equal to zero?
    pub fn is_zero(&self) -> bool {
        match self {
            BtConstantValue::Int8(value) => *value == 0,
            BtConstantValue::Int16(value) => *value == 0,
            BtConstantValue::Int32(value) => *value == 0,
            BtConstantValue::Int64(value) => *value == 0,
            BtConstantValue::UInt8(value) => *value == 0,
            BtConstantValue::UInt16(value) => *value == 0,
            BtConstantValue::UInt32(value) => *value == 0,
            BtConstantValue::UInt64(value) => *value == 0,
            BtConstantValue::Float32(value) => *value == 0.0,
            BtConstantValue::Float64(value) => *value == 0.0,
        }
    }

    /// Gets the IR type of the constant value.
    pub fn get_bt_type(&self) -> BtType {
        match self {
            BtConstantValue::Int8(_) => BtType::Int8,
            BtConstantValue::Int16(_) => BtType::Int16,
            BtConstantValue::Int32(_) => BtType::Int32,
            BtConstantValue::Int64(_) => BtType::Int64,
            BtConstantValue::UInt8(_) => BtType::UInt8,
            BtConstantValue::UInt16(_) => BtType::UInt16,
            BtConstantValue::UInt32(_) => BtType::UInt32,
            BtConstantValue::UInt64(_) => BtType::UInt64,
            BtConstantValue::Float32(_) => BtType::Float32,
            BtConstantValue::Float64(_) => BtType::Float64,
        }
    }
}
